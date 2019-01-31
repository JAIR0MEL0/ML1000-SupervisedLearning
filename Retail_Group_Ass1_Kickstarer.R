###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
#import packages;
library(caret)
library(pROC)

#################################################################################
##########################    IMPORING DATASET     ##############################
#################################################################################
getwd();
data=read.csv("~/desktop/ML/YORK/Assigment1/kickstarter-projects/ks-projects-201801.csv", header = TRUE, dec = ".")

#check # of rows
ncol(data);
nrow(data);

#quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(data,10)
str(data)
summary(data)


#Add a new column to make a clear Binary classification
data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')

#Create a factor for the new column
data$successful <- factor(data$successful, labels = c('NO', 'YES'))

#We select only the fields with more relevant significant . We'll use USD Pledged as it's a normalized currency for all rojects
data <- data[c('name','category','main_category','currency','deadline','goal','state','successful','usd.pledged')]

# what is the proportion of our outcome variable?
prop.table(table(data$successful))


# save the outcome for the glmnet model
tempOutcome <- data$successful

# generalize outcome and predictor variables
outcomeName <- 'successful'
predictorsNames <- names(data)[names(data) != outcomeName]


#################################################
# model it
#################################################

# pick model gbm and find out what type of model it is
getModelInfo()$gbm$type

# split data into training and testing chunks
set.seed(1234)

#This is to use the same data set for Training and Test; 2018 has sufficient records; but we can use different year for Training and Testing
splitIndex <- createDataPartition(data[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]

objControl <- trainControl(method = 'cv', number=3, returnResamp='none')

# run model

#Still working on the model... the predictors and the Successful factors.  It's showing an error.  Need to sitdown and look it deeper
objModel <- train(trainDF[,predictorsNames], as.factor(trainDF[,outcomeName]), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c('knnImpute', 'pca'))

# find out variable importance
summary(objModel)

# find out model details
objModel


#################################################
# evalutate model
#################################################
# get predictions on our testing data

# class prediction
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
head(predictions)
postResample(pred=predictions, obs=as.factor(testDF[,outcomeName]))


# probabilities 
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
head(predictions)
postResample(pred=predictions[[2]], obs=ifelse(testDF[,outcomeName]=='YES',1,0))
auc <- roc(ifelse(testDF[,outcomeName]=="YES",1,0), predictions[[2]])
print(auc$auc)
