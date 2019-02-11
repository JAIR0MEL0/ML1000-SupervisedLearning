###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)
#library(plotly)
install.packages("randomForest")
library(randomForest)

#The Data originally comes from Kaggle site, and the dataset is sourced from the kickstarter Project
#https://www.kaggle.com/kemical/kickstarter-projects/home

getwd();
data=read.csv("C:\\Users\\khade\\MachineLearning\\Assign1\\kickstarter_dataset\\ks-projects-201801.csv", header = TRUE, dec = ".")

#check # of rows
ncol(data);
nrow(data);

#Delete missing values
data = data[complete.cases(data),]

#Make sure missing data points are absent (better be zero)
#sum(!complete.cases(data))

#Retain only two levels of $state
data <- data[ which(data$state=='successful' | data$state =='failed'), ]
#set levels=2
data$state <- factor(data$state)

#Use launched date and deadline date to create another variable campaign.
data[["campaign"]] <- difftime(data$deadline, data$launched , units = c("days"))

#eliminate features that are duplicates
data <- data[c("main_category","state","backers","country","usd_goal_real","campaign","currency")]

#Data cleaning ends here

#quick preview data structure with HEAD(), STR(), and SUMMARY()
head(data,10)
str(data)
summary(data)

#change 5000 to a conveniently high number when running finally.
smallDF <- data[1:5000,]

#Feature selection
rfImp <- randomForest(state ~ ., data = smallDF, ntree = 200, importance = TRUE)
importance(rfImp,type=2)

#Retained all since all have nonzero MeanDecreaseGini

# split data into training and testing chunks
set.seed(1234)

# generalize outcome and predictor variables
outcomeName <- 'state'

#This is to use the same data set for Training and Test
splitIndex <- createDataPartition(smallDF[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- smallDF[ splitIndex,]
testDF  <- smallDF[-splitIndex,]

# pick model gbm and find out what type of model it is (WHAT DOES THIS LINE DO ?)
#getModelInfo()$gbm$type

#Let's evaluate some alhorithms
trctl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
metric <- "Accuracy"

# a) Linear Discriminat Analysis
set.seed(7)
fit.lda <- train(state ~ main_category + usd_goal_real + backers + campaign, data=trainDF, method="lda", metric=metric, trControl=trctl)
# b) nonlinear algorithms
# Classification Tree / Recursive Partitioning
set.seed(7)
fit.cart <- train(state ~ main_category + usd_goal_real + backers + campaign, data=trainDF, method="rpart", metric=metric, trControl=trctl)
#random forest
set.seed(7)
fit.rf <- train(state ~ main_category + usd_goal_real + backers + campaign, data=trainDF, method="rf", metric=metric, trControl=trctl)
#################################################
# evalutate model
#################################################
#Now let's select the best model
#summarize accuracy of models (Training)

results <- resamples(list(lda=fit.lda, cart=fit.cart, rf=fit.rf))
summary(results)

#Visualize the accuracy of the models
dotplot(results)

#Let's make a prediction (accuracy of testing dataset)

#LDA
predictions <- predict(fit.lda, testDF)
#head(predictions,5)
confusionMatrix(predictions,testDF$state)

#cart
predictions <- predict(fit.cart, testDF)
head(predictions)
confusionMatrix(predictions,testDF$state)

#RF
predictions <- predict(fit.rf, testDF)
head(predictions)
confusionMatrix(predictions,testDF$state)


#Deployment
#Predicting outcome of an unseen data point
xnew = data[211701,c("main_category","backers","country","usd_goal_real","campaign","currency")]
predict(fit.rf,xnew)
