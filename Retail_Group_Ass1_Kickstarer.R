###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################

library(lattice)
library(ggplot2)
library(caret)

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

#################################################
# Data Preparation
#################################################


data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')

data$successful <- factor(data$successful, labels = c('NO', 'YES'))

data <- data[c('category','main_category','currency','backers','country','deadline','name','goal','usd_goal_real','successful','usd.pledged','usd_pledged_real')]

# dummy variables for factors/characters
data$category <- as.factor(data$category)
data$main_category <- as.factor(data$main_category)
data$currency <- as.factor(data$currency)
data$backers <- as.factor(data$backers)
data$country <- as.factor(data$country)

# what is the proportion of our outcome variable?
percentage <- prop.table(table(data$successful)) * 100
cbind(freq=table(data$successful), percentage)

# pick model gbm and find out what type of model it is
getModelInfo()$gbm$type

# split data into training and testing chunks
set.seed(1234)

#This is to use the same data set for Training and Test
splitIndex <- createDataPartition(data[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]

#Dimensions of the dataset
dim(trainDF)


#list types for each attribute
sapply(trainDF, class)

#There are only two level so it's a Binary classification problem.  
#If we had selected state, there are multiple levels so it would be a multinomial classifgication.
#I this case, we'll used the converted succesful as we only need to know if the project successed or not
levels(trainDF$successful)

#split input and output
x <- trainDF[,1:5]
y <- trainDF[,10]

#################################################
# Visualization
#################################################



#boxplot for each attribute on one image
par(mfrow=c(1,5))
  for(i in 1:5){
    boxplot(x[,i], main=names(trainDF)[i]) #getting an error in here... I think this is related to the vector x
}


#barplot for class breakdown
plot(y)

#I need to understand why the vector is not recognized.



featurePlot(x=x, y=y, plot = "ellipse")

#Multivariate plots
featurePlot(x=x, y=y, plot ="box")

#density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)



#Let's evaluate some alhorithms
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

#################################################
# model it
#################################################


#Now we are building some models
# a) linear algorithms
set.seed(7)
fit.lda <- train(successful~., data=trainDF, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(successful~., data=trainDF, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(successful~., data=trainDF, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(successful~., data=trainDF, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(successful~., data=trainDF, method="rf", metric=metric, trControl=control)

#################################################
# evalutate model
#################################################
#Now let's select the best model
#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#Visualize the accuracy of the models
dotplot(results)


#summarize best model
print(fit.lda)


#Let's make a prediction
#Estimate skill of LDA on trainDF dataset
predictions <- predict(fit.lda, trainDF)
confusionMatrix(predictions,trainDF$successful)
