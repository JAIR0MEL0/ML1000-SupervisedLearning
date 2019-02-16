###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)
#library(plotly)
install.packages("randomForest")
library(randomForest)
library(dplyr)

#The Data originally comes from Kaggle site, and the dataset is sourced from the kickstarter Project
#https://www.kaggle.com/kemical/kickstarter-projects/home

getwd();
data=read.csv("~/desktop/ML/YORK/Assigment1/kickstarter-projects/ks-projects-201801.csv", header = TRUE, dec = ".")

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
data$country <- factor(data$country)

sum((data$backers>=20000) & (data$state == 'successful'))

#Use launched date and deadline date to create another variable campaign.
data[["campaign"]] <- as.integer(difftime(data$deadline, data$launched , units = c("days")))

data <- select(data,-name)
data <- select(data,-ID)
data <- select(data,-category)
data <- select(data,-deadline)
data <- select(data,-launched)
data <- select(data,-usd.pledged)
data <- select(data,-usd_pledged_real)
data <- select(data,-pledged)
data <- select(data,-goal)

str(data)

dataVariable <- data[10000,]


#----------------Data preparation/cleaning------------------

#################################################
# model it
#################################################


#This is to use the same data set for Training and Test
splitIndex <- createDataPartition(data[,'state'], p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]
summary(trainDF)
summary(testDF)

# a) Linear Discriminant Analysis
trctl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
metric <- "Accuracy"


set.seed(152)
fit.lda <- train(state ~ main_category + usd_goal_real + backers + campaign + country, data=trainDF, method="lda", metric=metric, trControl=trctl)
# b) Nonlinear algorithms Classification Tree / Recursive Partitioning
set.seed(152)
fit.cart <- train(state ~ main_category + usd_goal_real + backers + campaign + country, data=trainDF, method="rpart", metric=metric, trControl=trctl)
# c) Final algorithm Random Forest
set.seed(152)
fit.rf <- train(state ~ main_category + usd_goal_real + backers + campaign + country, data=trainDF, method="rf", metric=metric, trControl=trctl)


## Evalutate model
#################################################
#summarize accuracy of models (Training)
results <- resamples(list(lda=fit.lda, cart=fit.cart, rf=fit.rf))
summary(results)

## Visualize the accuracy of the models
dotplot(results)


##Let's make a prediction (accuracy of testing dataset)
#a) Linear Discriminant Analysis and Confusion Matrix
predictions <- predict(fit.lda, testDF)
head(predictions)
confusionMatrix(predictions,testDF$state)

#b) Classification Tree / Recursive Partitioning and Confusion Matrix
predictions <- predict(fit.cart, testDF)
head(predictions)
confusionMatrix(predictions,testDF$state)

#c) Final algorithm Random Forest and confusion matrix
predictions <- predict(fit.rf, testDF)
head(predictions)
confusionMatrix(predictions,testDF$state)


## Deployment
#Predicting outcome of an unseen data point
xnew = data[211701,c("main_category","backers","country","usd_goal_real","campaign","currency")]
predict(fit.rf,xnew)
#Evaluating Models
#Now it's time to evaluate the models:
results <- resamples(list(lda=fit.lda, cart=fit.cart, rf=fit.rf))
summary(results)

