###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################


library(lattice)
library(ggplot2)
library(caret)
library(ellipse)
#library(pROC)

#################################################################################
##########################    IMPORING DATASET     ##############################
#################################################################################

#Problem statement: Mitchel wants to be a new entrepreneur; recently graduated from York University 
#would like to have one of his ideas being sponsored by Kickstarter.  He wants to explore whether his 
#brand new is a good option to be sponsored. 

#################################################
#Data Source
#################################################

#The Data originally comes from Kaggle site, and the dataset is sourced from the kickstarter Project
#https://www.kaggle.com/kemical/kickstarter-projects/home


getwd();
data=read.csv("~/desktop/ML/YORK/Assigment1/kickstarter-projects/ks-projects-201801.csv", header = TRUE, dec = ".")

#check # of rows
ncol(data);
nrow(data);


#Develop a proposed set of drivers and relationships 
#to inputs, state the set of assumptions related to the problem, define key metrics of success 
#and describe how you have applied the ethical ML framework, identify and prioritize means of 
#data acquisition, you proceed with the following sections. 


#quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(data,10)
str(data)
summary(data)


#################################################
# Data Preparation
#################################################


#o	What is the purpose of the data set we selected (i.e., why was this data collected in the first place?). 
#   How we would define and measure the outcomes from the dataset.
#   How would you measure the effectiveness of a good prediction algorithm or clustering algorithm?
#o	Describe the final dataset that is used for classification (include a description of any newly formed variables you created).


#Creation of the Binary result column
data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')

data$successful <- factor(data$successful, labels = c('NO', 'YES'))

#We will select only the fields that will help us to determine the probability of success
data <- data[c('category','main_category','currency','backers','country','deadline','name','goal','usd_goal_real','successful','usd.pledged','usd_pledged_real')]

# Setting factors to each column
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
    boxplot(x[,i], main=names(trainDF)[i])
}


#barplot for class breakdown
plot(y)

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


#o	Describe the meaning and type of data (scale, values, etc.) for each attribute in the data file.
#o	Verify data quality: Are there missing values? Duplicate data? Outliers? Are those mistakes? How do you deal with these problems?
#  o	Give simple, appropriate statistics (range, mode, mean, median, variance, counts, etc.) for the most important attributes and describe what they mean or if you found something interesting. Note: You can also use data from other sources for comparison.
#o	Visualize the most important attributes appropriately (at least 5 attributes). Important: Provide an interpretation for each chart. Explain for each attribute why you chose the used visualization.
#o	Explore relationships between attributes: Look at the attributes via scatter plots, correlation, cross-tabulation, group-wise averages, etc. as appropriate.
#o	Identify and explain interesting relationships between features and the class you are trying to predict (or cluster).
#o	Are there other features that could be added to the data or created from existing features?  Which ones?
#  o	Perform supervised learning using several methods for different features.
#o	Use internal and external validation measures to describe and compare the models and the predictions (some visual methods would be good).
#o	Describe your results. What findings are the most interesting?
  


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


#Deployment
#o	How useful is your model for interested parties (i.e., the companies or organizations that might want to use it)?
#  o	How would you deploy your model for interested parties?
#  o	What other data should be collected?
#  o	How often would the model need to be updated, etc.?
#  o	Build a simple shiny app with a particular user in mind


