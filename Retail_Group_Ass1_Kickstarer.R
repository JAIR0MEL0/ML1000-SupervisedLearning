###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)
library(ellipse)

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

# Setting factors to each column
data$backers <- as.factor(data$backers)
data$main_category <- factor(data$main_category, labels = "music")

data$category <- factor(data$category, labels = "music")
data$currency <- factor(data$currency, labels = "USD")
data$country <- factor(data$country, labels = "US")


# df$main_category <- factor(df$main, labels = "class")
#df$Survived <- factor(df$Survived, labels = c('No', 'Yes'))

#Creation of the Binary result column
data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')

data$successful <- factor(data$successful, labels = c('YES', 'NO'))

#We will select only the fields that will help us to determine the probability of success
data <- data[c('category','main_category','currency','backers','country','deadline','name','goal','usd_goal_real','successful','usd.pledged','usd_pledged_real')]



summary(data[, c('main_category','category','currency','country', 'successful')])

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

# what is the proportion of our outcome variable?
percentage <- prop.table(table(trainDF$successful)) * 100
cbind(freq=table(trainDF$successful), percentage)


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
#Start with Lineal regression model
trctl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)

model <- train(successful ~ category + main_category + country + currency, 
               data = trainDF,
               method = "glm",
               preProcess = c('knnImpute', 'pca'),
               na.action = na.pass,
               trControl = trctl)

model

# a) linear algorithms
set.seed(7)
fit.lda <- train(successful~., data=trainDF, method="lda", metric=metric, trControl=trctl)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(successful~., data=trainDF, method="rpart", metric=metric, trControl=trctl)
# kNN
set.seed(7)
fit.knn <- train(successful~., data=trainDF, method="knn", metric=metric, trControl=trctl)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(successful~., data=trainDF, method="svmRadial", metric=metric, trControl=trctl)
# Random Forest
set.seed(7)
fit.rf <- train(successful~., data=trainDF, method="rf", metric=metric, trControl=trctl)

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

