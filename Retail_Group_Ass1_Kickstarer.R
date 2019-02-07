###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)

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


#JM: I added the PIE by State to learn more about the relevant States
########################
#Understanding the Data
########################

#Percentage by State
mitabla <- table(data$state)
pct <- round(mitabla/sum(mitabla)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(mitabla,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

# what is the proportion of our outcome variable?
percentage <- prop.table(table(trainDF$state)) * 100
cbind(freq=table(data$state), percentage)

###############################################
# Exploration of relation between amount of
# projects per country and success ratio
###############################################

# plot country by number of occurrences
projects_per_country <- data.frame(
  country = levels(data$country), 
  n_projects = as.numeric(summary(data$country))
)

# plot amount of projects per
ggplot(projects_per_country, aes(x = country, y = n_projects)) + geom_bar(stat="identity", fill = "steelblue")

###############################################
# Most of projects are from US but how is the 
# ratio of success per country?
###############################################

# plot amount of projects per country
ggplot(bplot, aes(x = country, y = n_projects)) + geom_bar(stat="identity", fill = "steelblue")

# small table contains country and
# total amount of projects
projects_per_country <- data.frame(
  country = levels(data$country), 
  total = as.numeric(summary(data$country))
)

# initialize empty vectors
YES_vector <- c()
NO_vector <- c()

# iterate over country levels
for(i in 1:length(levels(data$country))) {
  country_letters <- levels(data$country)[i]
    
  # fill the empty YES and NO vectors with successful and failed projects per country
  YES_vector[i] <- (as.numeric(summary(subset(data, data$country == country_letters)$successful)[1]))
  NO_vector[i] <- (as.numeric(summary(subset(data, data$country == country_letters)$successful)[2]))
}

# add the new columns our small table
projects_per_country$YES <- YES_vector
projects_per_country$NO <- NO_vector

# calculate ratio of success per country
projects_per_country$success_ratio <- (projects_per_country$YES / projects_per_country$total) * 100

# plot ratio of success per country
ggplot(projects_per_country, aes(x = country, y = success_ratio)) + geom_bar(stat="identity", fill = "steelblue") + coord_cartesian(ylim = c(0, 100)) 

###############################################
###############################################

#################################################
# Data Preparation
#################################################

#o	What is the purpose of the data set we selected (i.e., why was this data collected in the first place?). 
#   How we would define and measure the outcomes from the dataset.
#   How would you measure the effectiveness of a good prediction algorithm or clustering algorithm?
#o	Describe the final dataset that is used for classification (include a description of any newly formed variables you created).

#Creation of the Binary result column
data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')

data$successful <- factor(data$successful, labels = c('YES', 'NO'))

#We will select only the fields that will help us to determine the probability of success
data <- data[c('main_category','goal','backers','state','successful','country','category','currency','deadline','name','usd_goal_real','usd.pledged','usd_pledged_real')]


summary(data[, c('main_category','goal','backers', 'successful')])





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

# split data into training and testing chunks
set.seed(1234)

# generalize outcome and predictor variables
outcomeName <- 'successful'
#predictorsNames <- names(data)[names(data) != outcomeName]

#This is to use the same data set for Training and Test
splitIndex <- createDataPartition(data[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]

# pick model gbm and find out what type of model it is
getModelInfo()$gbm$type

#Let's evaluate some alhorithms
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# a) Linear Discriminat Analysis
set.seed(7)
fit.lda <- train(successful ~ main_category + goal + backers, data=trainDF, method="lda", metric=metric, trControl=trctl)
# b) nonlinear algorithms
# Classification Tree / Recursive Partitioning
set.seed(7)
fit.cart <- train(successful ~ main_category + goal + backers, data=trainDF, method="rpart", metric=metric, trControl=trctl)
# k Nearest Neighbor
set.seed(7)
fit.knn <- train(successful ~ main_category + goal + backers, data=trainDF, method="knn", metric=metric, trControl=trctl)
# c) advanced algorithms
# SVM Support Vector Machine
set.seed(7)
fit.svm <- train(successful~., data=trainDF, method="svmRadial", metric=metric, trControl=trctl)

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
#Estimate skill of Random Forest on trainDF dataset
# Successful prediction
predictions <- predict(fit.rf, testDF)
head(predictions)
confusionMatrix(predictions,testDF$successful)

#Deployment
#o	How useful is your model for interested parties (i.e., the companies or organizations that might want to use it)?
#  o	How would you deploy your model for interested parties?
#  o	What other data should be collected?
#  o	How often would the model need to be updated, etc.?
#  o	Build a simple shiny app with a particular user in mind

