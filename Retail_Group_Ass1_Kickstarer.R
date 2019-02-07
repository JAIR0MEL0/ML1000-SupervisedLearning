###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)
library(plotly)

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

#quick preview data structure with HEAD(), STR(), and SUMMARY()
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

#We will select only the fields that will help us to determine the probability of success

data[["campaign"]] <- difftime(data$deadline ,data$launched , units = c("days"))

data <- data[c('main_category','goal','backers','state','country','campaign','category','currency','deadline','name','usd_goal_real','usd.pledged','usd_pledged_real')]

summary(data[, c('main_category','goal','backers', 'successful', 'campaign')])


#JM: I added the PIE by State to learn more about the relevant States
########################
#Understanding the Data
########################

levels(data$state)

#Let's review what are the most significant States
pieState <- table(data$state)
pct <- round(pieState/sum(pieState)*100)
lbls <- paste(names(pieState), "\n", pieState, sep="")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(pieState,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart by State")

# Summary of success in a table
percentage <- prop.table(table(data$state)) * 100
cbind(freq=table(data$state), percentage)

#JM: Let's now analyze the Backers per success project
#Filtering only successful projects
only_successful_data <- data[data['state'] == 'successful',]

plot_backers_by_main_category <- only_successful_data %>%
  plot_ly(
    x = ~main_category,
    y = ~backers,
    split = ~main_category,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Main Category"
    ),
    yaxis = list(
      title = "Total Backers by Category",
      zeroline = F
    )
  )

plot_backers_by_main_category

#Let's review if the campaing of the project affects the success rate

#Kickstarter allow only 60 days. They recommends that projects be set to 30 days or less. 
#The idea is that if you project has not been funded within 30 days, it's not likely to be funded 
#by their deadline either.

length.pct <- data %>%
  filter(state %in% c("successful", "failed"), campaign <= 61) %>%
  group_by(campaign, state) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count))

ggplot(length.pct[length.pct$state=="successful",], aes(campaign, pct)) + 
  geom_point(colour="royalblue4", size=2.5) + ggtitle("Success vs. Campaing") + 
  xlab("Project Campaign (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) + geom_vline(xintercept=30, colour="red") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))


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
  YES_vector[i] <- (as.numeric(summary(subset(data, data$country == country_letters)$state)[1]))
  NO_vector[i] <- (as.numeric(summary(subset(data, data$country == country_letters)$state)[2]))
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

#Leaving only Successful and Failed projects

data <- data[ which(data$state=='successful' 
                         | data$state =='failed'), ]


data$state <- factor(data$state, labels = c('successful', 'failed'))

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
outcomeName <- 'state'
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
fit.lda <- train(state ~ main_category + goal + backers + campaign, data=trainDF, method="lda", metric=metric, trControl=trctl)
# b) nonlinear algorithms
# Classification Tree / Recursive Partitioning
set.seed(7)
fit.cart <- train(state ~ main_category + goal + backers + campaign, data=trainDF, method="rpart", metric=metric, trControl=trctl)
# k Nearest Neighbor
set.seed(7)
fit.knn <- train(state ~ main_category + goal + backers + campaign, data=trainDF, method="knn", metric=metric, trControl=trctl)
# c) advanced algorithms
# SVM Support Vector Machine
set.seed(7)
fit.svm <- train(state~., data=trainDF, method="svmRadial", metric=metric, trControl=trctl)

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
confusionMatrix(predictions,testDF$state)

#Deployment
#o	How useful is your model for interested parties (i.e., the companies or organizations that might want to use it)?
#  o	How would you deploy your model for interested parties?
#  o	What other data should be collected?
#  o	How often would the model need to be updated, etc.?
#  o	Build a simple shiny app with a particular user in mind

