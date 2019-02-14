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

#quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(data,10)
str(data)
summary(data)

summary(data[, c('state')])


#################################################
# Data Preparation
#################################################

#o	What is the purpose of the data set we selected (i.e., why was this data collected in the first place?). 
#   How we would define and measure the outcomes from the dataset.
#   How would you measure the effectiveness of a good prediction algorithm or clustering algorithm?
#o	Describe the final dataset that is used for classification (include a description of any newly formed variables you created).


#Things to do:
#Some of the records are State = 'N/A'; we will need to input or remove those records
#outliers --> identify them and desire what to do

# Missing values
colSums(is.na(data))
colSums(data=="")

#Deleting any row with incomplete data
data = data[complete.cases(data),]
sum(!complete.cases(data))


#We will select only the fields that will help us to determine the probability of success

data[["campaign"]] <- as.integer(difftime(data$deadline ,data$launched , units = c("days")))

data <- data[c('main_category','goal','backers','state','country','campaign','category','currency','deadline','name','usd_goal_real','usd.pledged','usd_pledged_real')]


###############################
#Data Visualization
###############################

levels(data$state)

#Let's review what are the most significant States
pieState <- table(data$state)
pct <- round(pieState/sum(pieState)*100)
lbls <- paste(names(pieState), "\n", pieState, sep="")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(pieState,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart by State")

# what is the proportion of our outcome variable?
percentage <- prop.table(table(data$state)) * 100
cbind(freq=table(data$state), percentage)


#Let's analyze the relationship between backers and the successful projects per categories

only_successful_data <- data %>% filter(
  data$state == 'successful'
)


only_successful_data <- data[data['state'] == 'successful',]

summary(only_successful_data$state)

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
  geom_point(colour="royalblue4", size=2.5) + ggtitle("Success vs. Campaign") + 
  xlab("Project Campaign (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) + geom_vline(xintercept=30, colour="red") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))




#Leaving only Successful and Failed projects

data <- data[ which(data$state=='successful' 
                         | data$state =='failed'), ]


data$state <- factor(data$state, labels = c('successful', 'failed'))




ImpMeasure<-data.frame(varImp(modelFit)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
ImpMeasure[order(-ImpMeasure$Overall),][1:3,]




#Let's analyze the relationship between backers and the successful projects per categories.  As we think, there is a strong relationship between the number of backers and the success of projects.  We should consider Backers as a strong predictor for succesfull projects.


ggplot(data.frame(data), aes(country,fill=state)) + geom_bar( position="fill") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot() + geom_histogram(data=data, aes(x=backers,fill=state),position = "fill",breaks=seq(0,100,5))

# Now let;s look at the distributions of each predictor

# Backers
ggplot(data=data[c("backers")],aes(x=backers))+geom_histogram(aes(y=..count../sum(..count..)),breaks=seq(0,100,5),col="black",fill='blue') +
ggtitle('Normalized distribution of number of backers for all the projects')

# goal
ggplot(data=data[c("usd_goal_real")],aes(x=usd_goal_real))+geom_histogram(aes(y=..count../sum(..count..)),breaks=seq(0,20000,1000),col="black",fill='blue') +
ggtitle('Normalized distribution of the goal (USD) for all the projects')

# Campaign
ggplot(data=data[c("campaign")],aes(x=campaign))+geom_histogram(aes(y=..count../sum(..count..)),breaks=seq(0,100,5),col="black",fill='blue')+
ggtitle('Normalized distribution of number of campaign days for all the projects')

# category
ggplot(data.frame(data), aes(x=main_category)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='blue') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
ggtitle('Normalized distribution of number of projects for each category')

# country
ggplot(data.frame(data), aes(x=country)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='blue') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
ggtitle('Normalized distribution of number of projects for each country')

# Currency
ggplot(data.frame(data), aes(x=currency)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='blue') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
ggtitle('Normalized distribution of number of projects for each currency')

# But this does not  give an indication of predictive power of each feature. Look at the 
# Fraction of failed and successful projects in each bin of each predictor.
ggplot(data.frame(data), aes(main_category,fill=state)) + geom_bar( position="fill") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot() + geom_histogram(data=data, aes(x=usd_goal_real,fill=state),position = "fill",breaks=seq(0,20000,1000))
ggplot() + geom_histogram(data=data, aes(x=campaign,fill=state),position = "fill",breaks=seq(0,100,5))

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

# split data into training and testing chunks
set.seed(1234)

# generalize outcome and predictor variables
outcomeName <- 'state'

#This is to use the same data set for Training and Test
splitIndex <- createDataPartition(data[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]
summary(trainDF)
summary(testDF)
#Dimensions of the dataset
dim(trainDF)

#list types for each attribute
sapply(trainDF, class)


# pick model gbm and find out what type of model it is
getModelInfo()$gbm$type



#################################################
# model it
#################################################


#General Linear Model Start with Lineal regression model
#GLM is a supervised algorithm with a classic statistical technique (Supports thousands of input variables, text and transactional data) used for:
#Classification and/or Regression

trctl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
metric <- "Accuracy"

# a) Linear Discriminant Analysis
trctl <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
metric <- "Accuracy"

set.seed(152)
fit.lda <- train(state ~ main_category + usd_goal_real + backers + campaign, data=trainDF, method="lda", metric=metric, trControl=trctl)
# b) Nonlinear algorithms Classification Tree / Recursive Partitioning
set.seed(152)
fit.cart <- train(state ~ main_category + usd_goal_real + backers + campaign, data=trainDF, method="rpart", metric=metric, trControl=trctl)
# c) Final algorithm Random Forest
set.seed(152)
fit.rf <- train(state ~ main_category + usd_goal_real + backers + campaign, data=trainDF, method="rf", metric=metric, trControl=trctl)

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

## Visualize the accuracy of the models

dotplot(results)
#summarize best model
print(fit.rf)

##Running a Testing
#Estimate skill of Random Forest on trainDF dataset
predictions <- predict(fit.rf, testDF)
head(predictions)
confusionMatrix(predictions,testDF$successful)

#summarize best model
print(fit.rf)

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

