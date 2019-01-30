###############################################################################################
####################    Kickstarter lookalike and/or propensity model    #####################
###############################################################################################
#import packages;
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)

#################################################################################
##########################    IMPORING DATASET     ##############################
#################################################################################
getwd();
data=read.csv("~/desktop/ML/YORK/Assigment1/kickstarter-projects/ks-projects-201612.csv", header = TRUE, dec = ".")

#check # of rows
ncol(data);
nrow(data);

#quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(data,10)
str(data)
summary(data)

data <- data[c('name','category','main_category','currency','deadline','goal','state','successful','usd.pledged')]


# what is the proportion of our outcome variable?
prop.table(table(data$state))


# save the outcome for the glmnet model
tempOutcome <- data$state

# generalize outcome and predictor variables
outcomeName <- 'successful'
predictorsNames <- names(data)[names(data) != outcomeName]


#################################################
# model it
#################################################
# get names of all caret supported models 
names(getModelInfo())

data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')

# pick model gbm and find out what type of model it is
getModelInfo()$gbm$type

# split data into training and testing chunks
set.seed(1234)

#This is to use the same data set for Training and Test
splitIndex <- createDataPartition(data[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]

# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)



# run model
objModel <- train(trainDF[,predictorsNames], as.factor(trainDF[,outcomeName]), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))


# find out variable importance
summary(objModel)

# find out model details
objModel


#################################################
# evalutate model
#################################################
# get predictions on your testing data

# class prediction
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
head(predictions)
postResample(pred=predictions, obs=as.factor(testDF[,outcomeName]))

# probabilities 
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
head(predictions)
postResample(pred=predictions[[2]], obs=ifelse(testDF[,outcomeName]=='yes',1,0))
auc <- roc(ifelse(testDF[,outcomeName]=="yes",1,0), predictions[[2]])
print(auc$auc)

























####################   VISUALIZATIONS   ###############################

#1A. barplot with absolute counts
barplot(freq_tbl)

#1B. barplot with proportions - easier to read
barplot(prop.table(freq_tbl))

#2. create mosaic plot (plot of proportions)/ pie chart [useful for many levels]
mosaicplot(freq_tbl)
pie(freq_tbl)

#Becomes much more useful for categorial variables with many levels
mosaicplot(table(main_category))
pie(table(main_category))

#3. Stacked bar-plot - i.e. plotting it x-tab
freq_xtab=xtabs(~main_category+state)
head(freq_xtab)

#basic bar-plot will automatically be stacked if we give it the x-tab table as an input
barplot(freq_xtab)
barplot(prop.table(freq_xtab))
barplot(prop.table(freq_xtab,2), legend=rownames(freq_xtab))

#next we can add labels and stuff, but dont let that stuff confuse you...don't worry about memorizing syntax or antying for that...
barplot(prop.table(freq_xtab,2),
        legend=rownames(freq_xtab),
        ylab="Target Variable", xlab="Categories",
        main = "Looking for difference in Target Variable for different categiories target",
        col = c("red", "blue")
)

barplot(freq_xtab, legend=rownames(freq_xtab), ylab="state", xlab="main_category", col=c("orange","deepskyblue1"), beside=T, las=1)


category_vs_rwd_freq_xtab = table(category,state)
category_vs_state_xtab = prop.table(category_vs_rwd_freq_xtab,2)
barplot(category_vs_state_xtab,  col=c("orange","deepskyblue1"))



########################################################################################
#######################    DATA EXPLORATION: NUMERIC VARIABLES     #####################
########################################################################################

################################ INDEPENDENT EXPLORATION ###############################
#1. Simple summary(data) is very useful for numeric
summary(data)

#2 Hisograms are you friend
hist(data$country)
hist(log(data$country))

#3 What if we want to get histograms for all numeric variables? instead of 1 at a time
#We use a super helpful data manipulation package called reshape and a powerful plotting package called ggplot2...
#Step 1: Get only numeric colums
list_of_numcols = sapply(data, is.numeric)
numcols = data[ , list_of_numcols]

str(data)
str(numcols)

#Step 2: Melt data --> Turn it into skinny LONG table
melt_data = melt(numcols, id.vars=c("ID"))
head(melt_data, 10)

#Step 3: This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
