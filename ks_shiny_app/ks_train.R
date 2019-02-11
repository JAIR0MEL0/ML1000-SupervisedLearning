library("caret")

KS_data = read.csv('./ks_final_data.csv', header = TRUE)

splitIndex <- createDataPartition(KS_data$state, p = .75, list = FALSE, times = 1)
trainDF <- KS_data[ splitIndex,]
testDF  <- KS_data[-splitIndex,]

# Random Forest
set.seed(7)
ks_randomForest <- train(state ~ ., data=trainDF, method="rf", metric="Accuracy", trControl= trainControl(method = "cv", number = 10))

