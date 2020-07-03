Practical Machine Learning_Week 4 Assignment
Md.Nazmul hossen

###Load Libraries required  
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(RGtk2)
library(rattle)
library(randomForest)
library(gbm)

##datasets download
train_url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


init_org_training_data <- read.csv(url(train_url))
init_org_testing_data <- read.csv(url(test_url))

dim(init_org_training_data)

dim(init_org_testing_data)


##Data Cleaning
#Removing Variables which are having nearly zero variance.
non_zero_var <- nearZeroVar(init_org_training_data)

org_training_data <- init_org_training_data[,-non_zero_var]
org_testing_data <- init_org_testing_data[,-non_zero_var]

dim(org_training_data)
dim(org_testing_data)
#Removing Variables which are having NA values. Our threshhold is 95%.
na_val_col <- sapply(org_training_data, function(x) mean(is.na(x))) > 0.95

org_training_data <- org_training_data[,na_val_col == FALSE]
org_testing_data <- org_testing_data[,na_val_col == FALSE]

dim(org_training_data)

dim(org_testing_data)
#Removing variables which are non-numeric and hence will not contribute into our model
org_training_data <- org_training_data[,8:59]
org_testing_data <- org_testing_data[,8:59]

dim(org_training_data)

dim(org_testing_data)


##colnames_train <- names(org_training_data)

##org_training_data <- init_org_training_data[, c(colnames_train, "problem_id")]

colnames(org_training_data)

colnames(org_testing_data)

#Data Partitioning
inTrain <- createDataPartition(org_training_data$classe, p=0.6, list=FALSE)
training <- org_training_data[inTrain,]
testing <- org_training_data[-inTrain,]

dim(training)

dim(testing)

##Decision Tree Model
DT_modfit <- train(classe ~ ., data = training, method="rpart")

DT_prediction <- predict(DT_modfit, testing)
confusionMatrix(DT_prediction, testing$classe)

rpart.plot(DT_modfit$finalModel, roundint=FALSE)

#Random Forest Model
RF_modfit <- train(classe ~ ., data = training, method = "rf", ntree = 100)

RF_prediction <- predict(RF_modfit, testing)
RF_pred_conf <- confusionMatrix(RF_prediction, testing$classe)
RF_pred_conf

plot(RF_pred_conf$table, col = RF_pred_conf$byClass, 
     main = paste("Random Forest - Accuracy Level =",
                  round(RF_pred_conf$overall['Accuracy'], 4)))


#Gradient Boosting Model
GBM_modfit <- train(classe ~ ., data = training, method = "gbm", verbose = FALSE)
GBM_modfit$finalModel

GBM_prediction <- predict(GBM_modfit, testing)

GBM_pred_conf <- confusionMatrix(GBM_prediction, testing$classe)
GBM_pred_conf


plot(GBM_pred_conf$table, col = GBM_pred_conf$byClass, 
     main = paste("Gradient Boosting - Accuracy Level =",
                  round(GBM_pred_conf$overall['Accuracy'], 4)))


RF_pred_conf$overall

GBM_pred_conf$overall


#conclution
Final_RF_prediction <- predict(RF_modfit, org_testing_data )
Final_RF_prediction
