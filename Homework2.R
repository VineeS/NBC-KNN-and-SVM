library(ElemStatLearn)
library(caret)
library(klaR)

dataSet <- read.csv("/Users/vinee/Documents/workspace/code R/Kaggle-data.csv", header = TRUE)

str(dataSet)
dim(dataSet)

dataSet$label <- as.factor(dataSet$label)
str(dataSet)

# check the missing values
sum(!complete.cases(dataSet))


# Data Prepration 
# 1. Preparing 80 percent data for training and 20 percent for testing
train_index <- createDataPartition(dataSet$label, p = 0.8, list = FALSE)
dataSet_train <- dataSet[train_index,]
dataSet_test <- dataSet[-train_index,]

table(dataSet_train$label)
table(dataSet_test$label)




#-------Training and prediction model on basic NB model
nb1 <- suppressWarnings(train(label ~ ., data = dataSet_train, method = "nb"))
predict_nb1 <- suppressWarnings(predict(nb1, newdata = dataSet_test, type = "raw"))
confusionMatrix(predict_nb1, dataSet_test$label)


# Tune the model
tune_nb2 <- suppressWarnings(train(label ~ ., data = dataSet_train,
                                    method = "nb",
                                    trcontrol = trainControl(method = "cv", number = 3),
                                    tuneGrid = data.frame(fL = 1, usekernel = FALSE, adjust = 1)))

tune_nb2

Predict with Tuned Model
predict_nb2 <- suppressWarnings(predict(model_nb2, newdata = spam_test, type = "raw"))
confusionMatrix(predict_nb2, spam_test$spam)



# ---------- Training model for linear SVM
svm1 <- suppressWarnings(train(label ~ ., data = dataSet_train ,method = "svmLinear",preProcess = c("center", "scale"),trControl = trainControl(method = "boot", number = 25),tuneGrid = expand.grid(C = seq(0, 1, 0.05))))

predict_svm <- suppressWarnings(predict(svm1, newdata = dataSet_test))
confusionMatrix(predict_svm, dataSet_test$label)
plot(svm1)
# tuning 


# measure model performance
print(svm1)

# --- K-Nearest Neighbor method-----
# Training and predicting 


knn1 <- train(label ~ ., data = dataSet_train, method = "knn")
predict_knn <- predict(knn1, newdata = dataSet_test)
confusionMatrix(predict_knn, dataSet_test)
plot(knn1)
# measure model performance
print(knn1)

# Pre-process Data for KNN
pre_process <- suppressWarnings(preProcess(dataSet_train, method = c("scale", "center")))
pre_process

dataSet_train1 <- predict(pre_process, newdata = dataSet_train)
dataSet_test1 <- predict(pre_process, newdata = dataSet_test)
summary(dataSet_train1)

# New model using Standardized Dataset

model_knn1 <- train(label ~ ., data = dataSet_train1, method = "knn")
predict_knn1 <- suppressWarnings(predict(model_knn1, newdata = dataSet_train1))

confusionMatrix(predict_knn1, dataSet_train1$label, positive = "pos")
print(pre_process)

# model campari
comparison <- resamples(list(SVMLinear = svm1, KNN = model_knn1))
summary(comparison)

scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))
bwplot(comparison, scales = scales)
