library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)

data<- read.csv("C:/Users/HARIHARAPUTHRA/Downloads/diabetes.csv")

#Setting outcome variables as categorical 
data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))

str(data)

indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)

training <- data[indxTrain,]
testing <- data[-indxTrain,]

prop.table(table(data$Outcome)) * 100

prop.table(table(training$Outcome)) * 100

prop.table(table(testing$Outcome)) * 100

x = training[,-9]
y = training$Outcome

library(e1071)

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model

Predict <- predict(model,newdata = testing )

Predict

confusionMatrix(Predict, testing$Outcome )

X <- varImp(model)
plot(X)

data[, 2:7][data[, 2:7] == 0] <- NA


library(pROC)
roc=roc(response=testing$Outcome, predictor=Predict[,"1"])
dim(Predict)
