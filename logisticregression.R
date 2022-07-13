library(car)
library(caret)
library(class)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(performance)


diabetes <- read.csv("C:/Users/HARIHARAPUTHRA/Downloads/diabetes.csv")

diabetes <- diabetes %>%
  mutate(Outcome = factor(Outcome, levels = c(0,1), labels = c("Not Diabetes", "Diabetes")))


RNGkind(sample.kind = "Rounding")
set.seed(23)

intrain <- sample(nrow(diabetes),nrow(diabetes)*.8)
diabetes_train <- diabetes[intrain,]
diabetes_test <- diabetes[-intrain,]

prop.table(table(diabetes_train$Outcome))


diabetes_test_LR <- diabetes_test
diabetes_test_LR$pred_model_backward <- predict(object = LR_diabetes_model_backward,newdata = diabetes_test_LR,type = "response")
diabetes_test_LR$label_model_backward <- as.factor(ifelse(diabetes_test_LR$pred_model_backward > .5, "Diabetes", "Not Diabetes"))

ggplot(diabetes_test_LR, aes(x=pred_model_backward)) +
  geom_density(lwd=0.5) +
  labs(title = "Distribution of Probability Prediction Data",x="Diabetes Probability",y="Density") +
  theme_minimal()

confusionMatrix_LR <- confusionMatrix(data = diabetes_test_LR$label_model_backward,reference = diabetes_test_LR$Outcome,positive = "Diabetes")
confusionMatrix_LR


library(ROCR)
roc_pred=prediction(as.double(Predict,diabetes$Outcome))
roc_perf=performance(roc_pred,measure = "tpr",x.measure = "fpr")
plot(roc_perf)

diabetes_test_LR