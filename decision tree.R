library(rpart) # Decision tree
library(rpart.plot) # Plotting decision tree
library(caret) # Accuracy estimation
library(Metrics) # For diferent model evaluation metrics

data<- read.csv("C:/Users/HARIHARAPUTHRA/Downloads/diabetes.csv")


set.seed(123)
index <- sample(2, nrow(Diabetes), prob = c(0.8, 0.2), replace = TRUE)
Diabetes_train <- Diabetes[index==1, ] # Train data
Diabetes_test <- Diabetes[index == 2, ] # Test data

print(dim(Diabetes_train))
print(dim(Diabetes_test))

Diabetes_model <- rpart(formula = diabetes ~., 
                        data = Diabetes_train, 
                        method = "class")

rpart.plot(x = Diabetes_model, yesno = 2, type = 0, extra = 0)

class_predicted <- predict(object = Diabetes_model,  
                           newdata = Diabetes_test,   
                           type = "class")

confusionMatrix(data = class_predicted,       
                reference = Diabetes_test$diabetes)

accuracy(actual = class_predicted,       
         predicted = Diabetes_test$diabetes)

rpart.plot(x = best_model, yesno = 2, type = 0, extra = 0)