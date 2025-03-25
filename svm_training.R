library(tidyverse)
library(caret)
library(smotefamily)  

load("prepared_attrition_data.RData")


str(final_dataset)

final_dataset$Attrition <- as.factor(final_dataset$Attrition)

cat("Dataset shape: ", dim(final_dataset), "\n")
cat("Class distribution before SMOTE:\n")
print(table(final_dataset$Attrition))

train_index <- createDataPartition(final_dataset$Attrition, p = 0.8, list = FALSE)
train_data <- final_dataset[train_index, ]
test_data <- final_dataset[-train_index, ]

set.seed(123)
smote_result <- SMOTE(
  X = train_data[, -which(names(train_data) == "Attrition")],  
  target = train_data$Attrition,  
  K = 5,                        
  dup_size = 4
)


smote_data <- smote_result$data 

colnames(smote_data)[which(names(smote_data) == "class")] <- "Attrition"

cat("Class distribution after SMOTE:\n")
print(table(smote_data$Attrition))

train_control <- trainControl(method = "cv", number = 5)  
c_grid <- expand.grid(C = c(0.01, 0.1, 1, 1.5))

set.seed(123)
svm_model <- train(
  Attrition ~ .,                
  data = smote_data, 
  method = "svmLinear", 
  trControl = train_control ,
  tuneGrid = c_grid   
)

summary(svm_model)

print(svm_model)

svm_predictions <- predict(svm_model, newdata = test_data)

conf_matrix <- confusionMatrix(svm_predictions, test_data$Attrition)
print(conf_matrix)
