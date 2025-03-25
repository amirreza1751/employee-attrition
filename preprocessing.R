library(tidyverse)
library(caret)
library(dplyr)
file_path <- "dataset/WA_Fn-UseC_-HR-Employee-Attrition.csv"

attrition_data <- read.csv(file_path, stringsAsFactors = TRUE)

cat("Number of rows:", nrow(attrition_data), "\n")
cat("Number of columns:", ncol(attrition_data), "\n")

#print(head(attrition_data, 10))  

cat("Number of missing values:", sum(is.na(attrition_data)), "\n")

attrition_data$Attrition <- as.numeric(attrition_data$Attrition) - 1 
# Yes is 1. No is 0.

cat_cols <- sapply(attrition_data, is.character)  
attrition_data[, cat_cols] <- lapply(attrition_data[, cat_cols], as.factor)  

cols <- c("Over18", "EmployeeNumber", "EmployeeCount", "StandardHours")

attrition_data[cols] <- NULL

constant_columns <- sapply(attrition_data[, sapply(attrition_data, is.numeric)], function(x) sd(x) == 0)

attrition_data_clean <- attrition_data[, !constant_columns]


target_cor_threshold <- 0.1  
high_cor_threshold <- 0.75

numeric_data <- attrition_data %>% select(where(is.numeric))

cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_with_target <- cor_matrix["Attrition", ]
selected_features <- names(cor_with_target[abs(cor_with_target) > target_cor_threshold])


cor_matrix_selected <- cor_matrix[selected_features, selected_features]

cor_melted <- as.data.frame(as.table(cor_matrix_selected))
colnames(cor_melted) <- c("Feature1", "Feature2", "Correlation")
cor_melted <- cor_melted %>% filter(Feature1 != Feature2) 

high_cor_features <- cor_melted %>% filter(abs(Correlation) > high_cor_threshold)

features_to_remove <- c()
for (i in 1:nrow(high_cor_features)) {
  f1 <- high_cor_features$Feature1[i]
  f2 <- high_cor_features$Feature2[i]
  
  if (f1 %in% features_to_remove | f2 %in% features_to_remove) next
  
  if (abs(cor_with_target[f1]) >= abs(cor_with_target[f2])) {
    features_to_remove <- c(features_to_remove, f2)
  } else {
    features_to_remove <- c(features_to_remove, f1)
  }
}

final_features <- setdiff(selected_features, features_to_remove)

selected_data <- attrition_data[, c(final_features, "Attrition")]

cat("Selected Features:\n")
print(final_features)



num_features <- names(attrition_data)[sapply(attrition_data, is.numeric) & names(attrition_data) != "Attrition"]

cat_cols <- sapply(attrition_data, is.factor)

dummy_vars <- dummyVars(~ ., data = attrition_data[, cat_cols])
cat_data <- predict(dummy_vars, newdata = attrition_data)
cat_data <- as.data.frame(cat_data)

final_dataset <- cbind(attrition_data$Attrition, attrition_data[, final_features[final_features != "Attrition"]], cat_data)

colnames(final_dataset)[1] <- "Attrition"

str(final_dataset)

save(final_dataset, file = "prepared_attrition_data.RData")

cat("Dataset shape: ", dim(final_dataset), "\n")
