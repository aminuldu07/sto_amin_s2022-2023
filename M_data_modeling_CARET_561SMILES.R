rm(list = ls()) # remove all variables and functions from the current environment in R

#library (this.path)
library(tidyr)
library(dplyr)
library(data.table)
#library(ChemmineR)
library(rcdk) 
#library(reticulate)# for using python code in R
library(Rcpi) # About 307 descriptor calculation

library(caret)

library(mlr)

library(randomForest)

library(mlflow)

setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/Datacentral/Model building_R")

# One-Hot Encoding for the SMILES column

# Load the required library
library(dplyr)

# Load the dataset
df <- read.csv("M_data_modeling_561SMILES.csv")

# Perform One-Hot Encoding using the dummyVars function from the caret package
library(caret)
df_encoded <- predict(dummyVars(~ SMILES, data = df), newdata = df)

# Combine the original data frame and the encoded data frame column-wise
df_final <- cbind(df, df_encoded)

# View the final data frame
head(df_final)

# caret package in R to train a random forest model on the df_final data frame:

# Load the required libraries and data
library(dplyr)
library(caret)
library(randomForest)

#df_final <- read.csv("df_final.csv")

# Specify the response and predictor columns
response_col <- "mean_zscore"
predictor_cols <- setdiff(colnames(df_final), response_col)

# Split the data into training and testing sets
set.seed(123)
train_idx <- createDataPartition(df_final[, response_col], p = 0.8, list = FALSE)
train_data <- df_final[train_idx, ]
test_data <- df_final[-train_idx, ]

# Train a random forest model using the train data
rf_model <- randomForest(train_data[, predictor_cols], train_data[, response_col])

# Make predictions on the test data
test_pred <- predict(rf_model, newdata = test_data[, predictor_cols])

# Create a data frame with predicted, actual, and difference values
results_df <- data.frame(
  predicted_value = test_pred,
  original_value = test_data[, response_col],
  difference = test_pred - test_data[, response_col]
)

#head(results_df)

# Graphical evaluations of the predicted data 

# Create scatter plot
library(ggplot2)
ggplot(results_df, aes(x = predicted_value, y = original_value)) +
  geom_point() +
  labs(x = "Predicted Value", y = "Actual Value", title = "Scatter Plot of Predicted vs. Actual Values")

# Create density plot
ggplot(results_df, aes(x = difference)) +
  geom_density(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "Difference", y = "Density", title = "Density Plot of Differences between Predicted and Actual Values")


# Create box plot
results_df$actual_range <- cut(results_df$original_value, breaks = c(-Inf, 0, 2, 4, 6, Inf))
ggplot(results_df, aes(x = actual_range, y = difference)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "Actual Value Range", y = "Difference", title = "Box Plot of Differences between Predicted and Actual Values by Actual Value Range")



#@@@@@@@@@@@M_data_modeling_561SMILES_wihhoutSMILES.csv#######################
#..................................................................................
#,..............,,,,,,,,,,,,,,....................................................

# Load the dataset
data <- read.csv("M_data_modeling_561SMILES_wihhoutSMILES.csv")

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(data), 0.7*nrow(data))
train <- data[trainIndex,]
test <- data[-trainIndex,]

# Train a Random Forest regression model
library(randomForest)
model <- randomForest(mean_zscore ~ ., data = train, ntree = 500, mtry = sqrt(ncol(train)-1))

# Make predictions on the testing set
predictions <- predict(model, newdata = test)

# Calculate the RMSE and R-squared values
library(Metrics)
rmse <- rmse(predictions, test$mean_zscore)
r_squared <- R2(predictions, test$mean_zscore)

# Print the RMSE and R-squared values
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Create a data frame with the predicted and actual values
results <- data.frame(Predicted = predictions, Actual = test$mean_zscore, Difference = predictions - test$mean_zscore)

# Print the first few rows of the results
head(results)

# Plot the predicted vs. actual values
library(ggplot2)
ggplot(results, aes(x = Actual, y = Predicted)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("Predicted vs. Actual Mean Z-score") + 
  xlab("Actual Mean Z-score") + 
  ylab("Predicted Mean Z-score")

class(data$mean_zscore)
derere <- data.frame(unique(data$mean_zscore))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

## format the mean_zscore column to three decimal places

# Load the dataset
data <- read.csv("M_data_modeling_561SMILES_wihhoutSMILES.csv")

# Round the mean_zscore column to three decimal places
data$mean_zscore <- round(data$mean_zscore, digits = 2)

digit2 <- data.frame (unique(data$mean_zscore))

digit3 <- data.frame (unique(data[,'mean_zscore' ]))

# Select unique rows based on the "name" column
unique_rows <- subset(data, !duplicated(data$mean_zscore))

#write.csv(unique_rows,"t3digit_zs_M_data_modeling_561SMILES_wihhoutSMILES.csv", row.names = FALSE)


df_final <- read.csv("t3digit_zs_M_data_modeling_561SMILES_wihhoutSMILES.csv")

# Specify the response and predictor columns
response_col <- "mean_zscore"
predictor_cols <- setdiff(colnames(df_final), response_col)

# Split the data into training and testing sets
set.seed(123)
train_idx <- createDataPartition(df_final[, response_col], p = 0.8, list = FALSE)
train_data <- df_final[train_idx, ]
test_data <- df_final[-train_idx, ]

# Train a random forest model using the train data
rf_model <- randomForest(train_data[, predictor_cols], train_data[, response_col])

# Make predictions on the test data
test_pred <- predict(rf_model, newdata = test_data[, predictor_cols])

# Create a data frame with predicted, actual, and difference values
results_df <- data.frame(
  predicted_value = test_pred,
  original_value = test_data[, response_col],
  difference = test_pred - test_data[, response_col]
)

#head(results_df)

# Graphical evaluations of the predicted data 

# Create scatter plot
library(ggplot2)
ggplot(results_df, aes(x = predicted_value, y = original_value)) +
  geom_point() +
  labs(x = "Predicted Value", y = "Actual Value", title = "Scatter Plot of Predicted vs. Actual Values")

# Create density plot
ggplot(results_df, aes(x = difference)) +
  geom_density(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "Difference", y = "Density", title = "Density Plot of Differences between Predicted and Actual Values")


# Create box plot
results_df$actual_range <- cut(results_df$original_value, breaks = c(-Inf, 0, 2, 4, 6, Inf))
ggplot(results_df, aes(x = actual_range, y = difference)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "Actual Value Range", y = "Difference", title = "Box Plot of Differences between Predicted and Actual Values by Actual Value Range")



#@@@@@@@@@@@M_data_modeling_561SMILES_wihhoutSMILES.csv#######################
#..................................................................................
#,..............,,,,,,,,,,,,,,....................................................

# Load the dataset
data <- read.csv("M_data_modeling_561SMILES_wihhoutSMILES.csv")

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(data), 0.7*nrow(data))
train <- data[trainIndex,]
test <- data[-trainIndex,]

# Train a Random Forest regression model
library(randomForest)
model <- randomForest(mean_zscore ~ ., data = train, ntree = 500, mtry = sqrt(ncol(train)-1))

# Make predictions on the testing set
predictions <- predict(model, newdata = test)

# Calculate the RMSE and R-squared values
library(Metrics)
rmse <- rmse(predictions, test$mean_zscore)
r_squared <- R2(predictions, test$mean_zscore)

# Print the RMSE and R-squared values
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Create a data frame with the predicted and actual values
results <- data.frame(Predicted = predictions, Actual = test$mean_zscore, Difference = predictions - test$mean_zscore)

# Print the first few rows of the results
head(results)

# Plot the predicted vs. actual values
library(ggplot2)
ggplot(results, aes(x = Actual, y = Predicted)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("Predicted vs. Actual Mean Z-score") + 
  xlab("Actual Mean Z-score") + 
  ylab("Predicted Mean Z-score")

class(data$mean_zscore)
derere <- data.frame(unique(data$mean_zscore))

setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/Datacentral/MORDRED")
py_data <- read.csv("SMILES_descriptors.csv")
unique <- data.frame(unique(py_data$SMILES))

# Subset data frame to remove columns with any zero or null values
no_zero_df <- py_data[, apply(py_data != 0 & !is.na(py_data), 2, all)]

# Print the new data frame
print(no_zero_df) # no_zero_df has 13  columns

class(no_zero_df$X600)

