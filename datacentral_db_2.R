rm(list = ls()) # remove all variables and functions from the current environment in R

#library (this.path)
library(tidyr)
library(dplyr)
library(data.table)
library(ChemmineR)
library(rcdk) 
library(reticulate)# for using python code in R
library(Rcpi) # About 307 descriptor calculation 
library(caret)
setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/Datacentral")

#@@@@#@ Step -9  @@@ :::: build the model where zscore is the function of descriptor 

merged_Smiles_zscores_empty_smiles_deleted <- read.csv('merged_Smiles_zscores_empty_smiles_deleted.csv')

# Group by zscore vaule
SMILES_grouped_merged_Smiles_zscores_empty_smiles_deleted <- merged_Smiles_zscores_empty_smiles_deleted %>% 
  # If there is only one row, assigning the BWSTRESN value as the mean value and 1 as the sd value
    group_by(SMILES) %>%
    summarize(mean_zscore = ifelse(n() > 1, mean(zscore), zscore),
             sd_zscore = ifelse(n() > 1, sd(zscore), 1))


# Group by zscore value and keep all the column 
SMILES_allcol_grouped_merged_Smiles_zscores_empty_smiles_deleted <- merged_Smiles_zscores_empty_smiles_deleted %>% 
  # If there is only one row, assigning the BWSTRESN value as the mean value and 1 as the sd value
  group_by(SMILES) %>%
  summarize(mean_zscore = ifelse(n() > 1, mean(zscore), zscore),
            sd_zscore = ifelse(n() > 1, sd(zscore), 1),
            STUDYID = first(STUDYID),
            APPID = first(APPID),
            USUBJID = first(USUBJID),
            BWSTRESN = first(BWSTRESN),
            BWSTRESU = first(BWSTRESU),
            TSSPECIES = first(TSSPECIES),
            TRTDOSLEVEL = first(TRTDOSLEVEL),
            APPROVAL_ID = first (APPROVAL_ID),
            PUBCHEM = first (PUBCHEM))

# Read the finally cleaned descriptors which have 1522 rows and 47 column 

no_zero_na_df_1532_py_1522row_wd <-read.csv('no_zero_na_df_1532_py_1522row.csv')
# any(is.na(no_zero_na_df_1532_py_1522row )) # check if there are any zero values

library(dplyr)

no_zero_na_df_1532_py_1522row_unique_smiles <- no_zero_na_df_1532_py_1522row_wd %>% 
  distinct(SMILES, .keep_all = TRUE)


# keep only the rows with common SMILES ( Semi join keep only descriptors column)
#common_smiles_df <- semi_join(no_zero_na_df_1532_py_1522row_unique_smiles, SMILES_allcol_grouped_merged_Smiles_zscores_empty_smiles_deleted, by = "SMILES")

# keep only the rows with common SMILES ( left join have all the column from both of the data frame)
left_common_smiles_df <- left_join(no_zero_na_df_1532_py_1522row_unique_smiles, SMILES_allcol_grouped_merged_Smiles_zscores_empty_smiles_deleted, by = "SMILES")

class(left_common_smiles_df$mean_)

common_smiles_df_with_descriptors <- left_common_smiles_df  %>% select(-APPID,
                                                                      -sd_zscore,
                                                                      -APPROVAL_ID,
                                                                      -PUBCHEM,
                                                                      -USUBJID,
                                                                      -STUDYID,
                                                                      -BWSTRESN,
                                                                      -BWSTRESU,
                                                                      -TSSPECIES,
                                                                      -TRTDOSLEVEL)
write.csv(common_smiles_df_with_descriptors, "M_data_modeling_561SMILES.csv", row.names = FALSE )








set.seed(123) # for reproducibility

# Split data into training and testing sets
trainIndex <- createDataPartition(no_zeros_or_nulls$target, p = 0.8, list = FALSE)
train_data <- no_zeros_or_nulls[trainIndex, ]
test_data <- no_zeros_or_nulls[-trainIndex, ]

# Separate labels from the data
train_labels <- train_data$target
train_data <- train_data[, -1]
test_labels <- test_data$target
test_data <- test_data[, -1]

# Define the model architecture
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(11)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Train the model
history <- model %>% fit(
  x = train_data,
  y = train_labels,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2 # use 20% of the training data for validation
)

# Evaluate the model
metrics <- model %>% evaluate(
  x = test_data,
  y = test_labels,
  batch_size = 32
)

# Print the evaluation metrics
print(metrics)
