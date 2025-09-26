rm(list = ls()) # remove all variables and functions from the current environment in R

library (this.path)
library(tidyr)
library(data.table)
library(dplyr)

setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ZSCORECALC_Storage")

df1 <- read.csv("NCBW.csv")

df2 <- read.csv("ABW.csv")

# Create empty data frame to store the results
zscore_df <- data.frame()

# Loop over each unique combination of STUDYID and SETCD in df1
for (study_id in unique(df1$STUDYID)) {
  for (set_cd in unique(df1$SETCD)) {
    # Subset df1 to the filtered group
    filtered_group <- df1[df1$STUDYID == study_id & df1$SETCD == set_cd, ]
    
    # Calculate the mean and standard deviation of BWSTRESN for the current group
    mean_BWSTRESN <- mean(filtered_group$BWSTRESN)
    sd_BWSTRESN <- sd(filtered_group$BWSTRESN)
    
    # Subset df2 to the current group and join with the mean and standard deviation values
    filtered_df2 <- df2[df2$STUDYID == study_id & df2$SETCD == set_cd, ] %>% distinct(STUDYID, SETCD, .keep_all = TRUE)
    joined_df <- inner_join(filtered_df2, filtered_group, by = c("STUDYID", "SETCD")) %>%
      mutate(zscore = (BWSTRESN - mean_BWSTRESN) / sd_BWSTRESN)
    
    # Append the current group's results to the z-score data frame
    zscore_df <- rbind(zscore_df, joined_df)
  }
}

# Select columns for the final data frame
Zscoresf <- zscore_df %>% select(STUDYID, USUBJID, EXROUTE, SEX, BWTESTCD, SETCD, zscore)
#..............................................
#............................................
# Create an empty data frame to store the z-scores
Zscores2 <- data.frame(STUDYID = character(),
                       USUBJID = character(),
                       EXROUTE = character(),
                       SEX = character(),
                       BWTESTCD = character(),
                       SETCD = character(),
                       zscore = numeric())

# Get unique combinations of STUDYID and SETCD
unique_groups <- unique(df1[, c("STUDYID", "SETCD")])

# Loop through each unique group
for (i in seq_along(unique_groups$STUDYID)) {
  # Get the current group's values for STUDYID and SETCD
  current_studyid <- unique_groups$STUDYID[i]
  current_setcd <- unique_groups$SETCD[i]
  
  # Subset df1 to the current group
  current_group_df1 <- df1[df1$STUDYID == current_studyid & df1$SETCD == current_setcd, ]
  
  # Calculate the mean and standard deviation of BWSTRESN for the current group
  current_mean <- mean(current_group_df1$BWSTRESN)
  current_sd <- sd(current_group_df1$BWSTRESN)
  
  # Subset df2 to the current group
  current_group_df2 <- df2[df2$STUDYID == current_studyid & df2$SETCD == current_setcd, ]
  
  # Calculate the z-score for each row in the current group
  for (j in seq_along(current_group_df2$BWSTRESN)) {
    current_zscore <- (current_group_df2$BWSTRESN[j] - current_mean) / current_sd
    
    # Add the z-score to the Zscores data frame
    Zscores2[nrow(Zscores2) + 1, ] <- c(current_studyid,
                                        current_group_df2$USUBJID[j],
                                        current_group_df2$EXROUTE[j],
                                        current_group_df2$SEX[j],
                                        current_group_df2$BWTESTCD[j],
                                        current_setcd,
                                        current_zscore)
  }
}

# Print the resulting data frame
print(Zscores2)
# Write the results to a CSV file
write.csv(Zscores2, "Zscores2.csv", row.names = FALSE)

##################################################################################################################### 

