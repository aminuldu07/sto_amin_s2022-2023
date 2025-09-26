rm(list = ls()) # remove all variables and functions from the current environment in R

library (this.path)
library(tidyr)
library(data.table)
library(dplyr)

homedir <- dirname(this.path())
setwd(homedir)

my_files <- list.files(path="C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ZSCORECALC", full.names=FALSE)

all_csv <- lapply(my_files, read.csv)

names(all_csv) <- gsub(".csv","",list.files("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ZSCORECALC",full.names = FALSE),fixed = TRUE)

df1 <-  as.data.frame(all_csv$nc)

df2 <- as.data.frame(all_csv$ab)

####1.CR##CD##############################################################################
# # Group by STUDYID and SETCD in df1 and calculate mean and standard deviation of BWSTRESN

df1_grouped <- df1 %>%
  group_by(STUDYID, SETCD) %>%
summarize(mean_BWSTRESN = mean(BWSTRESN),
          sd_BWSTRESN = sd(BWSTRESN))

# # Inner join with df2 on STUDYID and SETCD and zscore calculaiton
joined_df <- inner_join(df1_grouped, df2, by = c("STUDYID", "SETCD"), multiple = "all")

zscore_df <- joined_df %>%
  mutate(zscore = (BWSTRESN - mean_BWSTRESN) / sd_BWSTRESN)
print(zscore_df)
Zscores <- zscore_df %>% select(STUDYID, USUBJID, EXROUTE, SEX, BWTESTCD, SETCD, zscore)
Zscores
# Write the results to a CSV file
write.csv(Zscores, "Zscores.csv", row.names = FALSE)

 ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2.CR##CD#@@@@@@$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######################for@@@@@@@%%%%%%loops CR##CD##########################################################

# Create empty data frames to store the results
df1_grouped <- data.frame()
joined_df <- data.frame()
zscore_df <- data.frame()

# Loop over unique combinations of STUDYID and SETCD in df1
for (studyid in unique(df1$STUDYID)) {
  for (setcd in unique(df1[df1$STUDYID == studyid, ]$SETCD)) {
    
    # Subset df1 to the current group
    filtered_group <- df1[df1$STUDYID == studyid & df1$SETCD == setcd, ]
    
    # Calculate the mean and standard deviation of BWSTRESN for the current group
    mean_BWSTRESN <- mean(filtered_group$BWSTRESN)
    sd_BWSTRESN <- sd(filtered_group$BWSTRESN)
    
    # Append the current group's results to the df1_grouped data frame
    df1_grouped <- rbind(df1_grouped, data.frame(STUDYID = studyid, SETCD = setcd,
                                                 mean_BWSTRESN = mean_BWSTRESN, sd_BWSTRESN = sd_BWSTRESN))
  }
}

# Inner join with df2 on STUDYID and SETCD
for (i in 1:nrow(df1_grouped)) {
  current_group <- df1_grouped[i, ]
  joined_group <- inner_join(df2, current_group, by = c("STUDYID", "SETCD"))
  joined_df <- rbind(joined_df, joined_group)
}

# Calculate zscores for each group
for (i in 1:nrow(joined_df)) {
  current_row <- joined_df[i, ]
  current_zscore <- (current_row$BWSTRESN - current_row$mean_BWSTRESN) / current_row$sd_BWSTRESN
  zscore_df <- rbind(zscore_df, data.frame(STUDYID = current_row$STUDYID, USUBJID = current_row$USUBJID,
                                           EXROUTE = current_row$EXROUTE, SEX = current_row$SEX,
                                           BWTESTCD = current_row$BWTESTCD, SETCD = current_row$SETCD,
                                           zscore = current_zscore))
}

# Select columns for the final data frame
Zscores2 <- zscore_df %>% select(STUDYID, USUBJID, EXROUTE, SEX, BWTESTCD, SETCD, zscore)
Zscores2

# Write the results to a CSV file
write.csv(Zscores2, "Zscores2.csv", row.names = FALSE)

#This code loops over each unique combination of STUDYID and SETCD in df1, subsets df1 to the current group, 
#calculates the mean and standard deviation of BWSTRESN for the current group, and appends the current group's results
#to df1_grouped. It then loops over each row of df1_grouped, joins it with df2 on STUDYID and SETCD, and appends the 
#joined results to joined_df. Finally, it loops over each row of joined_df, calculates the zscore for each row, 
#and appends the results to zscore_df. The code ends by selecting the desired columns for the final data frame,Zscores.##

#........................................................................................................................
### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$3.CR##CD#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#...........................# For loops .................................................................................
 # Initialize empty data frame to store results
 zscore_list <- list()
 
 # Loop through unique combinations of STUDYID and SETCD in df1
 for (i in 1:nrow(unique(df1[, c("STUDYID", "SETCD")]))){
   
   # Subset df1 for current combination of STUDYID and SETCD
   current_subset <- df1[df1$STUDYID == unique(df1$STUDYID)[i] & df1$SETCD == unique(df1$SETCD)[i],]
   
   # Calculate mean and standard deviation of BWSTRESN
   mean_BWSTRESN <- mean(current_subset$BWSTRESN)
   sd_BWSTRESN <- sd(current_subset$BWSTRESN)
   
   # Subset df2 for current combination of STUDYID and SETCD and join with df1 on STUDYID and SETCD
   current_join <- inner_join(df2[df2$STUDYID == unique(df1$STUDYID)[i] & df2$SETCD == unique(df1$SETCD)[i],], 
                              data.frame(STUDYID = unique(df1$STUDYID)[i], SETCD = unique(df1$SETCD)[i], 
                                         mean_BWSTRESN = mean_BWSTRESN, sd_BWSTRESN = sd_BWSTRESN), 
                              by = c("STUDYID", "SETCD"))
   
   # Calculate z-score for current join
   current_join$zscore <- (current_join$BWSTRESN - current_join$mean_BWSTRESN) / current_join$sd_BWSTRESN
   
   # Add current result to list
   zscore_list[[i]] <- current_join[, c("STUDYID", "USUBJID", "EXROUTE", "SEX", "BWTESTCD", "SETCD", "zscore")]
   
 }
 
 # Combine all results into a single data frame
 Zscores1 <- do.call(rbind, zscore_list)

 # Write the results to a CSV file
 write.csv(Zscores1, "Zscores1.csv", row.names = FALSE)
 
 #########################################################################################
 