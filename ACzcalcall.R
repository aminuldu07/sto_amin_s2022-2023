rm(list = ls()) # remove all variables and functions from the current environment in R

library (this.path)
library(tidyr)
library(dplyr)
library(data.table)

setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ZSCORECALC")
df1 <- read.csv("nc.csv")
df2 <- read.csv("ab.csv")

####1.CR##CD##############################################################################
df1_grouped <- df1 %>%
  group_by(STUDYID, SETCD) %>%
summarize(mean_BWSTRESN = mean(BWSTRESN),
          sd_BWSTRESN = sd(BWSTRESN))

joined_df <- inner_join(df1_grouped, df2, by = c("STUDYID", "SETCD"), multiple = "all")

zscore_df <- joined_df %>%
  mutate(zscore = (BWSTRESN - mean_BWSTRESN) / sd_BWSTRESN)
print(zscore_df)
Zscores <- zscore_df %>% select(STUDYID, USUBJID, EXROUTE, SEX, BWTESTCD, SETCD, zscore)
write.csv(Zscores, "Zscores.csv", row.names = FALSE)

 ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2.CR##CD#@@@@@@$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######################for@@@@@@@%%%%%%loopsCR##CD##########################################################

df1_grouped <- data.frame()
joined_df <- data.frame()
zscore2_df <- data.frame()

for (study in unique(df1$STUDYID)) {
  for (setcd in unique(df1[df1$STUDYID == study, ]$SETCD)) {
    filtered_group <- df1[df1$STUDYID == study & df1$SETCD == setcd, ]
    mean_BWSTRESN <- mean(filtered_group$BWSTRESN)
    sd_BWSTRESN <- sd(filtered_group$BWSTRESN)
    df1_grouped <- rbind(df1_grouped, data.frame(STUDYID = study, SETCD = setcd,
                                                 mean_BWSTRESN = mean_BWSTRESN, sd_BWSTRESN = sd_BWSTRESN))
  }
}

for (i in 1:nrow(df1_grouped)) {
  current_group <- df1_grouped[i, ]
  joined_group <- inner_join(df2, current_group, by = c("STUDYID", "SETCD"))
  joined_df <- rbind(joined_df, joined_group)
}

for (i in 1:nrow(joined_df)) {
  current_row <- joined_df[i, ]
  current_zscore <- (current_row$BWSTRESN - current_row$mean_BWSTRESN) / current_row$sd_BWSTRESN
  zscore2_df <- rbind(zscore2_df, data.frame(STUDYID = current_row$STUDYID, USUBJID = current_row$USUBJID,
                                           EXROUTE = current_row$EXROUTE, SEX = current_row$SEX,
                                           BWTESTCD = current_row$BWTESTCD, SETCD = current_row$SETCD,
                                           zscore = current_zscore))
}

zscores2 <- zscore2_df

write.csv(zscores2, "zscores2.csv", row.names = FALSE)
###########################################################################################
#,.........................;;;;;;;;;;;;3.CR##CD#;;;;;;;;;;;;;;;;;;;;,...........................
# Create an empty data frame to store the z-scores
Zscores3 <- data.frame(STUDYID = character(),
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
    Zscores3[nrow(Zscores3) + 1, ] <- c(current_studyid,
                                        current_group_df2$USUBJID[j],
                                        current_group_df2$EXROUTE[j],
                                        current_group_df2$SEX[j],
                                        current_group_df2$BWTESTCD[j],
                                        current_setcd,
                                        current_zscore)
  }
}

# Print the resulting data frame
print(Zscores3)
# Write the results to a CSV file
write.csv(Zscores3, "Zscores3.csv", row.names = FALSE)
