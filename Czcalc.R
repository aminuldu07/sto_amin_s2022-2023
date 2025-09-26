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

