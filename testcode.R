rm(list = ls()) # remove all variables and functions from the current environment in R

library (this.path)
library(tidyr)
library(dplyr)
library(data.table)

setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/testAnimalZsxore")
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

