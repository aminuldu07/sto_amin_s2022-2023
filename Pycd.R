
#######python code#######################################
######################################################
################
import pandas as pd
import numpy as np

# create example dataframes
df1 = pd.DataFrame({
  'USUBJID': ['001', '002', '003', '003', '004', '005', '006', '006'],
  'SETCD': ['A', 'A', 'B', 'B', 'C', 'D', 'D', 'E'],
  'TERMBW': [75.0, 70.0, 80.0, 82.0, 90.0, 85.0, 83.0, 78.0]
})
df2 = pd.DataFrame({
  'USUBJID': ['001', '001', '002', '003', '004', '004', '005', '006'],
  'SETCD': ['A', 'B', 'A', 'B', 'C', 'D', 'D', 'E'],
  'ATERMBW': [80.0, 85.0, 65.0, 83.0, 95.0, 87.0, 82.0, 77.0]
})

# select unique rows from df1 based on USUBJID and SETCD, calculate mean and standard deviation of TERMBW
mean_TERMBW = df1.groupby(['USUBJID', 'SETCD'])['TERMBW'].mean().reset_index()
mean_TERMBW = mean_TERMBW.rename(columns={'TERMBW': 'mean_TERMBW'})
std_TERMBW = df1.groupby(['USUBJID', 'SETCD'])['TERMBW'].std().reset_index()
std_TERMBW = std_TERMBW.rename(columns={'TERMBW': 'SD_TERMBW'})

# merge mean_TERMBW and std_TERMBW to df2 based on USUBJID and SETCD
df2 = pd.merge(df2, mean_TERMBW, on=['USUBJID', 'SETCD'])
df2 = pd.merge(df2, std_TERMBW, on=['USUBJID', 'SETCD'])

# calculate ( ATERMBW - mean_TERMBW) / SD_TERMBW for each row in df2
df2['Z_SCORE'] = (df2['ATERMBW'] - df2['mean_TERMBW']) / df2['SD_TERMBW']
@@####################################################################################################
######################################################################################################

library(dplyr)

# Select unique rows from df1 based on the unique combinations of STUDYID and SETCD
df1_unique <- df1 %>%
  distinct(STUDYID, SETCD, .keep_all = TRUE) 

# Calculate mean and standard deviation of BWSTRESN for each unique combination of STUDYID and SETCD in df1_unique
df1_summary <- df1_unique %>%
  group_by(STUDYID, SETCD) %>%
  summarize(mean_BWSTRESN = mean(BWSTRESN),
            SD_BWSTRESN = sd(BWSTRESN))

# Inner join df2 and df1_summary by STUDYID and SETCD
df_merged <- inner_join(df2, df1_summary, by = c("STUDYID", "SETCD"))

# Calculate z-score for BWSTRESN based on mean_BWSTRESN and SD_BWSTRESN
df_merged <- df_merged %>%
  mutate(zscore = (BWSTRESN - mean_BWSTRESN) / SD_BWSTRESN)

