rm(list = ls()) # remove all variables and functions from the current environment in R

#library (this.path)
library(tidyr)
library(dplyr)
library(data.table)
library(ChemmineR)
library(rcdk) 
library(reticulate)# for using python code in R
library(Rcpi) # About 307 descriptor calculation 

setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/Datacentral")


#@@@@#@ Step -1 @@@ ::::  Reading and cleaning NC file ("IND147206NC.csv")

dfNC <- read.csv("IND147206NC.csv")
dfNC$BWSTRESN <- as.numeric(dfNC$BWSTRESN)

# subset function to remove rows where the BWSTRESN column has the value "NULL"
dfNC_cleanf <- subset(dfNC, BWSTRESN != "NULL")

#write.csv(dfNC_cleanf, "dfNC_cleanf.csv", row.names = FALSE)


#@@@@#@ Step -2 @@@ ::::Reading and cleaning Animal Body weight  file ("IND147206AB.csv")

#"IND147206AB.csv"data were Initially cleaned manually by deleting the unwanted rows manually

dfAB <- read.csv("IND147206AB_cleaned.csv")

# Desired columns selections
selected_cols <- c("APPID", "STUDYID", "USUBJID", "BWSTRESN", "BWSTRESU", "TSSPECIES", "TRTDOSLEVEL")
dfAB1 <- dfAB[, selected_cols]

## replace the single comma and keep remaining
dfAB1$TRTDOSLEVEL <- gsub(",", "", dfAB1$TRTDOSLEVEL)

# subset function to remove rows where the BWSTRESN column has the value "NULL"
dfAB_cleanf <- subset(dfAB1, BWSTRESN != "NULL")

#Creation of new data from df_cleaned_n where BWSTERNS & TRTDOSLEVE is numeric

# Convert numeric for BWSTERNS & TRTDOSLEVEL
dfAB_cleanf$BWSTRESN <- as.numeric(dfAB_cleanf$BWSTRESN)

dfAB_cleanf$TRTDOSLEVEL <- as.numeric(dfAB_cleanf$TRTDOSLEVEL)

#write.csv(dfAB_cleanf, "dfAB_cleanf.csv", row.names = FALSE)



#@@@@#@ Step -3 @@@ :::: ( High Dose Selection) For loop for getting the TRTDOSLEVEL... 
# from "dfAB_cleanf.csv" ( cleaned "IND147206AB.csv")

df <- read.csv("dfAB_cleanf.csv")

# Initialize an empty data frame to store the results
highdose_dfAB_cleanf <- data.frame()

# Loop over each unique STUDYID
for (unique_studyid in unique(df$STUDYID)) {
  
  # Subset the data frame to only include rows with the current STUDYID
  subset_df <- df[df$STUDYID == unique_studyid , ]
  
  # Get the index of the row with the highest TRTDOSLEVEL value
  max_index <- which.max(as.numeric(gsub(",", ".", subset_df$TRTDOSLEVEL)))
  
  # Extract the row with the highest TRTDOSLEVEL value
  max_row <- subset_df[max_index, ]
  
  # Append the max_row to the result_df data frame
  highdose_dfAB_cleanf <- rbind(highdose_dfAB_cleanf, max_row)
}
# Print the result data frame
highdose_dfAB_cleanf

#write.csv(highdose_dfAB_cleanf , "highdose_dfAB_cleanf.csv", row.names = FALSE)



#@@@@#@ Step -4 @@@ :::: "ZSCORES" calculations for each unique STUDYID

# Reading the final NC data ("dfNC_cleanf.csv")
df1 <-  read.csv("dfNC_cleanf.csv")

# Reading the final AB High Dose Selection data
df2 <- highdose_dfAB_cleanf

## NC grouping for each STUDYID for BWSTRESN column only
df1_grouped <- df1 %>% ## ## Excludes where each STUDYID has one row and keep multiple rows
  group_by(STUDYID) %>%
  summarize(mean_BWSTRESN = if(n() > 1) mean(BWSTRESN) else NA_real_,
            sd_BWSTRESN = if(n() > 1) sd(BWSTRESN) else NA_real_) %>%
  na.omit()

# ## NC grouping for each STUDYID and keep all the columns 
# df1_grouped1 <- df1 %>% na.omit() %>%
#   group_by(STUDYID) %>%
#   summarise(mean_BWSTRESN = ifelse(n() > 1, mean(BWSTRESN), BWSTRESN),
#             sd_BWSTRESN = ifelse(n() > 1, sd(BWSTRESN), 1),
#             across(.cols = everything(), .fns = ~first(.)))

## Next task to find the difference between df1_grouped & df1_grouped1

# df1_grouped1 <- df1 %>% ## includes where each STUDYID has only one row
#   group_by(STUDYID) %>%
#   summarize(mean_BWSTRESN = mean(BWSTRESN),
#             sd_BWSTRESN = sd(BWSTRESN))
# 
#  
# df1_grouped2 <- df1 %>% # If there is only one row, assigning the BWSTRESN value as the mean value and 1 as the sd value
#   group_by(STUDYID) %>%
#   summarize(mean_BWSTRESN = ifelse(n() > 1, mean(BWSTRESN), BWSTRESN),
#             sd_BWSTRESN = ifelse(n() > 1, sd(BWSTRESN), 1))

# joining grouped NC data with  highdose_dfAB_cleanf data 
joined_df <- inner_join(df1_grouped, df2, by = c("STUDYID"), multiple = "all")

# Zscore calcualtion
zscore_df <- joined_df %>%
  mutate(zscore = (BWSTRESN - mean_BWSTRESN) / sd_BWSTRESN) %>% 
  filter(!is.na(zscore), zscore != -Inf, zscore != Inf) # filtering the 'null/inf' values

# Reorganize the column of the zscore_df
zscore_df_org <- zscore_df %>%
select(APPID, STUDYID, USUBJID, zscore, mean_BWSTRESN, sd_BWSTRESN, BWSTRESN, BWSTRESU, TSSPECIES, TRTDOSLEVEL)

#write.csv(zscore_df_org , "zscore.csv", row.names = FALSE)




#@@@@#@ Step -5 @@@ :::: GSRS data processing @@@@@@ for......................... 
#...................................................... Inner joining 'zscore' data frame with GSRS data frame on 'APPID'


# Merge "BrowseApplications_" & "export_" files on Approval_ID
data1_APPID <- read.csv("BrowseApplications-13-04-2023_7-08-00.csv")
data2_smiles <- read.csv("export-13-04-2023_7-08-24.csv")
merged_data <- merge(data1_APPID, data2_smiles, by = "APPROVAL_ID")
#write.csv(merged_data, "merged_on_Approval_ID.csv", row.names = FALSE)

# 'APPID' column generation by pasting 'APP_TYPE'&	'APP_NUMBER' columns
data_APPID <- read.csv("merged_on_Approval_ID.csv") 
data_APPID$APPID <- paste(data_APPID$APP_TYPE, data_APPID$APP_NUMBER, sep = "")
data_APPID <- data_APPID[, c(45, 1:44)] ### column reshuffle...
data_APPID <- data_APPID  %>%  select(APPID, SMILES,INCHIKEY, PRODUCT_NAME, PUBCHEM)
#colnames(data3)[3] <- "APPID"
# write.csv(data_APPID, "merged_data_APPID.csv", row.names = FALSE)

# Checking unique APPID number in "merged_data_APPID.csv" data frame 
uniqueAPPIDin_data_APPID <- unique (data_APPID$APPID)
uniqueAPPIDin_data_APPID_df <- data.frame(APPID = uniqueAPPIDin_data_APPID) # data frame




#@@@@#@ Step -6 @@@ :::: Inner join 'zscore' data frame with GSRS_merged data frame on 'APPID'

## merging data based on a column ( APPID column)
gsrsmerged <- read.csv("merged_data_APPID.csv")
fzscores <- read.csv("zscore.csv")

# Checking unique APPID number in zscore 
uniqueAPPID_in_fzscores <- unique (fzscores$APPID)
uniqueAPPID_in_fzscores_df <- data.frame(APPID = uniqueAPPID_in_fzscores ) # data frame



# Inner join on the APPID column
merged_df <- inner_join(uniqueAPPIDin_data_APPID_df, uniqueAPPID_in_fzscores_df, by = "APPID")


# merging zscore & agglomerated gsrs data 
finalmerged <- merge(gsrsmerged, fzscores, by = "APPID")

# Reorganize the column of the score_df
finalmerged <- finalmerged %>%
  select(APPID, PUBCHEM,SMILES,STUDYID, USUBJID, zscore, mean_BWSTRESN, sd_BWSTRESN, BWSTRESN, BWSTRESU, TSSPECIES, TRTDOSLEVEL)

# Filter to remove the rows not having SMILES 
finalmerged %>% filter(!is.na(SMILES))

#write.csv(finalmerged, "merged_Smiles_zscores.csv", row.names = FALSE)

### ????? finalmerged %>% filter(!is.na(SMILES)) code couldn't clean the empty SMILES column value...............
### ..............................................therefore, manually deleted the empty SMILES.........................

## Is there any converter to convert SMILES to Compund ?????????????????????????????????

# Read the empty SMILES deleted file ("merged_Smiles_zscores_empty_smiles_deleted.csv")
merged_Smiles_zscores_empty_smiles_deleted <- read.csv("merged_Smiles_zscores_empty_smiles_deleted.csv")


#@@@@#@ Step -7 @@@ :::: get the data frame containing APPID, STUDYID, SMILEID, zscore 
# Select the column from the "merged_Smiles_zscores_empty_smiles_deleted.csv"

merged_Smiles_zscores_empty_smiles_deleted <- merged_Smiles_zscores_empty_smiles_deleted  %>% 
  select (SMILES,APPID, STUDYID, zscore )



#@@@@#@ Step -8 @@@ :::: calculate the descriptor for each of the smile 


#Selection of the SMILES column from the csv file 
merged_Smiles_zscores_empty_smiles_deleted <- merged_Smiles_zscores_empty_smiles_deleted # from setp 7


# Calculate all molecular descriptors using the Molecule object and For Loop 

# create an empty list to store descriptors for each SMILES
descriptors_list <- list()

# iterate over every element in the SMILES column
for (smile in merged_Smiles_zscores_empty_smiles_deleted$SMILES) {
  # parse the SMILES to create the molecules object using the parse.smiles() function from the rcdk package
  molecules_obj <- parse.smiles(smile)
  
  # get the descriptors using the extractDrugAIO function
  descriptors <- extractDrugAIO(molecules_obj, silent = TRUE, warn = TRUE)
  
  # add the descriptors to the list
  descriptors_list[[length(descriptors_list)+1]] <- descriptors
}

# convert the list of descriptors to a data frame
descriptors_df <- do.call(rbind, descriptors_list)

# add the SMILES column to the data frame
final_df_descriptors <- cbind(merged_Smiles_zscores_empty_smiles_deleted["SMILES"], descriptors_df)

#write.csv(final_df_descriptors, file = "final_df_descriptors_rowT.csv", row.names = TRUE)

###########@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##########################################################
#............................................................................................................
#...........................................................................................................

##@@@@@@@@@@@@@@@@@@@@ Work on Molecular descriptors @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(tidyr)
library(dplyr)
library(data.table)
library(rcdk) 
library(reticulate)# for using python code in R
library(Rcpi) # About 307 descriptor calculation 

rm(list = ls()) # remove all variables and functions from the current environment in R
setwd("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/Datacentral")

#$#$# It is important that 293 descriptor column both has integer and numeric values. 
### Selection columns from 293 descriptors column 
## If, any column having non zero or null values ===  "1541" , then number of selected column 13
## If, any column having non zero or null values ===  "1540" , then number of selected column 14
## If, any column having non zero or null values ===  "1539" , then number of selected column 25
## If, any column having non zero or null values ===  "1537" , then number of selected column 32
## If, any column having non zero or null values ===  "1536" , then number of selected column 33
## If, any column having non zero or null values ===  "1535" , then number of selected column 35
## If, any column having non zero or null values ===  "1534" , then number of selected column 43
## If, any column having non zero or null values ===  "1533" , then number of selected column 44
## If, any column having non zero or null values ===  "1532" , then number of selected column 47
## If, any column having non zero or null values ===  "1531" , then number of selected column 47
## If, any column having non zero or null values ===  "1530" , then number of selected column 47
## If, any column having non zero or null values ===  "1529" , then number of selected column 47
## If, any column having non zero or null values ===  "1528" , then number of selected column 48
## If, any column having non zero or null values ===  "1527" , then number of selected column 48
## If, any column having non zero or null values ===  "1526" , then number of selected column 48
## If, any column having non zero or null values ===  "1525" , then number of selected column 48
## If, any column having non zero or null values ===  "1520" , then number of selected column 49
## If, any column having non zero or null values ===  "1515" , then number of selected column 50
## If, any column having non zero or null values ===  "1510" , then number of selected column 52
## If, any column having non zero or null values ===  "1500" , then number of selected column 62
## If, any column having non zero or null values ===  "1490" , then number of selected column 67
## If, any column having non zero or null values ===  "1475" , then number of selected column 72
## If, any column having non zero or null values ===  "1450" , then number of selected column 78

# Read the final_df_descriptors.csv
final_df_descriptors_with_SMILES_Col_ <- read.csv('final_df_descriptors.csv')


#class(final_df_descriptors_with_SMILES_Col_$nS)
# # select all columns except for the "SMILE" column
# cols_to_convert <- setdiff(colnames(final_df_descriptors_with_SMILES_Col_), "SMILES")
# 
# # convert the selected columns to numeric
# final_df_descriptors_with_SMILES_Col_n <- final_df_descriptors_with_SMILES_Col_ %>%
#   mutate_at(vars(cols_to_convert), as.numeric)

# Subset data frame to remove columns with any zero or null values
no_zero_df <- final_df_descriptors_with_SMILES_Col_[, apply(final_df_descriptors_with_SMILES_Col_ != 0 & !is.na(final_df_descriptors_with_SMILES_Col_), 2, all)]

# Print the new data frame
print(no_zero_df) # no_zero_df has 13  columns


# Subset data frame to remove columns with less than "~~~~~"  non-zero values
#no_zero_df_1450 <- final_df_descriptors_with_SMILES_Col_[, apply(final_df_descriptors_with_SMILES_Col_ != 0 & !is.na(final_df_descriptors_with_SMILES_Col_), 2, function(x) sum(x > 0) >= 1450)]

# Print the new data frame
#print(no_zero_df_1539)


# Count number of non-zero values in each column of final_df_descriptors_with_SMILES_Col_

nz_count <- apply(final_df_descriptors_with_SMILES_Col_, 2, function(x) sum(x != 0))
nz_count_df <- as.data.frame(nz_count) # Convert the result to a data frame
print(nz_count_df) # Print the data frame


# count the number of column having 1541 non-zero values 
# Remove rows with missing or NA values
nz_count_df <- na.omit(nz_count_df)

# Convert column to integers
#nz_count_df$nz_count <- as.integer(nz_count_df$nz_count)

# Check if there are any NA values left in the column
if (any(is.na(nz_count_df$nz_count))) {
  cat("WARNING: There are NA values in the 'nz_count' column.")
}

# Create a data frame by selecting specific non zero values from the "final_df_descriptors_with_SMILES_Col_ " data frame 

# Subset data frame to remove columns with less than "1532"  non-zero values == total 47 column selected 
no_zero_df_1532 <- final_df_descriptors_with_SMILES_Col_[, apply(final_df_descriptors_with_SMILES_Col_ != 0 & !is.na(final_df_descriptors_with_SMILES_Col_), 2, function(x) sum(x > 0) >= 1532)]
# write.csv(no_zero_df_1532, file = "no_zero_df_1532.csv", row.names = FALSE)

# Read 'no_zero_df_1532.csv' non zero rows with 47 columns 
no_zero_df_1532 <- read.csv('no_zero_df_1532.csv')

# Identify rows with no zero values
no_zero_rows <- apply(no_zero_df_1532 != 0, 1, all)

# Check if there are any NA values
any(is.na(no_zero_df_1532 ))

# Count the number of NA values in each column
na_count <- colSums(is.na(no_zero_df_1532))
na_count_df <- as.data.frame(na_count)

# Count the number of NA and zero values in each column
na_zero_or_NA_count <- colSums(is.na(no_zero_df_1532) | no_zero_df_1532 == 0)
na_zero_or_NA_count_df <- as.data.frame(na_zero_or_NA_count)

# Print the result
print(na_zero_count)


# Remove rows with zero or NA values
no_zero_na_df_1532 <- no_zero_df_1532[complete.cases(no_zero_df_1532), ]
#any(is.na(no_zero_na_df ))

# Count the number of NA values in each column
na_count <- colSums(is.na(no_zero_na_df_1532))
na_count_df <- as.data.frame(na_count)
print(no_zero_rows_df)

#write.csv(no_zero_na_df_1532, file = "no_zero_na_df_1532.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~!!!!!!!!!!!!!!!!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read no_zero_na_df_1532.csv data frame 
no_zero_na_df_1532 <- read.csv('no_zero_na_df_1532.csv')




##############################################################################





# count the number of zeros or null values in each column
col_counts <- colSums(is.na(final_df_descriptors_with_SMILES_Col_) | final_df_descriptors_with_SMILES_Col_ == 0)

# find the index of the column with no zero or null values
non_zero_col <- which(col_counts == 0)

# extract the column from the data frame
non_zero_col_data <- final_df_descriptors_with_SMILES_Col_[, non_zero_col]



#ddddddddddddddddd
























# Subset allmdescriptors to include only columns with no zero or null values
no_zeros_or_nulls <- subset(allmdescriptors, select = which(colSums(is.na(allmdescriptors) | allmdescriptors == 0) == 0))



# Count the number of zero or null values in every column of a data frame
zero_counts <- colSums(is.null(allmdescriptors) | is.na(allmdescriptors) | allmdescriptors == 0)
zero_counts_df <- data.frame(counts = zero_counts)# Convert zero_counts to a data frame
print(zero_counts_df)

# Subset allmdescriptors to include only columns with less than 500 zero or null values
new_df <- subset(allmdescriptors, select = zero_counts_df$counts < 100)
print(new_df)




# Print the new data frame
#print(no_zeros_or_nulls)
# write.csv(no_zeros_or_nulls, "no_zeros_or_nulls.csv", row.names = TRUE)




# merging merged_Smiles_zscores_empty_smiles_deleted & no_zeros_or_nulls

no_zeros_or_nulls_SMILES <- read.csv('no_zeros_or_nulls_SMILES_added_.csv')

merged_Smiles_zscores_empty_smiles_deleted_and_no_zeros_or_nulls_SMILES <- merge(merged_Smiles_zscores_empty_smiles_deleted,
                                                                          no_zeros_or_nulls_SMILES, by = "SMILES")





 







#@@@@#@ Step -9  @@@ :::: build the model where zscore is the function of descriptor 










#######################################........................................................................
#...............................................................................................................
#.,.,.,.,.,.,..........,,,,,,,,,,,.,,.,.,.,.,.,...,.,.,.......,,,,,,,,,,,,,,.,.,.,....,.,,.,.,.,.,.,..,.,.,.,.,.,.,
#@#@#@#@@@@@@@@@@@@@@@ Data checking @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Check the number of rows in the merged data frame
nrow(merged_df)

merged_agaain <- inner_join ( merged_df, fzscores, by = "APPID", multiple = "all")

# Read the two data frames
# uniqueAPPIDin_data_APPID_df <- read.csv("uniqueAPPIDin_data_APPID.csv")
# uniqueAPPID_in_fzscores_df <- read.csv("uniqueAPPID_in_fzscores.csv")



df <-  read.csv('zscore.csv')
unique__appid <- length(unique(df$APPID))
unique_studyid <- length(unique(df$STUDYID))
unique__trtdoslevel <- length(unique(df$TRTDOSLEVEL))


df1 <-  read.csv('merged_data_APPID.csv')
unique__appid <- length(unique(df1$APPID))




# Counting number of unique rows based on APPID column 
unique_abID1 <- read.csv('merged_data_APPID.csv')
num_unique_rows1 <- nrow(unique(unique_abID1["APPID"]))
num_unique_rows1
cat("Number of unique rows based on APPID:", num_unique_rows1, "\n") # print a message to the console


### 
unique_abID <- read.csv('abID041423.csv')
num_unique_rows <- nrow (unique(unique_abID["APPID"]))
num_unique_rows
cat("Number of unique rows based on APPID:", num_unique_rows, "\n")


# Check the datatype of TRTDOSLEVEL column
column_type <- sapply(df$TRTDOSLEVEL, class)
column_type <- unique(sapply(df$TRTDOSLEVEL, class))
column_type

## unique APPID and STUDYID
unique__appid <- length(unique(df$APPID))
unique_studyid <- length(unique(df$STUDYID))
unique__trtdoslevel <- length(unique(df$TRTDOSLEVEL))

# Check for missing values
sum(is.na(zscore_df$zscore))

# Check for non-numeric values
sum(!is.numeric(zscore_df$zscore))
# Get a vector of unique STUDYID values
unique_studies <- unique(df$STUDYID)

# get the unique values from the TRTDOSLEVEL column
unique_values <- unique(df_cleaned$TRTDOSLEVEL)
unique_values
df_unique_values <- data.frame(TRTDOSLEVEL = unique_values)

# See the differnce between the result_df vs max_values
diff <- setdiff(result_df$TRTDOSLEVEL, max_values$TRTDOSLEVEL)

df_diff <- result_df[result_df$TRTDOSLEVEL != max_values$TRTDOSLEVEL, ]
if (all(is.na(df_diff))) {
  print("All values in df_diff are NA.")
} else {
  print("There are non-NA values in df_diff.")
}

#Checking a row  contains a semicolon.
df2[df2$STUDYID == 17-1012 & grepl(";", df2$TRTDOSLEVEL), ]

# Checking whether a column contains a specific value
grepl("0\\.15-0\\.50", df2_keep $TRTDOSLEVEL)

# Count the number of TRUE and FALSE values
counts <- table(grepl("0\\.15-0\\.50", df2_keep $TRTDOSLEVEL))
print(counts)


# get the unique values from the TRTDOSLEVEL column
unique_values <- unique(df2$TRTDOSLEVEL)
unique_values
df_unique_values <- data.frame(TRTDOSLEVEL = unique_values)
write.csv(df_unique_values, file = "unique_values.csv", row.names = FALSE)


# Exclude rows that have more than one comma in TRTDOSLEVEL 
# or have a comma-separated value that contains another comma in it
# or have semicolon ';'
# or have colon ':'
# or have text in it

df2_discard <- subset(df2, grepl(",", TRTDOSLEVEL) 
                      | grepl(",,", TRTDOSLEVEL) 
                      | grepl(".*,[^,]*,.*", TRTDOSLEVEL) #matches any string that contains at least two commas separated by any number of non-comma characters
                      | grepl("[:;]", TRTDOSLEVEL) # search for the presence of a semicolon ; or colon : character
                      | grepl("[a-zA-Z]", TRTDOSLEVEL) # Any text
                      | TRTDOSLEVEL %in% c("0", "0.0", "0.00"))

# 1. TRTDOSLEVEL has only single or multiple commas, semicolons, or colons 
df2_withwired1 <- subset(df2, grepl("^([^,:;]*([,:;]|,[^,:;]+)+[^,:;]*)*$", TRTDOSLEVEL))

# 2. TRTDOSLEVEL has single or multiple comma, semicolon, or colon, or any letter (lowercase or uppercase)
df2_withwired2 <- subset(df2, grepl("[,:;a-zA-Z]", TRTDOSLEVEL) 
                         | grepl(",,", TRTDOSLEVEL) | grepl(".*,[^,]*,.*", TRTDOSLEVEL))

# 3.TRTDOSLEVEL column has only single or multiple commas, semicolons, or colons,
#as well as any letter or "zero" values

df2_with_wired3 <- subset(df2, grepl("^([0-9a-zA-Z,;:]*|0)$", TRTDOSLEVEL))

#4. TRTDOSLEVEL column has only single or multiple commas, semicolons, or colons, and does not
#contain any letters, "zero", "0.0", or "0.00",
df2_with_wired4 <- subset(df2, grepl("^[0-9:,;]+$", TRTDOSLEVEL) & !grepl("[a-zA-Z]+", TRTDOSLEVEL)
                          & !grepl("^(zero|0\\.0|0\\.00)$", TRTDOSLEVEL))
write.csv(df2_discard, "df2_discard.csv", row.names = FALSE)

# Exclude rows and create new data frame
df2_keep <- df2[!rownames(df2) %in% rownames(df2_discard), ]


# # Get a list of unique study IDs
# study_ids <- unique(df$STUDYID)
# 
# # Create an empty data frame to store the results
# max_values <- data.frame(STUDYID = numeric(), TRTDOSLEVEL = character())
# 
# # Loop over each unique study ID
# for (id in study_ids) {
#   
#   # Subset the data frame to include only rows with the current study ID
#   subset_df <- df[df$STUDYID == id, ]
#   
#   # Get the highest TRTDOSLEVEL value for the current study ID
#   max_value <- max(as.numeric(subset_df$TRTDOSLEVEL), na.rm = TRUE)
#   
#   # Create a data frame with the current study ID and max TRTDOSLEVEL value
#   max_values_row <- data.frame(STUDYID = id, TRTDOSLEVEL = max_value)
#   
#   # Append the row to the results data frame
#   max_values <- rbind(max_values, max_values_row)
# }

# # View the results
# max_values

#checking the unique values
unique_values_studyid <- length(unique(df2$STUDYID))
unique_values_appid <- length(unique(df2$APPID))
unique_values_trtdoslevel <- length(unique(df2$TRTDOSLEVEL))

# Count total rows with single comma
count_single_comma <- sum(grepl(",", df2$TRTDOSLEVEL) & !grepl(",,", df2$TRTDOSLEVEL))
print(count_single_comma)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ZSCORES calculations based on grouping on STUDYID,TSSPECIES, TRTDOSLEVEL
df2_grouped <- df2 %>%
  group_by(STUDYID,TSSPECIES, TRTDOSLEVEL ) %>%
  summarize(df2mean_BWSTRESN = mean(BWSTRESN),
            df2sd_BWSTRESN = sd(BWSTRESN))

joined_df <- inner_join(df1_grouped, df2_grouped, by = c("STUDYID"), multiple = "all")

zscore_df <- joined_df %>%
  mutate(zscore = (df2mean_BWSTRESN - df1mean_BWSTRESN) / df1sd_BWSTRESN)
print(zscore_df)
Zscores <- zscore_df %>% select(STUDYID, USUBJID, EXROUTE, SEX, BWTESTCD, SETCD, zscore)

write.csv(Zscores, "Zscores_oSTUDYID.csv", row.names = FALSE)

#.................................................................................................
#..................................................................................................





######################################################.........................................................
--- IND147206 (export) 
SELECT  ID.APPID, ID.STUDYID, DS.DSDECOD, DM.SEX,DM.SPECIES, BW.USUBJID, BW.BWTESTCD, BW.BWSTRESN, BW.BWSTRESU, TS.TSVAL AS TSSPECIES, TX.TXVAL AS TRTDOSLEVEL
FROM (((((ID INNER JOIN DS ON (DS.STUDYID = ID.STUDYID AND DS.USUBJID = DM.USUBJID ) )
         INNER JOIN TS ON TS.STUDYID = ID.STUDYID)
        INNER JOIN TX ON (TX.STUDYID = ID.STUDYID AND TX.SETCD = DM.SETCD ) )
       INNER JOIN DM ON DM.STUDYID = ID.STUDYID)
      INNER JOIN BW ON (BW.STUDYID = ID.STUDYID AND BW.USUBJID= DM.USUBJID))
WHERE TX.TXPARMCD == "TRTDOS" AND DM.SEX == "M" AND TS.TSPARMCD LIKE '%SPECIES%' AND DS.DSDECOD == "TERMINAL SACRIFICE" AND BW.BWTESTCD == "TERMBW";--0 AND ID.APPID == "IND147206"; 

--- IND147206 (export)nc 
SELECT ID.APPID, ID.STUDYID, DS.DSDECOD, DM.SEX,DM.SPECIES, BW.USUBJID, BW.BWTESTCD, BW.BWSTRESN, BW.BWSTRESU, TS.TSVAL AS TSSPECIES, TX.TXVAL AS TRTDOSLEVEL, TX.TXPARMCD
FROM (((((ID INNER JOIN DS ON (DS.STUDYID = ID.STUDYID AND DS.USUBJID = DM.USUBJID ) )
         INNER JOIN TS ON TS.STUDYID = ID.STUDYID)
        INNER JOIN TX ON (TX.STUDYID = ID.STUDYID AND TX.SETCD = DM.SETCD ) )
       INNER JOIN DM ON DM.STUDYID = ID.STUDYID)
      INNER JOIN BW ON (BW.STUDYID = ID.STUDYID AND BW.USUBJID= DM.USUBJID))
WHERE TX.TXPARMCD == "TCNTRL" AND DM.SEX == "M" AND TS.TSPARMCD LIKE '%SPECIES%' AND DS.DSDECOD == "TERMINAL SACRIFICE" AND BW.BWTESTCD == "TERMBW";

# --Animal BW_0413023
# SELECT DISTINCT BW.STUDYID, BW.USUBJID,BW.BWTESTCD,BW.BWSTRESN, BW.BWSTRESU,EX.EXROUTE, DM.SEX  --, DM.ARMCD,DM.SETCD,TX.SETCD AS TXsetcd, TX.TXPARMCD
# FROM ((((BW INNER JOIN EX ON BW.USUBJID = EX.USUBJID)
#         INNER JOIN DM ON DM.USUBJID=BW.USUBJID)
#        INNER JOIN TX ON DM.STUDYID = TX.STUDYID) 
#       INNER JOIN ID ON DM.STUDYID = ID.STUDYID)
# WHERE TX.TXPARMCD == "TRTDOS" AND BW.BWTESTCD == "TERMBW" AND DM.STUDYID == TX.STUDYID AND DM.SETCD == TX.SETCD;

# ---NCBW_04122023
# SELECT DISTINCT BW.STUDYID, BW.USUBJID,BW.BWTESTCD,BW.BWSTRESN, BW.BWSTRESU,EX.EXROUTE, DM.SEX, DM.ARMCD,DM.SETCD,TX.SETCD AS TXsetcd, TX.TXPARMCD
# FROM (((BW INNER JOIN EX ON BW.USUBJID = EX.USUBJID)
#        INNER JOIN DM ON DM.USUBJID=BW.USUBJID)
#       DM INNER JOIN TX ON DM.STUDYID = TX.STUDYID) 
# WHERE TX.TXPARMCD == "TCNTRL" AND BW.BWTESTCD == "TERMBW" AND DM.STUDYID == TX.STUDYID AND DM.SETCD == TX.SETCD;
#..............................................................................................................
################################################
# --Animal BW_0414023
# SELECT DISTINCT BW.STUDYID, BW.USUBJID,BW.BWTESTCD,BW.BWSTRESN, BW.BWSTRESU,EX.EXROUTE, DM.SEX, DM.SETCD,DM.SPECIES,ID.APPID  --, DM.ARMCD,,TX.SETCD AS TXsetcd, TX.TXPARMCD
# FROM ((((BW INNER JOIN EX ON BW.USUBJID = EX.USUBJID)
#         INNER JOIN DM ON DM.USUBJID=BW.USUBJID)
#        INNER JOIN TX ON DM.STUDYID = TX.STUDYID) 
#       INNER JOIN ID ON DM.STUDYID = ID.STUDYID)
# --INNER JOIN TS ON BW.STUDYID = TS.STUDYID)
# WHERE TX.TXPARMCD == "TRTDOS" AND BW.BWTESTCD == "TERMBW" AND DM.STUDYID == TX.STUDYID AND DM.SETCD == TX.SETCD AND DM.STUDYID == ID.STUDYID ;

#........................
# ---NCBW_04142023
# SELECT DISTINCT BW.STUDYID, BW.USUBJID,BW.BWTESTCD,BW.BWSTRESN, BW.BWSTRESU,EX.EXROUTE, DM.SEX, DM.ARMCD,DM.SETCD,TX.SETCD AS TXsetcd, TX.TXPARMCD, ID.APPID
# FROM ((((BW INNER JOIN EX ON BW.USUBJID = EX.USUBJID)
#         INNER JOIN DM ON DM.USUBJID=BW.USUBJID)
#        INNER JOIN TX ON DM.STUDYID = TX.STUDYID) 
#       INNER JOIN ID ON DM.STUDYID = ID.STUDYID)
# WHERE TX.TXPARMCD == "TCNTRL" AND BW.BWTESTCD == "TERMBW" AND DM.STUDYID == TX.STUDYID AND DM.SETCD == TX.SETCD AND DM.STUDYID == ID.STUDYID;
##############################


