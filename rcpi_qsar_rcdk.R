rm(list = ls())
library("Rcpi")
library("rJava")

# Molecular Descriptor by Rcdk package
library(readr)
library("rcdk")

setwd('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/rcdk')
readcsv <- read.csv('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/rcdk/IND_mappings.csv')
allsmiles <- readcsv$SMILES

smiles <- allsmiles

# generate descriptors for each molecule
my_molecules <- parse.smiles(smiles)
my_desc <-eval.desc(my_molecules,which.desc = 'all', verbose = FALSE)

my_desc <- lapply(my_molecules, generate.desc, desc.type = "all")







# detect separator used in the file
separator <- guess_separator("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/rcdk/RI.smi")

# read file using the detected separator
smiles_df <- read_table("path/to/your/file.smi", col_names = FALSE, col_types = "c", sep = separator)

# extract SMILES strings from the data frame
smiles <- smiles_df$X1

# generate descriptors for each molecule
my_molecules <- parse.smiles(smiles)
my_desc <- lapply(my_molecules, generate.desc, desc.type = "all")














#set up your Java classpath to include the CDK library
.jinit()
.jaddClassPath("/path/to/cdk.jar")


IND_mappings  <- read.csv('IND_mappings .csv')
IND_mappings

xmol <- IND_mappings$SMILES
mmol_list <- strsplit(xmol, ",")
mmol_list

# jobjRef object creation:
j_array <- jarray(unlist(mmol_list), dim=c(length(mmol_list),1))



# Calculate selected molecular descriptors
descriptors <- suppressWarnings(cbind(
  extractDrugALOGP(mmol_list),
  extractDrugApol(mmol_list),
  extractDrugECI(mmol_list),
  extractDrugTPSA(mmol_list),
  extractDrugWeight(mmol_list),
  extractDrugWienerNumbers(mmol_list),
  extractDrugZagrebIndex(mmol_list)
))
descriptors
## Molecular Descriptor by ChemmineR package
# using  "ChemmineR"
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ChemmineR")

library(ChemmineR)
setwd('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ChemmineR')
#mols <- read.smi('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ChemmineR/RI.smi')

my_smiset <- read.SMIset("RI.smi", removespaces = TRUE)

descs <- desc(my_smiset )

## ## Molecular Descriptor by rcdk package

