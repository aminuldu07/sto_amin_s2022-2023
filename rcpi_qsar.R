rm(list = ls())
library("Rcpi")
library("rJava")

# Molecular Descriptor by Rcpi package
library("Rcpi")

setwd('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI')
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

