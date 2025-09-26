rm(list = ls())
library(rcdk)
library(rJava)
library(Rcpi)

#Selection of the SMILES column from the csv file 
setwd('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI')
readcsv <- read.csv('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI/IND_mappings.csv')
allsmiles <- readcsv$SMILES

# Write the a csv file containing only SMILES
write.csv(allsmiles, file = "IND_mappings_smiles.csv", row.names = FALSE)

readallsmilescsv <- read.csv('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI/IND_mappings_smiles.csv') 


#Convert the csv data to a Molecule object using the parse.smiles() function from the rcdk package
# parse.smiles convert the smles to a mol object

molecules_obj <- sapply(readallsmilescsv$smiles, parse.smiles) 

# Calculate selected molecular descriptors using the Molecule object
mdescriptors <- suppressWarnings(cbind(
  extractDrugALOGP(molecules_obj),
  extractDrugApol(molecules_obj),
  extractDrugECI(molecules_obj),
  extractDrugTPSA(molecules_obj),
  extractDrugWeight(molecules_obj),
  extractDrugWienerNumbers(molecules_obj),
  extractDrugZagrebIndex(molecules_obj)
))
mdescriptors

# export the dataframe to a CSV file
write.csv(mdescriptors, file = "mdescriptors.csv")

# Calculate all molecular descriptors using the Molecule object

allmdescriptors <- extractDrugAIO(molecules_obj, silent = TRUE, warn = TRUE)
 #......................................................
#.........................................................
#......................................................

# write the .smi files
#write.table(sapply(mols, toString), file = "IND_mappings_smiles.smi", sep = "\t", col.names = FALSE, quote = FALSE, row.names = FALSE)

# Read from the .smi file
#x.mol <- readMolFromSmi(IND_mappings_smiles.smi, type = "mol")

smis <- "C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI/IND_mappings_smiles.smi"
x.mol  = readMolFromSmi(smis, type = "mol")


#selects all the rows that have nonzero or not null values
nonzero_rows <- subset(allsmiles, apply(allsmiles, 1, function(x) any(x != 0 & !is.na(x))))











# Load the rcpi package
library(rJava)
library(Rcpi)
setwd('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI')
#x.mol <- readMolFromSmi(RI.smi, type = "mol")
# x.mol <- readMolFromSDF () 
# x.mol <- readMolFromSDF(IND_mappings, type = "sdf")

#sdf  = system.file('compseq/DB00859.sdf', package = 'Rcpi')

sdf <- "C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/RCPI/IND_mappings.sdf"
x.mol  = readMolFromSDF(sdf)
# Calculate selected molecular descriptors
x <- suppressWarnings(cbind(
  extractDrugALOGP(x.mol),
  extractDrugApol(x.mol),
  extractDrugECI(x.mol),
  extractDrugTPSA(x.mol),
  extractDrugWeight(x.mol),
  extractDrugWienerNumbers(x.mol),
  extractDrugZagrebIndex(x.mol)
))
 #........................................................................

library("Rcpi")


RI.csv <- system.file(
  "vignettedata/IND_mappings.csv",
  package = "Rcpi"
)

x.mol <- readMolFromSmi(RI.smi, type = "mol")
