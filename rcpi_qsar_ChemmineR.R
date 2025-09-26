rm(list = ls())
## Having problem with ChemmineR in cmp.parse(sdfset)
library("ChemmineR")

setwd('C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ChemmineR')

sdfset <- read.SDFset("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ChemmineR/sdfsample.sdf") 
sdfset <- read.SDFset("C:/Users/mdaminulisla.prodhan/All_My_Miscellenous/ChemmineR/IND_mappings.sdf") 

parsing <- cmp.parse(sdfset)

valid <- validSDF(sdfset)
sdfset <- sdfset[valid]

apset <- sdf2ap(sdfset) # Generate atom pair descriptor database for searching 
db <- apset
save(db, file="db.rda", compress=TRUE)

cmp.search(db, db[1], type=3, cutoff = 0.3, quiet=TRUE) # Search apset database with single compound. 


library(help="ChemmineR")
sdfset <- read.SDFset("http://faculty.ucr.edu/~tgirke/Documents/ R_BioCond/Samples/sdfsample.sdf")

 ????install.packages('ChemmineOB')






















