# remove objects from the environment
rm(list = ls())
#selected_studies <- "20098018"

#Packages
library(dplyr)
library(sendigR)
library(tidyverse)
library(this.path)

#Set File Path to Biocelerate Data
homePath <- dirname(this.path())
setwd(homePath)

#Database Load
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = paste0(homePath,"/TestDB.db"),
                                    dbCreate = FALSE)
#RptDoseStudyID <- sendigR::getStudiesSDESIGN(dbtoken,studyDesignFilter = "NULL")
RptDoseStudyID <- sendigR::getStudiesSDESIGN(dbtoken,studyDesignFilter = "PARALLEL")
MIStudyIDS <- sendigR::genericQuery(dbtoken, "SELECT STUDYID FROM MI") #limiting to MI because it is the least likely to be filled
dbStudyIDs <- intersect(RptDoseStudyID$STUDYID,MIStudyIDS$STUDYID)
dbStudyIDS <- unique(dbStudyIDs)

#Pull INDs and Study Titles for those StudyIDs
APPID <- sendigR::genericQuery(dbtoken, 'SELECT * FROM ID WHERE STUDYID in (:1)', dbStudyIDs) # (:1) parameter placeholder
STITLE <- sendigR::genericQuery(dbtoken, 'SELECT STUDYID, TSVAL FROM TS WHERE TSPARMCD = "STITLE" and STUDYID in (:1)', dbStudyIDs)
dbStudys <- merge(APPID, STITLE, by = "STUDYID")
dbStudys$CombinedName <- paste0(dbStudys$APPID,"-",dbStudys$STUDYID,": ",dbStudys$TSVAL)


# Sex variable 
sexes <- c('M', 'F')

#Standardizing Terminology
organSystems <- c('LIVER', 'KIDNEY')

MITESTCDlist <- list('LIVER' = c('LIVER'),
                     'KIDNEY' = c('KIDNEY'))

organTESTCDlist <- list('LIVER' = c('SERUM | ALT',
                                    'SERUM | AST',
                                    'SERUM | ALP',
                                    'SERUM | GGT',
                                    'SERUM | BILI',
                                    'SERUM | ALB'),
                        'KIDNEY' = c('SERUM | CREAT',
                                     'SERUM | UREAN',
                                     'SERUM | ALB',
                                     'SERUM | CL',
                                     'SERUM | K',
                                     'SERUM | PHOS',
                                     'SERUM | SODIUM',
                                     'URINE | CL',
                                     'URINE | K',
                                     'URINE | SODIUM',
                                     'URINE | GLUC',
                                     'URINE | SPGRAV',
                                     'URINE | VOLUME',
                                     'URINE | PROT',
                                    'URINE | UROBIL'))
# DOSE Ranking 
doseRanks <- c('Vehicle', 'LD', 'MD', 'HD')

# Convert concatenated names back to STUDYIDs
DatabaseStudies <- dbStudys$STUDYID 


# Find the position of "00963046" in DatabaseStudies
position <- which(DatabaseStudies == "20098018")

# Check if it exists in the vector
if (length(position) > 0) {
  # Randomly select one value from DatabaseStudies
  random_studies <- DatabaseStudies[position]
} else {
  # "00963046" doesn't exist in the vector
  random_studies <- NULL  # or any other appropriate value
}


# Create another variable to hold these selected values
selected_studies <- random_studies

# Find number of StudyIDs
numstudies <- length(selected_studies)

for (j in 1:numstudies){
  Name <- paste0('SENDStudy',as.character(j))
  #Pull relevant domain data for each domain
  bw <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM BW WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  dm <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM DM WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  ex <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM EX WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  fw <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM FW WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  lb <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM LB WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  mi <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM MI WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  om <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM OM WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  ts <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TS WHERE STUDYID = (:1)",
                              queryParams = selected_studies[j])
  ta <-sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TA WHERE STUDYID = (:1)",
                             queryParams = selected_studies[j])
  pooldef <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM POOLDEF WHERE STUDYID = (:1)",
                                   queryParams = selected_studies[j])
  #Combine into list of assigned name
  assign(Name, list('bw' = bw, 'dm' = dm,'ex' = ex, 'fw' = fw, 'pooldef'=pooldef,
                    'lb' = lb, 'mi' = mi, 'om'=om, 'ts'=ts, 'ta'=ta))
  print(paste0(Name, " = ", selected_studies[j]))
} 


#Check that SEND studies loaded are repeat-dose and have 4 doses (vehicle to high-dose)
non_repeat_dose_studies <- character(0)  # Initialize an empty character vector
repeat_dose_studies <- character(0)  # Initialize an empty character vector
for (nj in 1:numstudies){
  Name <- paste0('SENDStudy',as.character(nj))
  
  #Get Study Type
  SSTYP <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "SSTYP")]
  
  #Check if SSTYP is not Repeat Dose Toxicity
  if (!any(SSTYP %in% c("REPEAT DOSE TOXICITY", "REPEAT-DOSE TOXICITY",
                        "Repeat-Dose Toxicity", "Repeat Dose Toxicity"))){
    message(paste0("Study ID ", nj, " is not a Repeat-Dose Toxicity Study"))
    non_repeat_dose_studies <- c(non_repeat_dose_studies, Name)  # Add the name to the vector
  }else {
    repeat_dose_studies <- c(repeat_dose_studies, Name)  # Add the name to the repeat dose studies vector
    message(paste0("Study ID ", nj, " is  a Repeat-Dose Toxicity Study"))
  }
}


################################ #####   Load in Data (591-)    #############################################

# Create a data frame to hold CompileData
CompileData <- data.frame(StudyID = NA, Species = NA, USUBJID = NA, SEX = NA, ARMCD = NA)
SENDStudyLookup <- data.frame(Name = NA, StudyID = NA)

for (nj in 1:numstudies){
  Name <- paste0('SENDStudy',as.character(nj))
  #Pull all of the relevant DM Data
  Species <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "SPECIES")]
  TRTName <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "TRT")]
  Duration <-get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "DOSDUR")]
  DelMethod <-get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "ROUTE")]
  #Convert duration to days
  if (grepl("W",Duration) ==TRUE){
    days <- as.numeric(gsub("\\D","",Duration))*7
  } else if (grepl("M",Duration) == TRUE){
    days <- as.numeric(gsub("\\D","",Duration))*7*30
  } else {
    days <- as.numeric(gsub("\\D","",Duration))
  }
  Duration <- paste0(days,"D")
  #Make StudyID
  StudyID <- paste0(Species, " ", TRTName," ",DelMethod," ",Duration)
  SENDStudyLookup <- rbind(SENDStudyLookup, c(Name,StudyID))
  DMData <- data.frame(StudyID = rep(StudyID, length(get(Name)$dm$USUBJID)), # creating DM DATA 
                       Species = rep(Species, length(get(Name)$dm$USUBJID)),
                       USUBJID = get(Name)$dm$USUBJID,
                       SEX = get(Name)$dm$SEX,
                       ARMCD = get(Name)$dm$ARMCD)
  #Remove T from ARMCD for groups that include it for "treatment"
  DMData$ARMCD <- gsub('T','',DMData$ARMCD)
  #Add to CompileData
  CompileData <- rbind(CompileData, DMData)
}

# Remove NAs from the first line
CompileData <- na.omit(CompileData)

compilexx <- CompileData 
# needdeda <-  CompileData
#
#
# #Remove Recovery Animals and Recode Treatment Ranks
AllData <- CompileData


CompileData <- CompileData[!str_detect(CompileData$ARMCD, "R"),]
CompileData$ARMCD <- as.numeric(CompileData$ARMCD)
CompileData$ARMCD <- factor(CompileData$ARMCD) # why agian converting back to categorical values
#
# # # Again filtering to ensure the specific values of ARMCD are retained.
# if (length(unique(CompileData1$ARMCD))>4){
#   #error catch for accidental inclusion of other arm numbers
#   CompileData1 <- CompileData1[which(CompileData1$ARMCD %in% c(1,2,3,4)),]
#   CompileData1$ARMCD <- as.factor(as.numeric(CompileData1$ARMCD))
# }
#


# # custom levels for the DOSING in ARMCD
# levels(CompileData1$ARMCD) <- doseRanks

##################### Organ System Specific Graphs ######################(1041-1125)

# LB data gathering.................................................................

for (organSystem in organSystems) {
  #Make LB Data Data Frame to Hold Information
  LBData <- data.frame("USUBJID" = NA,"LBSPEC" = NA,"LBTESTCD" = NA,
                       "LBSTRESN" = NA, "VISITDY" = NA)
  for (dd in repeat_dose_studies){
    Name <- dd
    
    #Pull all of the relevant LB Data
    if ("LBDY" %in% colnames(get(Name)$lb) == TRUE){
      LBD <- get(Name)$lb[which((get(Name)$lb$LBDY >= 1)),
                          c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "LBDY")]
      colnames(LBD) <- c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")
      if (all(is.na(LBD$LBSPEC)) == TRUE){
        
        #Convert LBCAT to LBSPEC if no LBSPEC
        LBD$LBSPEC <- get(Name)$lb[which((get(Name)$lb$LBDY >= 1)),
                                   c("LBCAT")]
        if (any(c("HEMATOLOGY", "Hematology","hematology") %in% levels(LBD$LBSPEC))){
          levels(LBD$LBSPEC)[match(c("HEMATOLOGY", "Hematology","hematology"),
                                   levels(LBD$LBSPEC))] <- "WHOLE BLOOD"
        }
        if (any(c("CLINICAL CHEMISTRY","Clinical Chemistry") %in% levels(LBD$LBSPEC))){
          levels(LBD$LBSPEC)[match(c("CLINICAL CHEMISTRY","Clinical Chemistry"),
                                   levels(LBD$LBSPEC))] <- "SERUM" 
        }
        if (any(c("URINALYSIS","Urinalysis") %in% levels(LBD$LBSPEC))){
          levels(LBD$LBSPEC)[match(c("URINALYSIS","Urinalysis"),
                                   levels(LBD$LBSPEC))] <- "URINE" 
        }
      }
    } else {
      LBD <- get(Name)$lb[which((get(Name)$lb$VISITDY >= 1)),
                          c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")] 
    }
    #Add to CompileData
    LBData <- rbind(LBData, LBD)
  }
  LBData <- na.omit(LBData)
}
# LBData_without_NA <- na.omit(LBData)
# LBData_with_NA <- LBData[!complete.cases(LBData), ]



# just a back up copy
lbdadaaaaaaa <- LBData

# Concatenate LBSPEC and LBTESTCD
LBData$LBTESTCD <- paste(LBData$LBSPEC, LBData$LBTESTCD, sep = ' | ')

#Remove Not Included Tests
organIndex <- which(LBData$LBTESTCD %in% organTESTCDlist[[organSystem]])
LBData <- LBData[organIndex,] # This step remove not rows macthing test from ogransystem

# LBDatafinal <- LBData

# Identify Unique Animals for Recovery Analysis: (Make list of Recovery Animals)
RecoveryAnimals<-unique(subset(LBData$USUBJID, !(LBData$USUBJID %in% CompileData$USUBJID)))
RecoveryAnimals

#Find Final Day for Before Recovery for Recovery Animals
RecovData <- LBData[which(LBData$USUBJID %in% RecoveryAnimals),]

# RecovDataIntermediate <- LBData[which(LBData$USUBJID %in% RecoveryAnimals),]
# print(nrow(RecovDataIntermediate))  # To print the number of rows after this step
# 

# Filter LBData for Recovery Animals and Visit Days (filter the RecovData data frame)
RecovData <- RecovData[which(RecovData$VISITDY < 40),] # why < 40 days of VISITDY

# RecovData <- RecovDataIntermediate[which(RecovDataIntermediate$VISITDY < 40),]
# print(nrow(RecovData))  # To print the number of rows after filtering by visit days


# Analysis of Test Days:(iterates through each unique subject ID in LBData and
  # then through each unique test code (LBTESTCD) for that subject )

FinalDays <- NA
for (indv in unique(LBData$USUBJID)){
  indvtests <- LBData[which(LBData$USUBJID == indv), "LBTESTCD"]
  for (TEST in unique(indvtests)){
    if (indv %in% RecoveryAnimals){
      Indv <- which(RecovData$USUBJID == indv)
      IndvData <- RecovData[Indv,]
      IndvData <- IndvData[which(IndvData$LBTESTCD == TEST),]
      maxday <- suppressWarnings(max(IndvData$VISITDY))
    } else {
      Indv <- which(LBData$USUBJID == indv)
      IndvData <- LBData[Indv,]
      IndvData <- IndvData[which(IndvData$LBTESTCD == TEST),]
      maxday <- suppressWarnings(max(IndvData$VISITDY))
    }
    LastTest <- which(LBData$USUBJID == indv & LBData$VISITDY == maxday)
    FinalDays <- append(FinalDays, LastTest)
  }
}

FinalDays <- unique(FinalDays) #Removes accidentally created duplicates in FINAL DAYS
LBDatafinal <- LBData[FinalDays, c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN")]

#.........................................................................................................
#.................... scoring section................................................

#Calculate Z Score per LBTESTCD
# Merge with unique rows from AllData
LBDatafinalm <- merge(LBDatafinal, unique(AllData[, c("USUBJID", "ARMCD", "StudyID", "SEX")]), by = "USUBJID")

LBDatafinal111 <- LBDatafinalm

# Find Levels of Study
LBDatafinalm$ARMCD <- factor(LBDatafinalm$ARMCD) 
ARMS <- levels(LBDatafinalm$ARMCD)
ARMS <- gsub("R","",ARMS) # removing R # ARMCD column has to be factor


levels(LBDatafinalm$ARMCD) <- ARMS #Remove leading 0s

levels(LBDatafinalm$ARMCD)[levels(LBDatafinalm$ARMCD) == "01"] <- "1"
levels(LBDatafinalm$ARMCD)[levels(LBDatafinalm$ARMCD) == "02"] <- "2"
levels(LBDatafinalm$ARMCD)[levels(LBDatafinalm$ARMCD) == "03"] <- "3"
levels(LBDatafinalm$ARMCD)[levels(LBDatafinalm$ARMCD) == "04"] <- "4"

# assigning Dose rank 
levels(LBDatafinalm$ARMCD) <- doseRanks


# Zscore calculation 
LBDatafinalm$zscore <- NA
for (sex in sexes) {
  ## Restrict Gender of compile data based on sex
  GenderData <- LBDatafinalm[which(LBDatafinalm$SEX == sex),]
  for (study in unique(LBDatafinalm$StudyID)){
    StudyData <- GenderData[which(GenderData$StudyID == study),]
    for (TEST in unique(StudyData$LBTESTCD)){
      TESTData <- StudyData[which(StudyData$LBTESTCD == TEST),]
      ControlTESTData <- TESTData[which(TESTData$ARMCD == "Vehicle"),]
      SIdx <- which(LBDatafinalm$StudyID == study)
      SIdx <- intersect(SIdx, which(LBDatafinalm$SEX == sex))
      index <- intersect(which(LBDatafinalm$LBTESTCD == TEST),SIdx)
      LB.mean.C <- mean(ControlTESTData$LBSTRESN, na.rm = TRUE)
      LB.sd.C <- sd(ControlTESTData$LBSTRESN, na.rm = TRUE)
      if (is.na(LB.sd.C) == TRUE){
        #print(paste0(TEST, "For ", study, sex))
      }
      LBDatafinalm$zscore[index] <- (LBDatafinalm$LBSTRESN[index]- LB.mean.C)/LB.sd.C
    }
  }
}


####################################### Scoring Portion #########################################################





