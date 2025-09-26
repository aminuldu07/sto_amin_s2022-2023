library(sendigR)
library(this.path)
library("tidyverse")

homedir <- dirname(this.path())
               setwd(homedir)
fullpath <- path.expand("C:/Users/mdaminulisla.prodhan/Documents/sendigR-main/tests/data/test_db.db")

db <- initEnvironment(dbType='sqlite',dbPath= fullpath)
sendigR::execSendDashboard(db)
execSendDashboard(db)


# Extract the relevant set of studies into data.table 'studies'
studies <-
  getStudiesSDESIGN(db,
                    studyDesignFilter = 'PARALLEL',
                    inclUncertain = TRUE) %>%
  getStudiesSTSTDTC(db, ., 
                    fromDTC = '2016',
                    inclUncertain = TRUE)

# Extract the complete set of control animals for the set of extracted studies 
# into data.table 'controlAnimals':
controlAnimals <-
  getControlSubj(db, studies,
                 inclUncertain = TRUE)

# Extract all 
#  1: the males
#  2: Sprague-Dawley rats
#  3: animals dosed by oral or oral gavage
# from the set of control animals into data.table 'animals':
animals <- 
  getSubjSex(db, controlAnimals, 
             sexFilter = 'M',
             inclUncertain = TRUE) %>%
  getSubjSpeciesStrain(db, .,
                       speciesFilter = 'RAT',
                       strainFilter = 'Sprague-Dawley',
                       inclUncertain = TRUE) %>%
  getSubjRoute(db, .,
               routeFilter = c('ORAL', 'ORAL GAVAGE'),
               inclUncertain = TRUE)

# Extract microscopic findings
#  1: all MI rows for the extrated set of animals
#  2: keep the MI rows from the treatment phase
#  3: keep the MI rows where the animals are between 10 and 14 weeks at the time 
#     of finding
# into data.table 'dataMI':
dataMI <- 
  getSubjData(db, animals, 'MI') %>%
  getFindingsPhase(db, .,
                   phaseFilter = "treatment",
                   inclUncertain = TRUE) %>%
  getFindingsSubjAge(db, .,
                     fromAge = '10w',
                     toAge = '14w',
                     inclUncertain = TRUE)

disconnectDB(db)