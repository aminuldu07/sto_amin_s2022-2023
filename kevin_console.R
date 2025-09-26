ID <- read.csv('ID-DataCentral.csv')
SMILES <- read.csv('merged_on_Approval_ID.csv')
ID_NDAs <- grep('^NDA', ID$APPID, value = T)
SMILES_NDAs <- grep('^NDA', SMILES$APPID, value = T)
length(ID_NDAs)
head(ID_NDAs)
ID_NDAs <- unique(ID_NDAs)
SMILES_NDAs <- unique(SMILES_NDAs)
length(ID_NDAs)
length(SMILES_NDAs)
head(SMILES)
blankIndex <- which(SMILES$SMILES == '')
length(blankIndex)
SMILES <- SMILES[-blankIndex,]
SMILES_NDAs <- grep('^NDA', SMILES$APPID, value = T)
SMILES_NDAs <- unique(SMILES_NDAs)
length(SMILES_NDAs)
length(ID_NDAs)
Overlap <- which(ID_NDAs %in% SMILES_NDAs)
length(Overlap)
Overlap
Overlap <- ID_NDAs[which(ID_NDAs %in% SMILES_NDAs)]
Overlap
NDAstudies <- ID$STUDYID[which(ID$APPID %in% Overlap
)]
head(NDAstudies)
studyIndex <- which(ID$APPID %in% Overlap)
studyIndex <- which(ID$STUDYID %in% NDAstudies)
for (study in NDAstudies) {studyIndex <- which(ID$STUDYID == study); apps <- unique(ID$APPID[studyIndex]; if (length(grep('^IND', apps)) == 0) {print(study)})}
for (study in NDAstudies) {studyIndex <- which(ID$STUDYID == study); apps <- unique(ID$APPID[studyIndex]); if (length(grep('^IND', apps)) == 0) {print(study)})}
for (study in NDAstudies) {studyIndex <- which(ID$STUDYID == study); apps <- unique(ID$APPID[studyIndex]); if (length(grep('^IND', apps)) == 0) {print(study)}}
noINDstudies <- NULL; for (study in NDAstudies) {studyIndex <- which(ID$STUDYID == study); apps <- unique(ID$APPID[studyIndex]); if (length(grep('^IND', apps)) == 0) {noINDstudies <- c(noINDstudies, study)}}
length(noINDstudies)
studyIndex <- which(ID$STUDYID %in% noINDstudies)
ID[studyIndex,]
NDAnoIND <- ID[studyIndex, 'APPID']
NDAnoIND <- unique(ID[studyIndex, 'APPID'])
length(NDAnoIND)
NDAnoINDstudies <- ID$STUDYID[which(ID$APPID %in% NDAnoIND)]
ID[which(ID$STUDYID %in% NDAnoINDstudies),]
NDAnoIND
for (NDA in NDAnoIND) {studies <- ID$STUDYID[which(ID$APPID == NDA); apps <- ID$APPID[which(ID$STUDYID %in% studies)]; if (length(grep('^IND', apps)) == 0) {print(NDA)}]}
for (NDA in NDAnoIND) {studies <- ID$STUDYID[which(ID$APPID == NDA); apps <- ID$APPID[which(ID$STUDYID %in% studies)]; if (length(grep('^IND', apps)) == 0) {print(NDA)}}
for (NDA in NDAnoIND) {studies <- ID$STUDYID[which(ID$APPID == NDA)]; apps <- ID$APPID[which(ID$STUDYID %in% studies)]; if (length(grep('^IND', apps)) == 0) {print(NDA)}}
realNDAnoIND <- NULL; for (NDA in NDAnoIND) {studies <- ID$STUDYID[which(ID$APPID == NDA)]; apps <- ID$APPID[which(ID$STUDYID %in% studies)]; if (length(grep('^IND', apps)) == 0) {realNDAnoIND <- c(realNDAnoIND, NDA)}}
length(realNDAnoIND)
ID[which(ID$APPID %in% realNDAnoIND),]
dim(ID[which(ID$APPID %in% realNDAnoIND),])