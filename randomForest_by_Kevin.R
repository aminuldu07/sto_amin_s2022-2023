rm(list =ls())

library(this.path)
library(randomForest)
library(caret)
# library(reprtree)
library(ROCR)

homeDir <- dirname(this.path())
setwd(homeDir)

`%ni%` <- Negate('%in%')

Impute <- T
ErrorMethod <- 'Prune' # Choose: "Flip" or "Prune" or "None"
Undersample <- T
reps <- 0
rfDataRDS <- 'rfData_1000000_0.05_1_Prune_Round_8.rds'
# rfDataRDS <- 'rfData_1000000_0.05_1_Prune_6.rds'
threshold <- 0.05
holdback <- 1
testReps <- 5
testHoldBack <- 0.2
Round <- T
generateBarPlot <- T
nTopImportance <- 20
indeterminateUpper <- .75
indeterminateLower <- .25
Type = 1

# removeEndpoints <- c('avg_ggt_zscore', 'Infiltrate', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')
removeEndpoints <- c('Infiltrate', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')

LB <- rbind(read.csv('Liver_master_LB_list.csv'), read.csv('not_Liver_master_LB_list.csv'))
OM <- rbind(read.csv('Liver_master_liverToBW.csv'), read.csv('not_Liver_master_liverToBW.csv'))

# lxx <- read.csv('Liver_master_LB_list.csv')
# lnxx <- read.csv('not_Liver_master_LB_list.csv') 

MIl <- read.csv('Liver_master_MI_list.csv')
MIn <- read.csv('not_Liver_master_MI_list.csv')

MIinter <- intersect(colnames(MIl), colnames(MIn))
MI <- rbind(MIl[, MIinter], MIn[, MIinter])

MIextraL <- setdiff(colnames(MIl), colnames(MIn))
MIextraN <- setdiff(colnames(MIn), colnames(MIl))
for (j in MIextraL) {
  MI[, j] <- NA
  MI[seq(nrow(MIl)), j] <- MIl[, j]
}
for (j in MIextraN) {
  MI[, j] <- NA
  MI[seq((nrow(MIl) + 1), nrow(MI)), j] <- MIn[, j]
}

MI[is.na(MI)] <- 0

findings2replaceIndex <- grep('.', colnames(MI), fixed = T)
f2replace <- colnames(MI)[findings2replaceIndex]
fn2replace <- unique(toupper(colnames(MI)[-findings2replaceIndex]))
removeIndex <- which(fn2replace %in% c('INDST_TO',
                                       'STUDYID',
                                       'UNREMARKABLE',
                                       'THIKENING',
                                       'POSITIVE'))
fn2replace <- fn2replace[-removeIndex]
for (finding in fn2replace) {
  synonyms <- grep(finding, f2replace, ignore.case = T, value = T)
  for (synonym in synonyms) {
    index <- which(MI[[synonym]] > 0)
    for (i in index) {
      if (MI[[synonym]][i] > MI[[finding]][i]) {
        MI[[finding]][i] <- MI[[synonym]][i]
      }
    }
  }
}
MI <- MI[,-findings2replaceIndex]


LIVER <- rbind(read.csv('sa_liscr_mod_liver_result_df6_575.csv'), read.csv('sa_nLiver_scr_mod_liver_result_df7_575.csv'))

commonStudies <- Reduce(intersect, list(LB$STUDYID, OM$STUDYID, MI$STUDYID, LIVER$STUDYID))

extraDomains <- c('OM', 'MI')
count <- 0
for (study in commonStudies) {
  count <- count + 1
  newRow <- LB[which(LB$STUDYID == study),]
  for (domain in extraDomains) {
    domainData <- get(domain)
    studyIndex <- which(domainData$STUDYID == study)
    for (j in colnames(domainData)[3:ncol(domainData)]) {
      newRow[[j]] <- domainData[studyIndex, j]
    }
  }
  if (count == 1) {
    Data <- newRow
  } else {
    Data <- rbind(Data, newRow)
  }
}


removeIndex <- which(colnames(Data) %in% removeEndpoints)
Data <- Data[, -removeIndex]

if (Round == T) {
  zscoreIndex <- c(grep('avg_', colnames(Data)), grep('liver', colnames(Data)))
  for (i in zscoreIndex) {
    Data[, i] <- floor(Data[, i])
    maxIndex <- which(Data[, i] > 5)
    Data[maxIndex, i] <- 5
  }
  histoIndex <- which(substr(colnames(Data), 1, 1) %in% toupper(letters))
  histoIndex <- histoIndex[-1]
  for (i in histoIndex) {
    Data[, i] <- ceiling(Data[, i])
  }
}

columnSums <- sort(colSums(Data[,3:ncol(Data)], na.rm = T), decreasing = T)
Data[,3:ncol(Data)] <- Data[, names(columnSums)]
colnames(Data)[3:ncol(Data)] <- names(columnSums)

#write.csv(Data, 'mergedData.csv', row.names = F)

if (generateBarPlot == T) {
  Finding <- NULL
  LIVER <- NULL
  Value <- NULL
  for (finding in colnames(Data)[3:ncol(Data)]) {
    Finding <- c(Finding, finding)
    LIVER <- c(LIVER, 'Y')
    Value <- c(Value, mean(Data[which(Data$indst_TO == "Liver"), finding], na.rm = T))
    
    Finding <- c(Finding, finding)
    LIVER <- c(LIVER, 'N')
    Value <- c(Value, mean(Data[which(Data$indst_TO != "Liver"), finding], na.rm = T))
    
  }
  plotData <- as.data.frame(cbind(Finding, LIVER, Value))
  plotData$LIVER <- factor(plotData$LIVER)
  plotData$Finding <- factor(plotData$Finding)
  plotData$Value = as.numeric(plotData$Value)
  # plotData$Value = log(as.numeric(plotData$Value), base = 10)
  
  p <- ggplot(plotData, aes(x = Finding, y = Value, fill = LIVER)) + 
    geom_bar(stat="identity", position = 'dodge') +
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ylab('Average Score')
  #print(p)
}

################ Random Forest Modeling ########################

rfData <- Data[, -2]
rfData[which(rfData$indst_TO == 'Liver'), 1] <- 1
rfData[which(rfData$indst_TO == 'not_Liver'), 1] <- 0
# rfData[,1] <- as.numeric(rfData[,1])
rfData[,1] <- factor(rfData[,1], levels = c(1, 0))

# removeIndex <- which(colnames(rfData) %in% c('INFILTRATE'))
# rfData <- rfData[, -removeIndex]

if (Impute == T) {
  rfData <- rfImpute(indst_TO ~ ., rfData)
  
  if (Round == T) {
    zscoreIndex <- c(grep('avg_', colnames(rfData)), grep('liver', colnames(rfData)))
    for (i in zscoreIndex) {
      rfData[, i] <- floor(rfData[, i])
      maxIndex <- which(rfData[, i] > 5)
      rfData[maxIndex, i] <- 5
    }
    histoIndex <- which(substr(colnames(rfData), 1, 1) %in% toupper(letters))
    histoIndex <- histoIndex[-1]
    for (i in histoIndex) {
      rfData[, i] <- ceiling(rfData[, i])
    }
  }
}

count <- 0
if (reps > 0) {
  for (rep in seq(reps)) {
    print(paste0(rep/reps*100, '% Complete...'))
    if (holdback == 1) {
      ind <- sample(seq(nrow(rfData)), 1)
      train <- rfData[-ind,]
      test <- rfData[ind,]
      testIndex <- ind
    } else {
      ind <- sample(2, nrow(rfData), replace = T, prob = c((1- holdback), holdback))
      train <- rfData[ind==1,]
      test <- rfData[ind==2,]
      testIndex <- which(ind == 2)
    }
    
    if (Undersample == T) {
      posIndex <- which(train[,1] == 1)
      nPos <- length(posIndex)
      trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
      train <- train[trainIndex,]
    }
    
    # control <- trainControl(method="repeatedcv", number=10, repeats=3)
    # metric <- "Accuracy"
    # mtry <- sqrt(ncol(train))
    # tunegrid <- expand.grid(.mtry=mtry)
    # rf_default <- train(indst_TO~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
    # rf_random <- train(indst_TO~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)
    # print(rf_default)
    # print(rf_random)
    
    # mtry <- tuneRF(rfData[,-1], rfData[,1], ntreeTry=500,
    #                stepFactor=1.5,improve=0.01, trace=F, plot=F)
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    # print(mtry)
    # print(best.m)
    
    best.m <- 4
    
    rf <- randomForest(indst_TO ~ ., data=train, mytry = best.m, importance = F, ntree = 500, proximity = F)
    # rf <- tuneRF(train[,-1], train[,1], doBest = T, stepFactor = 1.5)
    
    if ((ErrorMethod == 'Flip')|(ErrorMethod == 'Prune')) {
      p <- predict(rf, test, type = 'prob')[,1]
      flipLiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 1)&(as.numeric(p) < threshold))]
      flipnot_LiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 0)&(as.numeric(p) > (1 - threshold)))]
      
      if (length(flipLiverIndex) > 0) {
        count <- count + 1
        if (ErrorMethod == 'Flip') {
          rfData[flipLiverIndex, 1] <- 0
        } else {
          rfData <- rfData[-flipLiverIndex,]
        }
        if (Round == T) {
          saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
        } else {
          saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
        }
        
      }
      if (length(flipnot_LiverIndex) > 0) {
        count <- count + 1
        if (ErrorMethod == 'Flip') {
          rfData[flipnot_LiverIndex, 1] <- 1
        } else {
          rfData <- rfData[-flipnot_LiverIndex,]
        }
        if (Round == T) {
          saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
        } else {
          saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
        }
      }
    }
  }
}

rfData <- readRDS(rfDataRDS)

Sensitivity <- NULL
Specificity <- NULL
PPV <- NULL
NPV <- NULL
Prevalence <- NULL
Accuracy <- NULL
nRemoved <- NULL
rfTestData <- rfData
colnames(rfTestData) <- seq(ncol(rfTestData))
for (j in seq(ncol(rfTestData))) {
  rfTestData[,j] <- NA
}
rfTestData <- rfTestData[,1:2]
if (exists('gini')) {rm(gini)}
for (i in seq(testReps)) {
  if (i == 1) {
    sampleIndicies <- seq(nrow(rfData))
  }
  if (i < testReps) {
    ind <- sample(seq(nrow(rfData)), floor((nrow(rfData)/testReps)-1), replace = F)
    sampleIndicies <- sampleIndicies[which(sampleIndicies %ni% ind)]
  } else {
    ind <- sampleIndicies
  }
  trainIndex <- which(seq(nrow(rfData)) %ni% ind)
  testIndex <- ind
  # ind <- sample(2, nrow(rfData), replace = T, prob = c((1- testHoldBack), testHoldBack))
  train <- rfData[trainIndex,]
  test <- rfData[testIndex,]
  
  rfAll <- randomForest(indst_TO ~ ., data=rfData, mytry = best.m, 
                        importance = F, ntree = 500, proximity = T)
  # print(rfAll)
  
  if (Undersample == T) {
    posIndex <- which(train[,1] == 1)
    nPos <- length(posIndex)
    trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
    train <- train[trainIndex,]
    test <- rbind(train[-trainIndex,], test)
  }
  rf <- randomForest(indst_TO ~ ., data=train, mytry = best.m, 
                     importance = T, ntree = 500, proximity = T)
  
  # print(rf)
  
  p2r <- predict(rf, test, type = 'prob')[,1]
  
  rfTestData[names(p2r), i] <- as.numeric(p2r)
  
  indeterminateIndex <- which((p2r < indeterminateUpper)&(p2r > indeterminateLower))
  nRemoved <- c(nRemoved, length(indeterminateIndex)/length(p2r))
  p2r[indeterminateIndex] <- NA
  p2r <- round(p2r)
  
  Results <- confusionMatrix(factor(p2r, levels = c(1, 0)), factor(test$indst_TO, levels = c(1, 0)))
  Sensitivity <- c(Sensitivity, Results$byClass[['Sensitivity']])
  Specificity <- c(Specificity, Results$byClass[['Specificity']])
  PPV <- c(PPV, Results$byClass[['Pos Pred Value']])
  NPV <- c(NPV, Results$byClass[['Neg Pred Value']])
  Prevalence <- c(Prevalence, Results$byClass[['Prevalence']])
  Accuracy <- c(Accuracy, Results$byClass[['Balanced Accuracy']])
  
  giniTmp <- importance(rf, type = Type)
  if (exists('gini')) {
    gini <- cbind(gini, giniTmp)
  } else {
    gini <- giniTmp
  }
}
PerformanceMatrix <- cbind(Sensitivity,
                           Specificity,
                           PPV, NPV,
                           Prevalence,
                           Accuracy,
                           nRemoved)
PerformanceSummary <- colMeans(PerformanceMatrix, na.rm = T)
print(PerformanceSummary)
print(sort(rowMeans(gini), decreasing = T))

imp <- as.matrix(rowMeans(gini)[1:nTopImportance])
if (Type == 1) {
  colnames(imp) <- 'MeanDecreaseAccuracy'
} else {
  colnames(imp) <- 'MeanDecreaseGini'
}
ord <- order(imp[,1])
dotchart(imp[ord, 1], xlab = colnames(imp)[1], ylab = "", 
         main = paste0('Top ', nrow(imp), ' - Variable Importance'))#, xlim = c(xmin, max(imp[, i])))
# varImpPlot(rf,
#            sort = T,
#            n.var = 20,
#            main = "Top 20 - Variable Importance")


pred1=predict(rfAll,type = "prob")
perf = prediction(pred1[,1], levels(rfData[,1])[rfData[,1]])
# 1. Area under curve
auc = performance(perf, "auc")
AUC <- auc@y.values[[1]]
print(AUC)
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main=paste0("ROC Curve for Random Forest (AUC = ", round(AUC, digits = 3), ")"),col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# MDSplot(rf, train$indst_TO, k = 3, palette = rep(1, 3), pch = as.numeric(levels(train$indst_TO)))

reprtree:::plot.reprtree(reprtree::ReprTree(rfAll, train, metric='d2'))

# saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '.rds'))

histoData <- as.data.frame(cbind(rowMeans(rfTestData, na.rm = T), rfData[,1]))
histoData[which(histoData[,2] == 1), 2] <- 'Y'
histoData[which(histoData[,2] == 2), 2] <- 'N'
colnames(histoData) <- c('Probability', 'LIVER')

H <- p <- histoData %>%
  ggplot( aes(x=Probability, fill=LIVER)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  # theme_ipsum() +
  labs(fill = "LIVER", x = "Model Prediction P(LIVER)", y = "Count")
print(H)


rat_43 <- read.csv('selected_liver_43_rat_studyid.csv')
rat_168 <- read.csv('selected_not_liver_168_rat_studyids.csv')


