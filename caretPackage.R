rm(list = ls() )
#available packages by me
library(RRegrs)
library(Rcpi)
library(chemmodlab)


library(caret)
library(mlbench) #  for data 
data(Sonar)

set.seed(107)
inTrain <- createDataPartition( y = Sonar$Class, ## the outcome data are needed 
                                p = .75, ## The percentage of data in the training set
                                list = FALSE )

str(inTrain)

training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

nrow(training)
nrow(testing)

ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

set.seed(123)

plsFit <- train( Class ~ .,
                data = training,
                method = 'pls',
                preProc = c("center", "scale"),
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC"
)
plsFit


