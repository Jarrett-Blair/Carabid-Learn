library(caret)
library(e1071)
library(randomForest)
setwd("C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA")

sptrainPCA = read.csv("normsptrain.csv")
sptestPCA = read.csv("normsptest.csv")
grouptrainPCA = read.csv("normgrouptrain.csv")
grouptestPCA = read.csv("normgrouptest.csv")
subgenustrainPCA = read.csv("normsubgenustrain.csv")
subgenustestPCA = read.csv("normsubgenustest.csv")
genustrainPCA = read.csv("normgenustrain.csv")
genustestPCA = read.csv("normgenustest.csv")
subtribtrainPCA = read.csv("normsubtribtrain.csv")
subtribtestPCA = read.csv("normsubtribtest.csv")
tribtrainPCA = read.csv("normtribtrain.csv")
tribtestPCA = read.csv("normtribtest.csv")
suptribtrainPCA = read.csv("normsuptribtrain.csv")
suptribtestPCA = read.csv("normsuptribtest.csv")
subfamtrainPCA = read.csv("normsubfamtrain.csv")
subfamtestPCA = read.csv("normsubfamtest.csv")

setwd("C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidWide")

sptrainWide = read.csv("normsptrain.csv")
sptestWide = read.csv("normsptest.csv")
grouptrainWide = read.csv("normgrouptrain.csv")
grouptestWide = read.csv("normgrouptest.csv")
subgenustrainWide = read.csv("normsubgenustrain.csv")
subgenustestWide = read.csv("normsubgenustest.csv")
genustrainWide = read.csv("normgenustrain.csv")
genustestWide = read.csv("normgenustest.csv")
subtribtrainWide = read.csv("normsubtribtrain.csv")
subtribtestWide = read.csv("normsubtribtest.csv")
tribtrainWide = read.csv("normtribtrain.csv")
tribtestWide = read.csv("normtribtest.csv")
suptribtrainWide = read.csv("normsuptribtrain.csv")
suptribtestWide = read.csv("normsuptribtest.csv")
subfamtrainWide = read.csv("normsubfamtrain.csv")
subfamtestWide = read.csv("normsubfamtest.csv")

trControl = trainControl(method = "cv",
                         number = 10)
set.seed(27)

knn = train(SpeciesName ~., 
            method = "knn", 
            tuneGrid = expand.grid(k = 1:25), 
            trControl = trControl, 
            metric = "Accuracy",
            data = sptrainPCA)
pred.knn = predict.train(knn)
knnresults = knn$results
knnbestresults = knnresults[knn$bestTune$k,]

lda = train(SpeciesName ~., 
            method = "lda", 
            trControl = trControl, 
            metric = "Accuracy",
            data = sptrainPCA)
pred.lda = predict.train(lda)
ldaresults = lda$results

nbayes = train(SpeciesName ~., 
               method = "naive_bayes", 
               trControl = trControl, 
               metric = "Accuracy",
               data = sptrainPCA)
pred.nbayes = predict.train(nbayes)
nbayesresults = nbayes$results



predDF = data.frame(pred.knn, pred.lda, pred.nbayes, Actual = sptrainWide$SpeciesName, stringsAsFactors = F)

modelStack = train(Actual ~., 
                   data = predDF, 
                   method = "rf",
                   metric = "Accuracy",
                   tuneGrid = expand.grid(.mtry = c(1:3)))

test.pred.knn = predict(knn, sptestPCA)
test.pred.lda = predict(lda, sptestPCA)
test.pred.nbayes = predict(nbayes, sptestPCA)
testDF = data.frame(test.pred.knn, test.pred.lda, test.pred.nbayes, Actual = sptestPCA$SpeciesName, stringsAsFactors = F)
names(testDF) = names(predDF)

combPred = predict(modelStack, testDF)
ensembleaccuracy = combPred == testDF$Actual
length(ensembleaccuracy[ensembleaccuracy == TRUE])/nrow(testDF)

knnaccuracy = test.pred.knn == sptestPCA$SpeciesName
length(knnaccuracy[knnaccuracy == TRUE])/nrow(sptestPCA)
