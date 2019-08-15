library(caret)
library(e1071)
library(randomForest)

setwd("C:/Carabid_Data/Carabid_Data/CarabidPCA")

sptrainPCA = read.csv("normsptrain.csv")
spvalidPCA = read.csv("normspvalid.csv")
sptestPCA = read.csv("normsptest.csv")
grouptrainPCA = read.csv("normgrouptrain.csv")
groupvalidPCA = read.csv("normgroupvalid.csv")
grouptestPCA = read.csv("normgrouptest.csv")
subgenustrainPCA = read.csv("normsubgenustrain.csv")
subgenusvalidPCA = read.csv("normsubgenusvalid.csv")
subgenustestPCA = read.csv("normsubgenustest.csv")
genustrainPCA = read.csv("normgenustrain.csv")
genusvalidPCA = read.csv("normgenusvalid.csv")
genustestPCA = read.csv("normgenustest.csv")
subtribtrainPCA = read.csv("normsubtribtrain.csv")
subtribvalidPCA = read.csv("normsubtribvalid.csv")
subtribtestPCA = read.csv("normsubtribtest.csv")
tribtrainPCA = read.csv("normtribtrain.csv")
tribvalidPCA = read.csv("normtribvalid.csv")
tribtestPCA = read.csv("normtribtest.csv")
suptribtrainPCA = read.csv("normsuptribtrain.csv")
suptribvalidPCA = read.csv("normsuptribvalid.csv")
suptribtestPCA = read.csv("normsuptribtest.csv")
subfamtrainPCA = read.csv("normsubfamtrain.csv")
subfamvalidPCA = read.csv("normsubfamvalid.csv")
subfamtestPCA = read.csv("normsubfamtest.csv")

setwd("C:/Carabid_Data/Carabid_Data/CarabidWide")

sptrainWide = read.csv("normsptrain.csv")
spvalidWide = read.csv("normspvalid.csv")
sptestWide = read.csv("normsptest.csv")
grouptrainWide = read.csv("normgrouptrain.csv")
groupvalidWide = read.csv("normgroupvalid.csv")
grouptestWide = read.csv("normgrouptest.csv")
subgenustrainWide = read.csv("normsubgenustrain.csv")
subgenusvalidWide = read.csv("normsubgenusvalid.csv")
subgenustestWide = read.csv("normsubgenustest.csv")
genustrainWide = read.csv("normgenustrain.csv")
genusvalidWide = read.csv("normgenusvalid.csv")
genustestWide = read.csv("normgenustest.csv")
subtribtrainWide = read.csv("normsubtribtrain.csv")
subtribvalidWide = read.csv("normsubtribvalid.csv")
subtribtestWide = read.csv("normsubtribtest.csv")
tribtrainWide = read.csv("normtribtrain.csv")
tribvalidWide = read.csv("normtribvalid.csv")
tribtestWide = read.csv("normtribtest.csv")
suptribtrainWide = read.csv("normsuptribtrain.csv")
suptribvalidWide = read.csv("normsuptribvalid.csv")
suptribtestWide = read.csv("normsuptribtest.csv")
subfamtrainWide = read.csv("normsubfamtrain.csv")
subfamvalidWide = read.csv("normsubfamvalid.csv")
subfamtestWide = read.csv("normsubfamtest.csv")

set.seed(27)

knn = train(SpeciesName ~., 
            method = "knn", 
            tuneGrid = expand.grid(k = 1:25), 
            metric = "Accuracy",
            data = sptrainPCA)
pred.knn = predict.train(knn, newdata = spvalidPCA)
knnresults = knn$results
knnbestresults = knnresults[knn$bestTune$k,]

lda = train(SpeciesName ~., 
            method = "lda", 
            metric = "Accuracy",
            data = sptrainPCA)
pred.lda = predict.train(lda, newdata = spvalidPCA)
ldaresults = lda$results

nbayes = train(SpeciesName ~., 
               method = "naive_bayes", 
               metric = "Accuracy",
               data = sptrainPCA)
pred.nbayes = predict.train(nbayes, newdata = spvalidPCA)
nbayesresults = nbayes$results

rf = train(SpeciesName ~., 
           method = "rf", 
           tuneGrid = expand.grid(.mtry = c(1:13)), 
           metric = "Accuracy",
           data = sptrainWide)
pred.rf = predict.train(rf, newdata = spvalidWide)
rfresults = rf$results
rfbestresults = rfresults[rf$bestTune$.mtry,]

predDF = data.frame(pred.knn, 
                    pred.lda, 
                    pred.rf, 
                    #pred.nbayes,
                    Actual = spvalidWide$SpeciesName, 
                    stringsAsFactors = F)

#tuneRF(predDF[,1:3], predDF[,4], ntreeTry = 500)

modelStack = randomForest(Actual ~.,
                          data = predDF,
                          ntree = 500,
                          mtry = 2)

test.knn = predict(knn, sptestPCA)
test.lda = predict(lda, sptestPCA)
#test.nbayes = predict(nbayes, sptestPCA)
test.rf = predict(rf, sptestWide)
testDF = data.frame(test.knn, 
                    test.lda, 
                    #test.nbayes, 
                    test.rf, 
                    Actual = sptestPCA$SpeciesName, 
                    stringsAsFactors = F)
names(testDF) = names(predDF)

combPred = predict(modelStack, testDF)
ensembleaccuracy = combPred == testDF$Actual
length(ensembleaccuracy[ensembleaccuracy == TRUE])/nrow(testDF)

knnaccuracy = test.knn == sptestPCA$SpeciesName
length(knnaccuracy[knnaccuracy == TRUE])/nrow(sptestPCA)

rfaccuracy = test.rf == sptestPCA$SpeciesName
length(rfaccuracy[rfaccuracy == TRUE])/nrow(sptestPCA)
