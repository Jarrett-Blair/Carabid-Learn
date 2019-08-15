library(lattice)
library(caret)
setwd("C:/Carabid_Data/Carabid_Data/CrossValidCarabid")
#With PCA
#normParam <- preProcess(trainData, method = "pca", thresh = 0.95)

#Species

sptrainData = as.data.frame(read.csv("sptrain.csv"))
sptestData = as.data.frame(read.csv("sptest.csv"))

spremove = c(1:19,22,23,25,26,31,35:38)
sptestData = sptestData[,-spremove]
sptrainData = sptrainData[,-spremove]

spnormParam <- preProcess(sptrainData, method = "pca", thresh = 0.99)
spnorm.testData <- predict(spnormParam, sptestData)
spnorm.trainData <- predict(spnormParam, sptrainData)

write.csv(spnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsptest.csv", row.names = F)
write.csv(spnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsptrain.csv", row.names = F)

#Group

grouptrainData = as.data.frame(read.csv("grouptrain.csv"))
grouptestData = as.data.frame(read.csv("grouptest.csv"))

groupremove = c(1:16,18:20,22,23,25,26,31,35:38)
grouptestData = grouptestData[,-groupremove]
grouptrainData = grouptrainData[,-groupremove]

groupnormParam <- preProcess(grouptrainData, method = "pca", thresh = 0.99)
groupnorm.testData <- predict(groupnormParam, grouptestData)
groupnorm.trainData <- predict(groupnormParam, grouptrainData)

write.csv(groupnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normgrouptest.csv", row.names = F)
write.csv(groupnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normgrouptrain.csv", row.names = F)

#Subgenus

subgenustrainData = as.data.frame(read.csv("subgenustrain.csv"))
subgenustestData = as.data.frame(read.csv("subgenustest.csv"))

subgenusremove = c(1:15,17:20,22,23,25,26,31,35:38)
subgenustestData = subgenustestData[,-subgenusremove]
subgenustrainData = subgenustrainData[,-subgenusremove]

subgenusnormParam <- preProcess(subgenustrainData, method = "pca", thresh = 0.99)
subgenusnorm.testData <- predict(subgenusnormParam, subgenustestData)
subgenusnorm.trainData <- predict(subgenusnormParam, subgenustrainData)

write.csv(subgenusnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsubgenustest.csv", row.names = F)
write.csv(subgenusnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsubgenustrain.csv", row.names = F)

#Genus

genustrainData = as.data.frame(read.csv("genustrain.csv"))
genustestData = as.data.frame(read.csv("genustest.csv"))

genusremove = c(1:14,16:20,22,23,25,26,31,35:38)
genustestData = genustestData[,-genusremove]
genustrainData = genustrainData[,-genusremove]

genusnormParam <- preProcess(genustrainData, method = "pca", thresh = 0.99)
genusnorm.testData <- predict(genusnormParam, genustestData)
genusnorm.trainData <- predict(genusnormParam, genustrainData)

write.csv(genusnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normgenustest.csv", row.names = F)
write.csv(genusnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normgenustrain.csv", row.names = F)

#Subtribe

subtribtrainData = as.data.frame(read.csv("subtribtrain.csv"))
subtribtestData = as.data.frame(read.csv("subtribtest.csv"))

subtribremove = c(1:13,15:20,22,23,25,26,31,35:38)
subtribtestData = subtribtestData[,-subtribremove]
subtribtrainData = subtribtrainData[,-subtribremove]

subtribnormParam <- preProcess(subtribtrainData, method = "pca", thresh = 0.99)
subtribnorm.testData <- predict(subtribnormParam, subtribtestData)
subtribnorm.trainData <- predict(subtribnormParam, subtribtrainData)

write.csv(subtribnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsubtribtest.csv", row.names = F)
write.csv(subtribnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsubtribtrain.csv", row.names = F)

#Tribe

tribtrainData = as.data.frame(read.csv("tribtrain.csv"))
tribtestData = as.data.frame(read.csv("tribtest.csv"))

tribremove = c(1:12,14:20,22,23,25,26,31,35:38)
tribtestData = tribtestData[,-tribremove]
tribtrainData = tribtrainData[,-tribremove]

tribnormParam <- preProcess(tribtrainData, method = "pca", thresh = 0.99)
tribnorm.testData <- predict(tribnormParam, tribtestData)
tribnorm.trainData <- predict(tribnormParam, tribtrainData)

write.csv(tribnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normtribtest.csv", row.names = F)
write.csv(tribnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normtribtrain.csv", row.names = F)

#Supertribe

suptribtrainData = as.data.frame(read.csv("suptribtrain.csv"))
suptribtestData = as.data.frame(read.csv("suptribtest.csv"))

suptribremove = c(1:11,13:20,22,23,25,26,31,35:38)
suptribtestData = suptribtestData[,-suptribremove]
suptribtrainData = suptribtrainData[,-suptribremove]

suptribnormParam <- preProcess(suptribtrainData, method = "pca", thresh = 0.99)
suptribnorm.testData <- predict(suptribnormParam, suptribtestData)
suptribnorm.trainData <- predict(suptribnormParam, suptribtrainData)

write.csv(suptribnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsuptribtest.csv", row.names = F)
write.csv(suptribnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsuptribtrain.csv", row.names = F)

#Subfamily

subfamtrainData = as.data.frame(read.csv("subfamtrain.csv"))
subfamtestData = as.data.frame(read.csv("subfamtest.csv"))

subfamremove = c(1:10,12:20,22,23,25,26,31,35:38)
subfamtestData = subfamtestData[,-subfamremove]
subfamtrainData = subfamtrainData[,-subfamremove]

subfamnormParam <- preProcess(subfamtrainData, method = "pca", thresh = 0.99)
subfamnorm.testData <- predict(subfamnormParam, subfamtestData)
subfamnorm.trainData <- predict(subfamnormParam, subfamtrainData)

write.csv(subfamnorm.testData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsubfamtest.csv", row.names = F)
write.csv(subfamnorm.trainData, "C:/Carabid_Data/Carabid_Data/CrossValidCarabid/CarabidPCA/normsubfamtrain.csv", row.names = F)