library(lattice)
library(caret)
setwd("C:/Carabid_Data/Carabid_Data/Carabid")
#With PCA
#normParam <- preProcess(trainData, method = "pca", thresh = 0.95)

#Species

sptrainData = as.data.frame(read.csv("sptrain.csv"))
spvalidData = as.data.frame(read.csv("sptest.csv"))
sptestData = as.data.frame(read.csv("spvalid.csv"))

spremove = c(1:19,22,23,25,26,31,35:38)
sptestData = sptestData[,-spremove]
sptrainData = sptrainData[,-spremove]
spvalidData = spvalidData[,-spremove]

spnormParam <- preProcess(sptrainData)
spnorm.testData <- predict(spnormParam, sptestData)
spnorm.trainData <- predict(spnormParam, sptrainData)
spnorm.validData <- predict(spnormParam, spvalidData)

write.csv(spnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsptest.csv", row.names = F)
write.csv(spnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsptrain.csv", row.names = F)
write.csv(spnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normspvalid.csv", row.names = F)

#Group

grouptrainData = as.data.frame(read.csv("grouptrain.csv"))
groupvalidData = as.data.frame(read.csv("grouptest.csv"))
grouptestData = as.data.frame(read.csv("groupvalid.csv"))

groupremove = c(1:16,18:20,22,23,25,26,31,35:38)
grouptestData = grouptestData[,-groupremove]
grouptrainData = grouptrainData[,-groupremove]
groupvalidData = groupvalidData[,-groupremove]

groupnormParam <- preProcess(grouptrainData)
groupnorm.testData <- predict(groupnormParam, grouptestData)
groupnorm.trainData <- predict(groupnormParam, grouptrainData)
groupnorm.validData <- predict(groupnormParam, groupvalidData)

write.csv(groupnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normgrouptest.csv", row.names = F)
write.csv(groupnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normgrouptrain.csv", row.names = F)
write.csv(groupnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normgroupvalid.csv", row.names = F)

#Subgenus

subgenustrainData = as.data.frame(read.csv("subgenustrain.csv"))
subgenusvalidData = as.data.frame(read.csv("subgenustest.csv"))
subgenustestData = as.data.frame(read.csv("subgenusvalid.csv"))

subgenusremove = c(1:15,17:20,22,23,25,26,31,35:38)
subgenustestData = subgenustestData[,-subgenusremove]
subgenustrainData = subgenustrainData[,-subgenusremove]
subgenusvalidData = subgenusvalidData[,-subgenusremove]

subgenusnormParam <- preProcess(subgenustrainData)
subgenusnorm.testData <- predict(subgenusnormParam, subgenustestData)
subgenusnorm.trainData <- predict(subgenusnormParam, subgenustrainData)
subgenusnorm.validData <- predict(subgenusnormParam, subgenusvalidData)

write.csv(subgenusnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubgenustest.csv", row.names = F)
write.csv(subgenusnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubgenustrain.csv", row.names = F)
write.csv(subgenusnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubgenusvalid.csv", row.names = F)

#Genus

genustrainData = as.data.frame(read.csv("genustrain.csv"))
genusvalidData = as.data.frame(read.csv("genustest.csv"))
genustestData = as.data.frame(read.csv("genusvalid.csv"))

genusremove = c(1:14,16:20,22,23,25,26,31,35:38)
genustestData = genustestData[,-genusremove]
genustrainData = genustrainData[,-genusremove]
genusvalidData = genusvalidData[,-genusremove]

genusnormParam <- preProcess(genustrainData)
genusnorm.testData <- predict(genusnormParam, genustestData)
genusnorm.trainData <- predict(genusnormParam, genustrainData)
genusnorm.validData <- predict(genusnormParam, genusvalidData)

write.csv(genusnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normgenustest.csv", row.names = F)
write.csv(genusnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normgenustrain.csv", row.names = F)
write.csv(genusnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normgenusvalid.csv", row.names = F)

#Subtribe

subtribtrainData = as.data.frame(read.csv("subtribtrain.csv"))
subtribvalidData = as.data.frame(read.csv("subtribtest.csv"))
subtribtestData = as.data.frame(read.csv("subtribvalid.csv"))

subtribremove = c(1:13,15:20,22,23,25,26,31,35:38)
subtribtestData = subtribtestData[,-subtribremove]
subtribtrainData = subtribtrainData[,-subtribremove]
subtribvalidData = subtribvalidData[,-subtribremove]

subtribnormParam <- preProcess(subtribtrainData)
subtribnorm.testData <- predict(subtribnormParam, subtribtestData)
subtribnorm.trainData <- predict(subtribnormParam, subtribtrainData)
subtribnorm.validData <- predict(subtribnormParam, subtribvalidData)

write.csv(subtribnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubtribtest.csv", row.names = F)
write.csv(subtribnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubtribtrain.csv", row.names = F)
write.csv(subtribnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubtribvalid.csv", row.names = F)

#Tribe

tribtrainData = as.data.frame(read.csv("tribtrain.csv"))
tribvalidData = as.data.frame(read.csv("tribtest.csv"))
tribtestData = as.data.frame(read.csv("tribvalid.csv"))

tribremove = c(1:12,14:20,22,23,25,26,31,35:38)
tribtestData = tribtestData[,-tribremove]
tribtrainData = tribtrainData[,-tribremove]
tribvalidData = tribvalidData[,-tribremove]

tribnormParam <- preProcess(tribtrainData)
tribnorm.testData <- predict(tribnormParam, tribtestData)
tribnorm.trainData <- predict(tribnormParam, tribtrainData)
tribnorm.validData <- predict(tribnormParam, tribvalidData)

write.csv(tribnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normtribtest.csv", row.names = F)
write.csv(tribnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normtribtrain.csv", row.names = F)
write.csv(tribnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normtribvalid.csv", row.names = F)

#Supertribe

suptribtrainData = as.data.frame(read.csv("suptribtrain.csv"))
suptribvalidData = as.data.frame(read.csv("suptribtest.csv"))
suptribtestData = as.data.frame(read.csv("suptribvalid.csv"))

suptribremove = c(1:11,13:20,22,23,25,26,31,35:38)
suptribtestData = suptribtestData[,-suptribremove]
suptribtrainData = suptribtrainData[,-suptribremove]
suptribvalidData = suptribvalidData[,-suptribremove]

suptribnormParam <- preProcess(suptribtrainData)
suptribnorm.testData <- predict(suptribnormParam, suptribtestData)
suptribnorm.trainData <- predict(suptribnormParam, suptribtrainData)
suptribnorm.validData <- predict(suptribnormParam, suptribvalidData)

write.csv(suptribnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsuptribtest.csv", row.names = F)
write.csv(suptribnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsuptribtrain.csv", row.names = F)
write.csv(suptribnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsuptribvalid.csv", row.names = F)

#Subfamily

subfamtrainData = as.data.frame(read.csv("subfamtrain.csv"))
subfamvalidData = as.data.frame(read.csv("subfamtest.csv"))
subfamtestData = as.data.frame(read.csv("subfamvalid.csv"))

subfamremove = c(1:10,12:20,22,23,25,26,31,35:38)
subfamtestData = subfamtestData[,-subfamremove]
subfamtrainData = subfamtrainData[,-subfamremove]
subfamvalidData = subfamvalidData[,-subfamremove]

subfamnormParam <- preProcess(subfamtrainData)
subfamnorm.testData <- predict(subfamnormParam, subfamtestData)
subfamnorm.trainData <- predict(subfamnormParam, subfamtrainData)
subfamnorm.validData <- predict(subfamnormParam, subfamvalidData)

write.csv(subfamnorm.testData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubfamtest.csv", row.names = F)
write.csv(subfamnorm.trainData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubfamtrain.csv", row.names = F)
write.csv(subfamnorm.validData, "C:/Carabid_Data/Carabid_Data/CarabidWide/normsubfamvalid.csv", row.names = F)