library(ggplot2)
library(lattice)
library(caret)
setwd("C:/Carabid_Data/Carabid")
#With PCA
#normParam <- preProcess(trainData, method = "pca", thresh = 0.95)

genustrainData = as.data.frame(read.csv("genustrain.csv"))
genusvalidData = as.data.frame(read.csv("genustest.csv"))
genustestData = as.data.frame(read.csv("genusvalid.csv"))

genusnormParam <- preProcess(genustrainData)
genusnorm.testData <- predict(genusnormParam, genustestData)
genusnorm.trainData <- predict(genusnormParam, genustrainData)
genusnorm.validData <- predict(genusnormParam, genusvalidData)

write.csv(genusnorm.testData, "normgenustest.csv", row.names = F)
write.csv(genusnorm.trainData, "normgenustrain.csv", row.names = F)
write.csv(genusnorm.validData, "normgenusvalid.csv", row.names = F)

#Species

sptrainData = as.data.frame(read.csv("sptrain.csv"))
spvalidData = as.data.frame(read.csv("sptest.csv"))
sptestData = as.data.frame(read.csv("spvalid.csv"))

spnormParam <- preProcess(sptrainData)
spnorm.testData <- predict(spnormParam, sptestData)
spnorm.trainData <- predict(spnormParam, sptrainData)
spnorm.validData <- predict(spnormParam, spvalidData)

write.csv(spnorm.testData, "normsptest.csv", row.names = F)
write.csv(spnorm.trainData, "normsptrain.csv", row.names = F)
write.csv(spnorm.validData, "normsptvalid.csv", row.names = F)

#Group

grouptrainData = as.data.frame(read.csv("grouptrain.csv"))
groupvalidData = as.data.frame(read.csv("grouptest.csv"))
grouptestData = as.data.frame(read.csv("groupvalid.csv"))

groupnormParam <- preProcess(grouptrainData)
groupnorm.testData <- predict(groupnormParam, grouptestData)
groupnorm.trainData <- predict(groupnormParam, grouptrainData)
groupnorm.validData <- predict(groupnormParam, groupvalidData)

write.csv(groupnorm.testData, "normgrouptest.csv", row.names = F)
write.csv(groupnorm.trainData, "normgrouptrain.csv", row.names = F)
write.csv(groupnorm.validData, "normgrouptvalid.csv", row.names = F)

#Subgenus

subgenustrainData = as.data.frame(read.csv("subgenustrain.csv"))
subgenusvalidData = as.data.frame(read.csv("subgenustest.csv"))
subgenustestData = as.data.frame(read.csv("subgenusvalid.csv"))

subgenusnormParam <- preProcess(subgenustrainData)
subgenusnorm.testData <- predict(subgenusnormParam, subgenustestData)
subgenusnorm.trainData <- predict(subgenusnormParam, subgenustrainData)
subgenusnorm.validData <- predict(subgenusnormParam, subgenusvalidData)

write.csv(subgenusnorm.testData, "normsubgenustest.csv", row.names = F)
write.csv(subgenusnorm.trainData, "normsubgenustrain.csv", row.names = F)
write.csv(subgenusnorm.validData, "normsubgenustvalid.csv", row.names = F)

#Subtribe

subtribtrainData = as.data.frame(read.csv("subtribtrain.csv"))
subtribvalidData = as.data.frame(read.csv("subtribtest.csv"))
subtribtestData = as.data.frame(read.csv("subtribvalid.csv"))

subtribnormParam <- preProcess(subtribtrainData)
subtribnorm.testData <- predict(subtribnormParam, subtribtestData)
subtribnorm.trainData <- predict(subtribnormParam, subtribtrainData)
subtribnorm.validData <- predict(subtribnormParam, subtribvalidData)

write.csv(subtribnorm.testData, "normsubtribtest.csv", row.names = F)
write.csv(subtribnorm.trainData, "normsubtribtrain.csv", row.names = F)
write.csv(subtribnorm.validData, "normsubtribtvalid.csv", row.names = F)

#Tribe

tribtrainData = as.data.frame(read.csv("tribtrain.csv"))
tribvalidData = as.data.frame(read.csv("tribtest.csv"))
tribtestData = as.data.frame(read.csv("tribvalid.csv"))

tribnormParam <- preProcess(tribtrainData)
tribnorm.testData <- predict(tribnormParam, tribtestData)
tribnorm.trainData <- predict(tribnormParam, tribtrainData)
tribnorm.validData <- predict(tribnormParam, tribvalidData)

write.csv(tribnorm.testData, "normtribtest.csv", row.names = F)
write.csv(tribnorm.trainData, "normtribtrain.csv", row.names = F)
write.csv(tribnorm.validData, "normtribtvalid.csv", row.names = F)

#Supertribe

suptribtrainData = as.data.frame(read.csv("suptribtrain.csv"))
suptribvalidData = as.data.frame(read.csv("suptribtest.csv"))
suptribtestData = as.data.frame(read.csv("suptribvalid.csv"))

suptribnormParam <- preProcess(suptribtrainData)
suptribnorm.testData <- predict(suptribnormParam, suptribtestData)
suptribnorm.trainData <- predict(suptribnormParam, suptribtrainData)
suptribnorm.validData <- predict(suptribnormParam, suptribvalidData)

write.csv(suptribnorm.testData, "normsuptribtest.csv", row.names = F)
write.csv(suptribnorm.trainData, "normsuptribtrain.csv", row.names = F)
write.csv(suptribnorm.validData, "normsuptribtvalid.csv", row.names = F)

#Subfamily

subfamtrainData = as.data.frame(read.csv("subfamtrain.csv"))
subfamvalidData = as.data.frame(read.csv("subfamtest.csv"))
subfamtestData = as.data.frame(read.csv("subfamvalid.csv"))

subfamnormParam <- preProcess(subfamtrainData)
subfamnorm.testData <- predict(subfamnormParam, subfamtestData)
subfamnorm.trainData <- predict(subfamnormParam, subfamtrainData)
subfamnorm.validData <- predict(subfamnormParam, subfamvalidData)

write.csv(subfamnorm.testData, "normsubfamtest.csv", row.names = F)
write.csv(subfamnorm.trainData, "normsubfamtrain.csv", row.names = F)
write.csv(subfamnorm.validData, "normsubfamtvalid.csv", row.names = F)