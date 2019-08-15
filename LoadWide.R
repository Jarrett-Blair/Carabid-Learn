setwd("C:/Carabid_Data/Carabid_Data/CarabidWide")

#############           #############
#############  Species  #############
#############           #############

sptestData= read.csv("normsptest.csv")
sptrainData = read.csv("normsptrain.csv")
spvalidData = read.csv("normspvalid.csv")

sptrainLabels = sptrainData[,1]
spvalidLabels = spvalidData[,1]
sptestLabels = sptestData[,1]

num.sptrain = sptrainData[, sapply(sptrainData, is.numeric)]
num.spvalid = spvalidData[, sapply(spvalidData, is.numeric)]
num.sptest = sptestData[, sapply(sptestData, is.numeric)]

#############         #############
#############  Group  #############
#############         #############

grouptestData= read.csv("normgrouptest.csv")
grouptrainData = read.csv("normgrouptrain.csv")
groupvalidData = read.csv("normgroupvalid.csv")

grouptrainLabels = grouptrainData[,1]
groupvalidLabels = groupvalidData[,1]
grouptestLabels = grouptestData[,1]

num.grouptrain = grouptrainData[, sapply(grouptrainData, is.numeric)]
num.groupvalid = groupvalidData[, sapply(groupvalidData, is.numeric)]
num.grouptest = grouptestData[, sapply(grouptestData, is.numeric)]

#############            #############
#############  Subgenus  #############
#############            #############

subgenustestData= read.csv("normsubgenustest.csv")
subgenustrainData = read.csv("normsubgenustrain.csv")
subgenusvalidData = read.csv("normsubgenusvalid.csv")

subgenustrainLabels = subgenustrainData[,1]
subgenusvalidLabels = subgenusvalidData[,1]
subgenustestLabels = subgenustestData[,1]

num.subgenustrain = subgenustrainData[, sapply(subgenustrainData, is.numeric)]
num.subgenusvalid = subgenusvalidData[, sapply(subgenusvalidData, is.numeric)]
num.subgenustest = subgenustestData[, sapply(subgenustestData, is.numeric)]

############         #############
############  GENUS  #############
############         #############

genustestData= read.csv("normgenustest.csv")
genustrainData = read.csv("normgenustrain.csv")
genusvalidData = read.csv("normgenusvalid.csv")

genustrainLabels = genustrainData[,1]
genusvalidLabels = genusvalidData[,1]
genustestLabels = genustestData[,1]

num.genustrain = genustrainData[, sapply(genustrainData, is.numeric)]
num.genusvalid = genusvalidData[, sapply(genusvalidData, is.numeric)]
num.genustest = genustestData[, sapply(genustestData, is.numeric)]

#############            #############
#############  Subtribe  #############
#############            #############

subtribtestData= read.csv("normsubtribtest.csv")
subtribtrainData = read.csv("normsubtribtrain.csv")
subtribvalidData = read.csv("normsubtribvalid.csv")

subtribtrainLabels = subtribtrainData[,1]
subtribvalidLabels = subtribvalidData[,1]
subtribtestLabels = subtribtestData[,1]

num.subtribtrain = subtribtrainData[, sapply(subtribtrainData, is.numeric)]
num.subtribvalid = subtribvalidData[, sapply(subtribvalidData, is.numeric)]
num.subtribtest = subtribtestData[, sapply(subtribtestData, is.numeric)]

#############         #############
#############  Tribe  #############
#############         #############

tribtestData= read.csv("normtribtest.csv")
tribtrainData = read.csv("normtribtrain.csv")
tribvalidData = read.csv("normtribvalid.csv")

tribtrainLabels = tribtrainData[,1]
tribvalidLabels = tribvalidData[,1]
tribtestLabels = tribtestData[,1]

num.tribtrain = tribtrainData[, sapply(tribtrainData, is.numeric)]
num.tribvalid = tribvalidData[, sapply(tribvalidData, is.numeric)]
num.tribtest = tribtestData[, sapply(tribtestData, is.numeric)]

#############              #############
#############  Supertribe  #############
#############              #############

suptribtestData= read.csv("normsuptribtest.csv")
suptribtrainData = read.csv("normsuptribtrain.csv")
suptribvalidData = read.csv("normsuptribvalid.csv")

suptribtrainLabels = suptribtrainData[,1]
suptribvalidLabels = suptribvalidData[,1]
suptribtestLabels = suptribtestData[,1]

num.suptribtrain = suptribtrainData[, sapply(suptribtrainData, is.numeric)]
num.suptribvalid = suptribvalidData[, sapply(suptribvalidData, is.numeric)]
num.suptribtest = suptribtestData[, sapply(suptribtestData, is.numeric)]

#############             #############
#############  Subfamily  #############
#############             #############

subfamtestData= read.csv("normsubfamtest.csv")
subfamtrainData = read.csv("normsubfamtrain.csv")
subfamvalidData = read.csv("normsubfamvalid.csv")

subfamtrainLabels = subfamtrainData[,1]
subfamvalidLabels = subfamvalidData[,1]
subfamtestLabels = subfamtestData[,1]

num.subfamtrain = subfamtrainData[, sapply(subfamtrainData, is.numeric)]
num.subfamvalid = subfamvalidData[, sapply(subfamvalidData, is.numeric)]
num.subfamtest = subfamtestData[, sapply(subfamtestData, is.numeric)]
