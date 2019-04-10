library(class)
library(gmodels)
library(pvclass)
setwd("C:/Carabid_Data/Carabid")

taxonaccuracy = data.frame(matrix(NA, nrow = 8, ncol = 2))
names(taxonaccuracy) = c("Rank", "Accuracy")

#         #
#  GENUS  #
#         #

genustestData= read.csv("normgenustest.csv")
genustrainData = read.csv("normgenustrain.csv")
genusvalidData = read.csv("normgenusvalid.csv")

genustestData = genustestData[,-c(1:14,16:20,22,23,25:31,33:39)]
genustrainData = genustrainData[,-c(1:14,16:20,22,23,25:31,33:39)]
genusvalidData = genusvalidData[,-c(1:14,16:20,22,23,25:31,33:39)]

genustrainLabels = genustrainData[,1]
genusvalidLabels = genusvalidData[,1]
genustestLabels = genustestData[,1]

num.genustrain = genustrainData[, sapply(genustrainData, is.numeric)]
num.genusvalid = genusvalidData[, sapply(genusvalidData, is.numeric)]
num.genustest = genustestData[, sapply(genustestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.genusvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.genustrain, test = num.genusvalid, cl = genustrainLabels, k=n)
}

merge = data.frame(genusvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
genusaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
genusaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  genusaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(genusaccuracy) = c("K", "Accuracy")

taxonaccuracy[4,1] = "Genus"
taxonaccuracy[4,2] = max(genusaccuracy[,2])

#           #
#  Species  #
#           #

sptestData= read.csv("normsptest.csv")
sptrainData = read.csv("normsptrain.csv")
spvalidData = read.csv("normspvalid.csv")

sptestData = sptestData[,-c(1:19,22,23,25:31,33:39)]
sptrainData = sptrainData[,-c(1:19,22,23,25:31,33:39)]
spvalidData = spvalidData[,-c(1:19,22,23,25:31,33:39)]

sptrainLabels = sptrainData[,1]
spvalidLabels = spvalidData[,1]
sptestLabels = sptestData[,1]

num.sptrain = sptrainData[, sapply(sptrainData, is.numeric)]
num.spvalid = spvalidData[, sapply(spvalidData, is.numeric)]
num.sptest = sptestData[, sapply(sptestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.spvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.sptrain, test = num.spvalid, cl = sptrainLabels, k=n)
}

merge = data.frame(spvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
spaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
spaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  spaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(spaccuracy) = c("K", "Accuracy")

taxonaccuracy[1,1] = "Species"
taxonaccuracy[1,2] = max(spaccuracy[,2])

#         #
#  group  #
#         #

grouptestData= read.csv("normgrouptest.csv")
grouptrainData = read.csv("normgrouptrain.csv")
groupvalidData = read.csv("normgroupvalid.csv")

grouptestData = grouptestData[,-c(1:16,18:20,22,23,25:31,33:39)]
grouptrainData = grouptrainData[,-c(1:16,18:20,22,23,25:31,33:39)]
groupvalidData = groupvalidData[,-c(1:16,18:20,22,23,25:31,33:39)]

grouptrainLabels = grouptrainData[,1]
groupvalidLabels = groupvalidData[,1]
grouptestLabels = grouptestData[,1]

num.grouptrain = grouptrainData[, sapply(grouptrainData, is.numeric)]
num.groupvalid = groupvalidData[, sapply(groupvalidData, is.numeric)]
num.grouptest = grouptestData[, sapply(grouptestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.groupvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.grouptrain, test = num.groupvalid, cl = grouptrainLabels, k=n)
}

merge = data.frame(groupvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
groupaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
groupaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  groupaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(groupaccuracy) = c("K", "Accuracy")

taxonaccuracy[2,1] = "Group"
taxonaccuracy[2,2] = max(groupaccuracy[,2])

#         #
#  subgenus  #
#         #

subgenustestData= read.csv("normsubgenustest.csv")
subgenustrainData = read.csv("normsubgenustrain.csv")
subgenusvalidData = read.csv("normsubgenusvalid.csv")

subgenustestData = subgenustestData[,-c(1:15,17:20,22,23,25:31,33:39)]
subgenustrainData = subgenustrainData[,-c(1:15,17:20,22,23,25:31,33:39)]
subgenusvalidData = subgenusvalidData[,-c(1:15,17:20,22,23,25:31,33:39)]

subgenustrainLabels = subgenustrainData[,1]
subgenusvalidLabels = subgenusvalidData[,1]
subgenustestLabels = subgenustestData[,1]

num.subgenustrain = subgenustrainData[, sapply(subgenustrainData, is.numeric)]
num.subgenusvalid = subgenusvalidData[, sapply(subgenusvalidData, is.numeric)]
num.subgenustest = subgenustestData[, sapply(subgenustestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.subgenusvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.subgenustrain, test = num.subgenusvalid, cl = subgenustrainLabels, k=n)
}

merge = data.frame(subgenusvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
subgenusaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
subgenusaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  subgenusaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(subgenusaccuracy) = c("K", "Accuracy")

taxonaccuracy[3,1] = "Subgenus"
taxonaccuracy[3,2] = max(subgenusaccuracy[,2])

#         #
#  subtribe  #
#         #

subtribtestData= read.csv("normsubtribtest.csv")
subtribtrainData = read.csv("normsubtribtrain.csv")
subtribvalidData = read.csv("normsubtribvalid.csv")

subtribtestData = subtribtestData[,-c(1:13,15:20,22,23,25:31,33:39)]
subtribtrainData = subtribtrainData[,-c(1:13,15:20,22,23,25:31,33:39)]
subtribvalidData = subtribvalidData[,-c(1:13,15:20,22,23,25:31,33:39)]

subtribtrainLabels = subtribtrainData[,1]
subtribvalidLabels = subtribvalidData[,1]
subtribtestLabels = subtribtestData[,1]

num.subtribtrain = subtribtrainData[, sapply(subtribtrainData, is.numeric)]
num.subtribvalid = subtribvalidData[, sapply(subtribvalidData, is.numeric)]
num.subtribtest = subtribtestData[, sapply(subtribtestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.subtribvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.subtribtrain, test = num.subtribvalid, cl = subtribtrainLabels, k=n)
}

merge = data.frame(subtribvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
subtribaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
subtribaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  subtribaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(subtribaccuracy) = c("K", "Accuracy")

taxonaccuracy[5,1] = "Subtribe"
taxonaccuracy[5,2] = max(subtribaccuracy[,2])

#         #
#  Tribe  #
#         #

tribtestData= read.csv("normtribtest.csv")
tribtrainData = read.csv("normtribtrain.csv")
tribvalidData = read.csv("normtribvalid.csv")

tribtestData = tribtestData[,-c(1:12,14:20,22,23,25:31,33:39)]
tribtrainData = tribtrainData[,-c(1:12,14:20,22,23,25:31,33:39)]
tribvalidData = tribvalidData[,-c(1:12,14:20,22,23,25:31,33:39)]

tribtrainLabels = tribtrainData[,1]
tribvalidLabels = tribvalidData[,1]
tribtestLabels = tribtestData[,1]

num.tribtrain = tribtrainData[, sapply(tribtrainData, is.numeric)]
num.tribvalid = tribvalidData[, sapply(tribvalidData, is.numeric)]
num.tribtest = tribtestData[, sapply(tribtestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.tribvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.tribtrain, test = num.tribvalid, cl = tribtrainLabels, k=n)
}

merge = data.frame(tribvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
tribaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
tribaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  tribaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(tribaccuracy) = c("K", "Accuracy")

taxonaccuracy[6,1] = "Tribe"
taxonaccuracy[6,2] = max(tribaccuracy[,2])

#         #
#  Supertribe  #
#         #

suptribtestData= read.csv("normsuptribtest.csv")
suptribtrainData = read.csv("normsuptribtrain.csv")
suptribvalidData = read.csv("normsuptribvalid.csv")

suptribtestData = suptribtestData[,-c(1:11,13:20,22,23,25:31,33:39)]
suptribtrainData = suptribtrainData[,-c(1:11,13:20,22,23,25:31,33:39)]
suptribvalidData = suptribvalidData[,-c(1:11,13:20,22,23,25:31,33:39)]

suptribtrainLabels = suptribtrainData[,1]
suptribvalidLabels = suptribvalidData[,1]
suptribtestLabels = suptribtestData[,1]

num.suptribtrain = suptribtrainData[, sapply(suptribtrainData, is.numeric)]
num.suptribvalid = suptribvalidData[, sapply(suptribvalidData, is.numeric)]
num.suptribtest = suptribtestData[, sapply(suptribtestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.suptribvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.suptribtrain, test = num.suptribvalid, cl = suptribtrainLabels, k=n)
}

merge = data.frame(suptribvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
suptribaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
suptribaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  suptribaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(suptribaccuracy) = c("K", "Accuracy")

taxonaccuracy[7,1] = "Supertribe"
taxonaccuracy[7,2] = max(suptribaccuracy[,2])

#         #
#  Subfamily  #
#         #

subfamtestData= read.csv("normsubfamtest.csv")
subfamtrainData = read.csv("normsubfamtrain.csv")
subfamvalidData = read.csv("normsubfamvalid.csv")

subfamtestData = subfamtestData[,-c(1:10,12:20,22,23,25:31,33:39)]
subfamtrainData = subfamtrainData[,-c(1:10,12:20,22,23,25:31,33:39)]
subfamvalidData = subfamvalidData[,-c(1:10,12:20,22,23,25:31,33:39)]

subfamtrainLabels = subfamtrainData[,1]
subfamvalidLabels = subfamvalidData[,1]
subfamtestLabels = subfamtestData[,1]

num.subfamtrain = subfamtrainData[, sapply(subfamtrainData, is.numeric)]
num.subfamvalid = subfamvalidData[, sapply(subfamvalidData, is.numeric)]
num.subfamtest = subfamtestData[, sapply(subfamtestData, is.numeric)]


#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = nrow(num.subfamvalid), ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = num.subfamtrain, test = num.subfamvalid, cl = subfamtrainLabels, k=n)
}

merge = data.frame(subfamvalidLabels, pred)

#K vs Accuracy

i = 1
j = 1
True = 0
subfamaccuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
subfamaccuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  subfamaccuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(subfamaccuracy) = c("K", "Accuracy")

taxonaccuracy[8,1] = "Subfamily"
taxonaccuracy[8,2] = max(subfamaccuracy[,2])

taxonaccuracy[,2]=round(taxonaccuracy[,2],3)
taxonaccuracy$Rank <- factor(taxonaccuracy$Rank, levels = taxonaccuracy$Rank)

ggplot(data=taxonaccuracy, aes(x=Rank, y=Accuracy)) +
     geom_bar(stat="identity", fill="steelblue")+
     geom_text(aes(label=Accuracy), vjust=1.6, color="white", size=3.5)+
     theme(axis.text.x = element_text(angle = 90, hjust = 0))+
     ylim(0, 1)
