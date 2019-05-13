library(class)
library(gmodels)
library(ggplot2)
library(pvclass)
library(reshape2)
setwd("C:/Carabid_Data/Carabid")

taxonprob = data.frame(matrix(NA, nrow = 8, ncol = 3))
df = data.frame(matrix(NA, nrow = 24, ncol = 3))
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

results = data.frame(matrix(NA, nrow = 8, ncol = 7))
names(results) = c("Rank", "Accuracy", "True Prob", "False Prob", "Precision", "Recall", "F1")
numk = 50

#AutoKnn function
autoknn = function(traindat, testdat, trainlab, testlab, numrow, numk = 50){
  n = 1
  pred = data.frame(matrix(NA, nrow = numrow, ncol = numk))
  for(n in 1:numk){
    pred[, n] = knn(train = traindat, test = testdat, cl = trainlab, k=n)
  }
  merge = data.frame(testlab, pred)
  
#Accuracy Calculator
  i = 1
  j = 1
  True = 0
  accuracy = data.frame(matrix(NA, nrow = numk, ncol = 2))
  accuracy[,1] = c(1:numk)
  for(j in 2:ncol(merge)){
    for(i in 1:nrow(merge)) {
      if(merge[i,1] == merge[i,j]){
        True = True+1
      }
    }
    accuracy[j-1,2] = True/nrow(merge)
    True = 0
    i = 1
  }
  
  #Prob returns the vote share of the winning category
  prob = data.frame(matrix(NA, nrow = nrow(pred), ncol = 2))
  prob[,1] = knn(train = traindat, test = testdat, cl = trainlab, k=which.max(accuracy[,2]))
  prob[,2] = attr(knn(train = traindat, test = testdat, cl = trainlab, k=which.max(accuracy[,2]), prob = TRUE), "prob")
  mergeprob = data.frame(testlab, prob)
  names(mergeprob) = c("Real", "Pred", "Prob")
  bestk = which.max(accuracy[,2])
  
  #Average prob for rank (Correct and Incorrect Results)
  i = 1
  correct = 0
  incorrect = 0
  truetotal = 0
  falsetotal = 0
  for (i in 1:nrow(mergeprob)){
    if (mergeprob[i,1] == mergeprob[i,2]){
      correct = correct + 1
      truetotal = truetotal + mergeprob[i,3]
    }
    else{
      incorrect = incorrect + 1
      falsetotal = falsetotal + mergeprob[i,3]
    }
  }
  truetaxonprob = truetotal/correct
  falsetaxonprob = falsetotal/incorrect
  
  probdist = data.frame(matrix(NA, nrow = nrow(mergeprob), ncol = 2))
  names(probdist) = c("True", "False")
  i = 1
  total = 0
  for (i in 1:nrow(mergeprob)){
    if (mergeprob[i,1] == mergeprob[i,2]){
      probdist[i,1] = mergeprob[i,3]
    }
    else{
      probdist[i,2] = mergeprob[i,3]
    }
  }
  
  #Precision/recall/F1 calculator
  names = data.frame(levels(trainlab))
  max = data.frame(testlab, pred[,which.max(accuracy[,2])])
  
  pr = data.frame(matrix(NA, nrow = nrow(names), ncol = 3))
  
  i = 1
  j = 1
  Tp = 0
  Fp = 0
  Tn = 0
  Fn = 0
  for(i in 1:nrow(names)){
    for(j in 1:nrow(max)){
      if(max[j,1] == names[i,1]){
        if(max[j,1] == max[j,2]){
          Tp = Tp + 1
        }
        else{
          Fn = Fn + 1
        }
      } 
      if(max[j,2] == names[i,1] & max[j,1] != names[i,1]){
        Fp = Fp +1
      }
      else{
        Tn = Tn + 1
      }
    }
    pr[i,1] = Tp/(Tp + Fp)
    pr[i,2] = Tp/(Tp + Fn)
    pr[i,3] = 2*((pr[i,1]*pr[i,2])/(pr[i,1]+pr[i,2]))
    j = 1
    Tp = Tn = Fn = Fp = 0
  }
  
  pr = data.frame(names, pr)
  names(pr) = c("Names", "Precision", "Recall", "F1")
  pr[is.nan(pr)] = 0
  mean(pr[,2])
  mean(pr[,3])
  mean(pr[,4])
  
  mylist = list("Accuracy" = max(accuracy[,2]), "trueprob" = truetaxonprob, "falseprob" = falsetaxonprob, "Precision" = mean(pr[,2]), "Recall" = mean(pr[,3]), "F1" = mean(pr[,4]), "probdist" = probdist, "k" = bestk)
  
  return(mylist)
}

#############           #############
#############  Species  #############
#############           #############

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

#############         #############
#############  Group  #############
#############         #############

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

#############            #############
#############  Subgenus  #############
#############            #############

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

############         #############
############  GENUS  #############
############         #############

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

#############            #############
#############  Subtribe  #############
#############            #############

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

#############         #############
#############  Tribe  #############
#############         #############

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

#############              #############
#############  Supertribe  #############
#############              #############

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

#############             #############
#############  Subfamily  #############
#############             #############

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


m = 1
for(m in 1:8){
  if(m == 1){
    results[m,1] = "Species"
    list = autoknn(num.sptrain, num.spvalid, sptrainLabels, spvalidLabels, nrow(num.spvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Species")
  }
  if(m == 2){
    results[m,1] = "Group"
    list = autoknn(num.grouptrain, num.groupvalid, grouptrainLabels, groupvalidLabels, nrow(num.groupvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Group")
  }
  if(m == 3){
    results[m,1] = "Subgenus"
    list = autoknn(num.subgenustrain, num.subgenusvalid, subgenustrainLabels, subgenusvalidLabels, nrow(num.subgenusvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Subgenus")
  }
  if(m == 4){
    results[m,1] = "Genus"
    list = autoknn(num.genustrain, num.genusvalid, genustrainLabels, genusvalidLabels, nrow(num.genusvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Genus")
  }
  if(m == 5){
    results[m,1] = "Subtribe"
    list = autoknn(num.subtribtrain, num.subtribvalid, subtribtrainLabels, subtribvalidLabels, nrow(num.subtribvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Subtribe")
  }
  if(m == 6){
    results[m,1] = "Tribe"
    list = autoknn(num.tribtrain, num.tribvalid, tribtrainLabels, tribvalidLabels, nrow(num.tribvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Tribe")
  }
  if(m == 7){
    results[m,1] = "Supertribe"
    list = autoknn(num.suptribtrain, num.suptribvalid, suptribtrainLabels, suptribvalidLabels, nrow(num.suptribvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Supertribe")
  }
  if(m == 8){
    results[m,1] = "Subfamily"
    list = autoknn(num.subfamtrain, num.subfamvalid, subfamtrainLabels, subfamvalidLabels, nrow(num.subfamvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    trueprobdist = as.data.frame(table(list$probdist[,1]))
    falseprobdist = as.data.frame(table(list$probdist[,2]))
    trueprobdist[,1] = as.numeric(levels(trueprobdist[,1]))[trueprobdist[,1]]
    falseprobdist[,1] = as.numeric(levels(falseprobdist[,1]))[falseprobdist[,1]]
    plot(trueprobdist[,1], trueprobdist[,2], xlab = "Probability", ylab = "Frequency", main = "Subfamily")
  }
}

results[,2]=round(results[,2],3)
results$Rank <- factor(results$Rank, levels = results$Rank)

ggplot(data=results, aes(x=Rank, y=Accuracy)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Accuracy), vjust=1.5, color="white", size=3.5)+
    geom_text(aes(label=results[,9]), vjust=3, color="white", size=3.5)+
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          plot.caption = element_text(hjust = 0))+
    ylim(0, 1)

mresults = melt(results, id.vars = "Rank")
names(mresults) = c("Rank", "Variables", "Value")
mresults$Value = round(mresults$Value, digits = 3)
Accuracy = subset(mresults, Variables %in% "Accuracy")
prob = subset(mresults, Variables %in% c("True Prob", "False Prob"))
prf = subset(mresults, Variables %in% c("Precision", "Recall", "F1"))
order = c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")


ggplot(mresults, aes(Rank,Value, fill=Variables)) + 
    geom_bar(position = "dodge", stat="identity")+ 
    scale_x_discrete(limits=order) +
    scale_fill_brewer(palette = "Paired") +
    geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
    ggtitle("All Results")+
    theme(
      panel.background = element_rect(fill = "#BFD5E3", colour = "#BFD5E3",
                                    size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
      plot.title = element_text(hjust = 0.5))
 
ggplot(Accuracy, aes(Rank,Value, fill = Rank)) + 
  geom_bar(position = "dodge", stat="identity")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_fill_viridis(discrete = T, option = "D", limits = rev(order)) +
  guides(fill=FALSE) +
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
  ggtitle("Accuracy")+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#BFD5E3",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(hjust = 0.5))

ggplot(prob, aes(Rank,Value, fill=Variables)) + 
  geom_bar(position = "dodge", stat="identity")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
  ggtitle("Probability")+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#BFD5E3",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(hjust = 0.5))

ggplot(prf, aes(Rank,Value, fill=Variables)) + 
  geom_bar(position = "dodge", stat="identity")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_fill_brewer(palette = "Accent") +
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
  ggtitle("Precision/Recall/F1")+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#BFD5E3",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(hjust = 0.5))