library(RANN)
library(nabor)
kresults = data.frame(matrix(NA, nrow = 25, ncol = 10))
performance = data.frame(matrix(NA, nrow = 3, ncol = 2))
autoann = function(rank, traindat, testdat, trainlab, testlab, numrow, numk = 25){
  #Data frame with number of observations in each category in test data
  rankabund = as.data.frame(table(testlab))
  
  #ANN
  i = 1 #value of k
  j = 1 #value of current testdat row
  n = 1 #Used for converting index numbers; goes from 1 to i
  x = 1 #Row of results table for each test point
  top1 = 0
  top2 = 0
  top3 = 0
  predk = list()
  votek = list()
  pred = data.frame(matrix(NA, nrow = nrow(testdat), ncol = numk))
  vote = data.frame(matrix(NA, nrow = nrow(testdat), ncol = numk))
  ann = nn2(traindat, query = testdat, k = numk, treetype = "bd", searchtype = "priority")
  
  for(j in 1:nrow(testdat)){
    for(n in 1:numk){
      ann$nn.idx[j,n] = trainlab[ann$nn.idx[j,n]]
    }
  }
  
  for(i in 1:numk){
    for(j in 1:nrow(testdat)){
      for(n in 1:i){
        df = as.data.frame(table(unlist(ann$nn.idx[j,1:i])), stringsAsFactors=FALSE)
        df = df[order(df[,2], decreasing = T),]
      }
      for(x in 1:length(df[,1])){
        df$Var1[x] = levels(trainlab)[as.numeric(df$Var1[x])]
        pred[j,x] = df$Var1[x]
        vote[j,x] = df[x,2]
      }
    }
    predk[[i]] = pred
    votek[[i]] = vote
  }
  for(i in 1:length(pred)){
    predk[[i]] = data.frame(testlab, predk[[i]])
    predi = predk[[i]]
    for(j in 1:length(testlab)){
      if(predi[j,1] == predi[j,2]){
        top1 = top1 + 1
      }
      if(is.na(predi[j,3]) == FALSE){
        if(predi[j,1] == predi[j,3]){
          top2 = top2 + 1
        }
      }
      if(is.na(predi[j,4]) == FALSE){
        if(predi[j,1] == predi[j,4]){
          top3 = top3 + 1
        }
      }
    }
    kresults[i,2] = top1/length(testlab)
    kresults[i,3] = (top1 + top2)/length(testlab)
    kresults[i,4] = (top1 + top2 + top3)/length(testlab)
    top1 = top2 = top3 = 0
  }
  performance[1,1] = which.max(kresults[,2])
  performance[2,1] = which.max(kresults[,3])
  performance[3,1] = which.max(kresults[,4])
  performance[1,2] = max(kresults[,2])
  performance[2,2] = max(kresults[,3])
  performance[3,2] = max(kresults[,4])
  return(performance)
}

performancesp = autoann("Species", num.sptrain, num.spvalid, sptrainLabels, spvalidLabels, nrow(num.spvalid))
performancegroup = autoann("Group", num.grouptrain, num.groupvalid, grouptrainLabels, groupvalidLabels, nrow(num.groupvalid))
performancesubgenus = autoann("subgenus", num.subgenustrain, num.subgenusvalid, subgenustrainLabels, subgenusvalidLabels, nrow(num.subgenusvalid))
performancegenus = autoann("genus", num.genustrain, num.genusvalid, genustrainLabels, genusvalidLabels, nrow(num.genusvalid))
performancesubtrib = autoann("subtrib", num.subtribtrain, num.subtribvalid, subtribtrainLabels, subtribvalidLabels, nrow(num.subtribvalid))
performancetrib = autoann("trib", num.tribtrain, num.tribvalid, tribtrainLabels, tribvalidLabels, nrow(num.tribvalid))
performancesuptrib = autoann("suptrib", num.suptribtrain, num.suptribvalid, suptribtrainLabels, suptribvalidLabels, nrow(num.suptribvalid))
performancesubfam = autoann("subfam", num.subfamtrain, num.subfamvalid, subfamtrainLabels, subfamvalidLabels, nrow(num.subfamvalid))

top3results = performance = data.frame(matrix(NA, nrow = 8, ncol = 2))
top3results[1:8,1] = c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")
top3results[1,2] = performancesp[3,2]
top3results[2,2] = performancegroup[3,2]
top3results[3,2] = performancesubgenus[3,2]
top3results[4,2] = performancegenus[3,2]
top3results[5,2] = performancesubtrib[3,2]
top3results[6,2] = performancetrib[3,2]
top3results[7,2] = performancesuptrib[3,2]
top3results[8,2] = performancesubfam[3,2]
names(top3results) = c("Rank", "Accuracy")

top3results[,2]=round(top3results[,2],3)
top3results$Rank <- factor(top3results$Rank, levels = top3results$Rank)


points = data.frame(matrix(NA, nrow = 8, ncol = 2))
points[1:8,1] = c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")
points[1:8,2] = c(37, 31, 28, 20, 11, 11, 7, 6)
names(points) = c("Rank", "Value")

ggplot(top3results, aes(Rank, Accuracy)) + 
  geom_bar(position = "dodge", stat="identity", fill = "#000080")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(name = "Top 3 Accuracy", limits = c(0,1))+
  guides(fill=FALSE) +
  geom_text(aes(label = Accuracy), position = position_dodge(0.1), vjust = -0.5) +
  ggtitle("Top 3 Accuracy of Carabid KNN model at eight taxonomic ranks")+
  geom_point(data = points, aes(x = Rank, y = 3/Value), color= "#FFFFFF")+
  geom_text(aes(label = sprintf(print(paste0("n = ", c(37, 31, 28, 20, 11, 11, 7, 6))), digits = 3)), hjust = .5, vjust = 2, color= "#FFFFFF") +
  theme(
    plot.title = element_text(hjust = 0.5))

ggplot(top3results, aes(Rank, Accuracy)) + 
  geom_bar(position = "dodge", stat="identity", alpha=.3,fill='lightblue',color='lightblue4')+ 
  geom_bar(data = Accuracy, aes(y = Value), position = "dodge", stat = "identity", alpha = .3, fill = 'springgreen', color = 'springgreen4')+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(limits = c(0,1))+
  guides(fill=FALSE) +
  geom_text(aes(label = Accuracy), position = position_dodge(0.1), vjust = -0.5) +
  ggtitle("Top 1 & 3 Accuracy of Carabid KNN model at eight taxonomic ranks")+
  geom_point(data = points, aes(x = Rank, y = 3/Value), color= "blue")+
  geom_text(aes(label = sprintf(print(paste0("n = ", c(37, 31, 28, 20, 11, 11, 7, 6))), digits = 3)), hjust = .5, vjust = 2)+ 
  ######################
geom_text(data = Accuracy, mapping = aes(x = Rank, y = Value, label = Value), position = position_dodge(0.1), vjust = -0.5) +
  geom_point(data = results, aes(x = Rank, y = 1/Categories), color= "forestgreen")+
  theme(
    plot.title = element_text(hjust = 0.5))

