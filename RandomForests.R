library(randomForest)
rfresults = data.frame(matrix(NA, nrow = 8, ncol = 9))
names(rfresults) = c("Rank", "Categories", "Accuracy", "Top 3 Accuracy", "True Prob", "False Prob", "Precision", "Recall", "F1")

carabidforest = function(rank, traindat, testdat, testlab, mtrys){
  
  rankabund = as.data.frame(table(testlab))
  preddf = data.frame(matrix(NA, nrow = nrow(testdat), ncol = mtrys))
  prob = list()
  
  a=c()
  i=0
  
  if(rank == "Species"){
    for (i in 1:mtrys) {
      model <- randomForest(SpeciesName ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$SpeciesName)
    }
  }
  if(rank == "Group"){
    for (i in 1:mtrys) {
      model <- randomForest(Group ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Group)
    }
  }
  if(rank == "Subgenus"){
    for (i in 1:mtrys) {
      model <- randomForest(Subgenus ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Subgenus)
    }
  }
  if(rank == "Genus"){
    for (i in 1:mtrys) {
      model <- randomForest(Genus ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Genus)
    }
  }
  if(rank == "Subtribe"){
    for (i in 1:mtrys) {
      model <- randomForest(Subtribe ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Subtribe)
    }
  }
  if(rank == "Tribe"){
    for (i in 1:mtrys) {
      model <- randomForest(Tribe ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Tribe)
    }
  }
  if(rank == "Supertribe"){
    for (i in 1:mtrys) {
      model <- randomForest(Supertribe ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Supertribe)
    }
  }
  if(rank == "Subfamily"){
    for (i in 1:mtrys) {
      model <- randomForest(Subfamily ~ ., data = traindat, ntree = 500, mtry = i, importance = TRUE)
      preddf[,i] <- predict(model, testdat, type = "class")
      prob[[i]] = predict(model, testdat, type = "prob")
      a[i] = mean(preddf[,i] == testdat$Subfamily)
    }
  }
  bestmtry = which.max(a)
  bestprob = prob[[bestmtry]]
  preddf = data.frame(testlab, preddf)
  bestpred = bestmtry + 1
  
  pr = data.frame(matrix(NA, nrow = nrow(rankabund), ncol = 3))

  i = 0
  j = 0
  Tp = 0
  Fp = 0
  Tn = 0
  Fn = 0
  correct = 0
  incorrect = 0
  truetotal = 0
  falsetotal = 0
  for(i in 1:nrow(rankabund)){
    for(j in 1:nrow(preddf)){
      if(preddf[j,1] == rankabund[i,1]){
        if(preddf[j,1] == preddf[j,bestpred]){
          Tp = Tp + 1
          correct = correct + 1
          truetotal = truetotal + bestprob[j,i]
        }
        else{
          Fn = Fn + 1
        }
      }
      if(preddf[j,bestpred] == rankabund[i,1] & preddf[j,1] != rankabund[i,1]){
        Fp = Fp +1
        incorrect = incorrect + 1
        falsetotal = falsetotal + bestprob[j,i]
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

  trueprob = truetotal/correct
  falseprob = falsetotal/incorrect

  pr = data.frame(rankabund, pr)
  names(pr) = c(rank, "Abundance", "Precision", "Recall", "F1")
  pr[is.nan(pr)] = 0

  top3accuracy = data.frame(matrix(NA, nrow = nrow(bestprob), ncol = 3))
  m = 0
  for(i in 1:nrow(bestprob)){
    top3accuracy[i,1] = names(which.max(bestprob[i,]))
    top3accuracy[i,2] = names(which.max(bestprob[i, -grep(top3accuracy[i,1], colnames(bestprob))]))
    m = c(top3accuracy[i,1], top3accuracy[i,2])
    top3accuracy[i,3] = names(which.max(bestprob[i, -grep(paste(m, collapse = "|"), colnames(bestprob))]))
  }

  top3accuracy = data.frame(testlab, top3accuracy)
  top1 = 0
  top2 = 0
  top3 = 0
  for(j in 1:length(testlab)){
    if(top3accuracy[j,1] == top3accuracy[j,2]){
      top1 = top1 + 1
    }
    if(is.na(top3accuracy[j,3]) == FALSE){
      if(top3accuracy[j,1] == top3accuracy[j,3]){
        top2 = top2 + 1
      }
    }
    if(is.na(top3accuracy[j,4]) == FALSE){
      if(top3accuracy[j,1] == top3accuracy[j,4]){
        top3 = top3 + 1
      }
    }
  }

  mylist = list("Accuracy" = a[bestmtry], "top3accuracy" = (top1 + top2 + top3)/length(testlab), "trueprob" = trueprob, "falseprob" = falseprob, "Precision" = mean(pr[,3]), "Recall" = mean(pr[,4]), "F1" = mean(pr[,5]), "pr" = pr, "Categories" = nlevels(testlab))
  
  return(mylist)
}

getrfresults = function(plottype){
  m = 1
  for(m in 1:8){
    if(m == 1){
      rfresults[m,1] = "Species"
      list = carabidforest("Species", sptrainData, spvalidData, spvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Species")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Species Abundance/Recall Regression")
        splogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, splogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 2){
      rfresults[m,1] = "Group"
      list = carabidforest("Group", grouptrainData, groupvalidData, groupvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Group")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Group Abundance/Recall Regression")
        grouplogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, grouplogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 3){
      rfresults[m,1] = "Subgenus"
      list = carabidforest("Subgenus", subgenustrainData, subgenusvalidData, subgenusvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Subgenus")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Subgenus Abundance/Recall Regression")
        subgenlogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, subgenlogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 4){
      rfresults[m,1] = "Genus"
      list = carabidforest("Genus", genustrainData, genusvalidData, genusvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Genus")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Genus Abundance/Recall Regression")
        genlogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, genlogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 5){
      rfresults[m,1] = "Subtribe"
      list = carabidforest("Subtribe", subtribtrainData, subtribvalidData, subtribvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Subtribe")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Subtribe Abundance/Recall Regression")
        subtriblogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, subtriblogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 6){
      rfresults[m,1] = "Tribe"
      list = carabidforest("Tribe", tribtrainData, tribvalidData, tribvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Tribe")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Tribe Abundance/Recall Regression")
        triblogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, triblogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 7){
      rfresults[m,1] = "Supertribe"
      list = carabidforest("Supertribe", suptribtrainData, suptribvalidData, suptribvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Supertribe")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Supertribe Abundance/Recall Regression")
        suptriblogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, suptriblogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
    if(m == 8){
      rfresults[m,1] = "Subfamily"
      list = carabidforest("Subfamily", subfamtrainData, subfamvalidData, subfamvalidLabels, 13)
      rfresults[m,2] = list$Categories
      rfresults[m,3] = list$Accuracy
      rfresults[m,4] = list$top3accuracy
      rfresults[m,5] = list$trueprob
      rfresults[m,6] = list$falseprob
      rfresults[m,7] = list$Precision
      rfresults[m,8] = list$Recall
      rfresults[m,9] = list$F1
      if(plottype == "prcurve"){
        trueprobdistr = as.data.frame(list$trueprobdistr)
        falseprobdistr = as.data.frame(list$falseprobdistr)
        plot(pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T))
      }
      if(plottype == "probfreq"){
        trueprobdistr = as.data.frame(table(as.factor(trueprobdistr[,1])))
        falseprobdistr = as.data.frame(table(as.factor(falseprobdistr[,1])))
        trueprobdistr[,1] = as.numeric(levels(trueprobdistr[,1]))[trueprobdistr[,1]]
        falseprobdistr[,1] = as.numeric(levels(falseprobdistr[,1]))[falseprobdistr[,1]]
        plot(trueprobdistr[,1], trueprobdistr[,2], xlab = "Probability", ylab = "Frequency", main = "Subfamily")
      }
      if(plottype == "regression"){
        pr = list$pr
        xx = seq(min(pr$Abundance) - 5, max(pr$Abundance) + 5, length = 50)
        x = pr$Abundance
        y = pr$Recall
        d = data.frame(x,y)
        logEstimate = lm(y~log(x), data = d)
        plot(x,y,pch=19,ylim=c(0,1), main = "Subfamily Abundance/Recall Regression")
        subfamlogpred = predict(logEstimate, newdata = data.frame(x=xx))
        lines(xx, subfamlogpred)
        legend("bottomright", bty="n", legend = paste("R2 is", format(summary(logEstimate)$r.squared, digits = 4)))
      }
    }
  }
  return(rfresults)
}

rfresults = getrfresults("none")
rfresults[,3:9]=round(rfresults[,3:9],3)
mrfresults = melt(rfresults, id.vars = "Rank")
names(mrfresults) = c("Rank", "Variables", "Value")
mrfresults$Value = round(mrfresults$Value, digits = 3)
Accuracy = subset(mrfresults, Variables %in% "Accuracy")
top3accuracy = subset(mrfresults, Variables %in% "Top 3 Accuracy")
prob = subset(mrfresults, Variables %in% c("True Prob", "False Prob"))
prf = subset(mrfresults, Variables %in% c("Precision", "Recall", "F1"))
order = c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")

ggplot(Accuracy, aes(Rank,Value)) + 
  geom_bar(position = "dodge", stat="identity", fill = "#000080")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(name = "Accuracy", limits = c(0,1))+
  guides(fill=FALSE) +
  geom_text(aes(label = Value), position = position_dodge(0.1), vjust = -0.5) +
  ggtitle("Accuracy of Carabid rf model at eight taxonomic ranks")+
  geom_point(data = rfresults, aes(x = Rank, y = 1/Categories), color= "#FFFFFF")+
  geom_text(aes(label = sprintf(print(paste0("n = ", rfresults$Categories)), digits = 3)), hjust = .5, vjust = 2, color= "#FFFFFF") +
  theme(
    plot.title = element_text(hjust = 0.5))

ggplot(prob, aes(Rank,Value, fill=Variables)) + 
  geom_bar(position = "dodge", stat="identity")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(name = "Vote Share", limits = c(0,1))+
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
  ggtitle("(RF) Vote share of true and false predictions across all modelled taxa")+
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
  scale_y_continuous(name = "Value", limits = c(0,1))+
  scale_fill_brewer(palette = "Accent") +
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
  ggtitle("(RF) Precision/Recall/F1")+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#BFD5E3",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(hjust = 0.5))

ggplot(top3accuracy, aes(Rank, Value)) + 
  geom_bar(position = "dodge", stat="identity", alpha=.3,fill='lightblue',color='lightblue4')+ 
  geom_bar(data = Accuracy, aes(y = Value), position = "dodge", stat = "identity", alpha = .3, fill = 'springgreen', color = 'springgreen4')+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(limits = c(0,1))+
  guides(fill=FALSE) +
  geom_text(aes(label = Value), position = position_dodge(0.1), vjust = -0.5) +
  ggtitle("Top 1 & 3 Accuracy of Carabid RF model at eight taxonomic ranks")+
  geom_point(data = rfresults, aes(x = Rank, y = 3/Categories), color= "blue")+
  geom_text(aes(label = sprintf(print(paste0("n = ", c(37, 31, 28, 20, 11, 11, 7, 6))), digits = 3)), hjust = .5, vjust = 2)+ 
  ######################
geom_text(data = Accuracy, mapping = aes(x = Rank, y = Value, label = Value), position = position_dodge(0.1), vjust = -0.5) +
  geom_point(data = rfresults, aes(x = Rank, y = 1/Categories), color= "forestgreen")+
  theme(
    plot.title = element_text(hjust = 0.5))
