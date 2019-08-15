annresults = data.frame(matrix(NA, nrow = 8, ncol = 6))
names(annresults) = c("Rank", "Categories", "Accuracy", "Precision", "Recall", "F1")


autoann = function(rank, traindat, testdat, trainlab, testlab, numrow){
  #Data frame with number of observations in each category in test data
  rankabund = as.data.frame(table(testlab))
  
  #ANN
  ann = nn2(traindat, query = testdat, treetype = "bd", searchtype = "priority")
  idx = ann$nn.idx
  i = 1
  corr = 0
  for(i in 1:nrow(testdat)){
    if(trainlab[idx[i]] == testlab[i]){
      corr = corr+1
    }
  }
  accuracy = corr/nrow(testdat)
  
  #Precision/recall/F1 calculaton
  
  pr = data.frame(matrix(NA, nrow = nrow(rankabund), ncol = 3))
  
  i = 1
  j = 1
  Tp = 0
  Fp = 0
  Tn = 0
  Fn = 0
  for(i in 1:nrow(rankabund)){
    for(j in 1:nrow(testdat)){
      if(testlab[j] == rankabund[i,1]){
        if(testlab[j] == trainlab[idx[j]]){
          Tp = Tp + 1
        }
        else{
          Fn = Fn + 1
        }
      } 
      if(trainlab[idx[j]] == rankabund[i,1] & testlab[j] != rankabund[i,1]){
        Fp = Fp +1
      }
    }
    pr[i,1] = Tp/(Tp + Fp)
    pr[i,2] = Tp/(Tp + Fn)
    pr[i,3] = 2*((pr[i,1]*pr[i,2])/(pr[i,1]+pr[i,2]))
    j = 1
    Tp = Tn = Fn = Fp = 0
  }
  
  pr = data.frame(rankabund, pr)
  names(pr) = c(rank, "Abundance", "Precision", "Recall", "F1")
  pr[is.nan(pr)] = 0
  
  mylist = list("Accuracy" = accuracy, "Precision" = mean(pr[,3]), "Recall" = mean(pr[,4]), "F1" = mean(pr[,5]), "pr" = pr, "Categories" = nlevels(trainlab))
  
  return(mylist)
}
getannresults = function(plottype){
  m = 1
  for(m in 1:8){
    if(m == 1){
      annresults[m,1] = "Species"
      list = autoann("Species", num.sptrain, num.spvalid, sptrainLabels, spvalidLabels, nrow(num.spvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Group"
      list = autoann("Group", num.grouptrain, num.groupvalid, grouptrainLabels, groupvalidLabels, nrow(num.groupvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Subgenus"
      list = autoann("Subgenus", num.subgenustrain, num.subgenusvalid, subgenustrainLabels, subgenusvalidLabels, nrow(num.subgenusvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Genus"
      list = autoann("Genus", num.genustrain, num.genusvalid, genustrainLabels, genusvalidLabels, nrow(num.genusvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Subtribe"
      list = autoann("Subtribe", num.subtribtrain, num.subtribvalid, subtribtrainLabels, subtribvalidLabels, nrow(num.subtribvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Tribe"
      list = autoann("Tribe", num.tribtrain, num.tribvalid, tribtrainLabels, tribvalidLabels, nrow(num.tribvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Supertribe"
      list = autoann("Supertribe", num.suptribtrain, num.suptribvalid, suptribtrainLabels, suptribvalidLabels, nrow(num.suptribvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
      annresults[m,1] = "Subfamily"
      list = autoann("Subfamily", num.subfamtrain, num.subfamvalid, subfamtrainLabels, subfamvalidLabels, nrow(num.subfamvalid))
      annresults[m,2] = list$Categories
      annresults[m,3] = list$Accuracy
      annresults[m,4] = list$Precision
      annresults[m,5] = list$Recall
      annresults[m,6] = list$F1
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
  return(annresults)
}
annresults = getannresults("none")