library(e1071)
library(caret)
library(reshape2)

bayesresults = data.frame(matrix(NA, nrow = 8, ncol = 9))
names(bayesresults) = c("Rank", "Categories", "Accuracy", "Top 3 Accuracy", "True Prob", "False Prob", "Precision", "Recall", "F1")
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

carabidbayes = function(rank, traindat, testdat, trainlab, testlab){
  
  rankabund = as.data.frame(table(testlab))
  
  if(rank == "Species"){
    car.bayes = naiveBayes(SpeciesName ~., data = traindat)
  }
  if(rank == "Group"){
    car.bayes = naiveBayes(Group ~., data = traindat)
  }
  if(rank == "Subgenus"){
    car.bayes = naiveBayes(Subgenus ~., data = traindat)
  }
  if(rank == "Genus"){
    car.bayes = naiveBayes(Genus ~., data = traindat)
  }
  if(rank == "Subtribe"){
    car.bayes = naiveBayes(Subtribe ~., data = traindat)
  }
  if(rank == "Tribe"){
    car.bayes = naiveBayes(Tribe ~., data = traindat)
  }
  if(rank == "Supertribe"){
    car.bayes = naiveBayes(Supertribe ~., data = traindat)
  }
  if(rank == "Subfamily"){
    car.bayes = naiveBayes(Subfamily ~., data = traindat)
  }
  pred = predict(car.bayes, testdat)
  prob = predict(car.bayes, testdat, type = "raw")
  confmat = confusionMatrix(testlab, pred)
  preddf = data.frame(testlab, pred)
  
  pr = data.frame(matrix(NA, nrow = nrow(rankabund), ncol = 3))
  
  i = 1
  j = 1
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
        if(preddf[j,1] == preddf[j,2]){
          Tp = Tp + 1
          correct = correct + 1
          truetotal = truetotal + prob[j,i]
        }
        else{
          Fn = Fn + 1
        }
      } 
      if(preddf[j,2] == rankabund[i,1] & preddf[j,1] != rankabund[i,1]){
        Fp = Fp +1
        incorrect = incorrect + 1
        falsetotal = falsetotal + prob[j,i]
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
  
  falseprob = falsetotal/incorrect
  trueprob = truetotal/correct
  
  pr = data.frame(rankabund, pr)
  names(pr) = c(rank, "Abundance", "Precision", "Recall", "F1")
  pr[is.nan(pr)] = 0
  
  top3accuracy = data.frame(matrix(NA, nrow = nrow(prob), ncol = 3))
  m = 0
  for(i in 1:nrow(prob)){
    top3accuracy[i,1] = names(which.max(prob[i,]))
    top3accuracy[i,2] = names(which.max(prob[i, -grep(top3accuracy[i,1], colnames(prob))]))
    m = c(top3accuracy[i,1], top3accuracy[i,2])
    top3accuracy[i,3] = names(which.max(prob[i, -grep(paste(m, collapse = "|"), colnames(prob))]))
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
  
  mylist = list("Accuracy" = confmat$overall[1], "top3accuracy" = (top1 + top2 + top3)/length(testlab), "trueprob" = trueprob, "falseprob" = falseprob, "Precision" = mean(pr[,3]), "Recall" = mean(pr[,4]), "F1" = mean(pr[,5]), "pr" = pr, "Categories" = nlevels(trainlab))
  
  return(mylist)
}

getbayesresults = function(plottype){
  m = 1
  for(m in 1:8){
    if(m == 1){
      bayesresults[m,1] = "Species"
      list = carabidbayes("Species", sptrainData, spvalidData, sptrainLabels, spvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Group"
      list = carabidbayes("Group", grouptrainData, groupvalidData, grouptrainLabels, groupvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Subgenus"
      list = carabidbayes("Subgenus", subgenustrainData, subgenusvalidData, subgenustrainLabels, subgenusvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Genus"
      list = carabidbayes("Genus", genustrainData, genusvalidData, genustrainLabels, genusvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Subtribe"
      list = carabidbayes("Subtribe", subtribtrainData, subtribvalidData, subtribtrainLabels, subtribvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Tribe"
      list = carabidbayes("Tribe", tribtrainData, tribvalidData, tribtrainLabels, tribvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Supertribe"
      list = carabidbayes("Supertribe", suptribtrainData, suptribvalidData, suptribtrainLabels, suptribvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
      bayesresults[m,1] = "Subfamily"
      list = carabidbayes("Subfamily", subfamtrainData, subfamvalidData, subfamtrainLabels, subfamvalidLabels)
      bayesresults[m,2] = list$Categories
      bayesresults[m,3] = list$Accuracy
      bayesresults[m,4] = list$top3accuracy
      bayesresults[m,5] = list$trueprob
      bayesresults[m,6] = list$falseprob
      bayesresults[m,7] = list$Precision
      bayesresults[m,8] = list$Recall
      bayesresults[m,9] = list$F1
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
  return(bayesresults)
}

bayesresults = getbayesresults("none")
bayesresults[,3:9]=round(bayesresults[,3:9],3)
mbayesresults = melt(bayesresults, id.vars = "Rank")
names(mbayesresults) = c("Rank", "Variables", "Value")
mbayesresults$Value = round(mbayesresults$Value, digits = 3)
Accuracy = subset(mbayesresults, Variables %in% "Accuracy")
top3accuracy = subset(mbayesresults, Variables %in% "Top 3 Accuracy")
prob = subset(mbayesresults, Variables %in% c("True Prob", "False Prob"))
prf = subset(mbayesresults, Variables %in% c("Precision", "Recall", "F1"))
order = c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")

ggplot(Accuracy, aes(Rank,Value)) + 
  geom_bar(position = "dodge", stat="identity", fill = "#000080")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(name = "Accuracy", limits = c(0,1))+
  guides(fill=FALSE) +
  geom_text(aes(label = Value), position = position_dodge(0.1), vjust = -0.5) +
  ggtitle("Accuracy of Carabid Naive Bayes model at eight taxonomic ranks")+
  geom_point(data = bayesresults, aes(x = Rank, y = 1/Categories), color= "#FFFFFF")+
  geom_text(aes(label = sprintf(print(paste0("n = ", bayesresults$Categories)), digits = 3)), hjust = .5, vjust = 2, color= "#FFFFFF") +
  theme(
    plot.title = element_text(hjust = 0.5))

ggplot(prob, aes(Rank,Value, fill=Variables)) + 
  geom_bar(position = "dodge", stat="identity")+ 
  scale_x_discrete(limits=c("Species", "Group", "Subgenus", "Genus", "Subtribe", "Tribe", "Supertribe", "Subfamily")) +
  scale_y_continuous(name = "Vote Share", limits = c(0,1))+
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = Value), position = position_dodge(0.9), vjust = -0.5) +
  ggtitle("(Naive Bayes) Vote share of true and false predictions across all modelled taxa (PCA)")+
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
  ggtitle("(Naive Bayes) Precision/Recall/F1")+
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
  ggtitle("Top 1 & 3 Accuracy of Carabid Naive Bayes model at eight taxonomic ranks (PCA)")+
  geom_point(data = bayesresults, aes(x = Rank, y = 3/Categories), color= "blue")+
  geom_text(aes(label = sprintf(print(paste0("n = ", c(37, 31, 28, 20, 11, 11, 7, 6))), digits = 3)), hjust = .5, vjust = 2)+ 
  ######################
geom_text(data = Accuracy, mapping = aes(x = Rank, y = Value, label = Value), position = position_dodge(0.1), vjust = -0.5) +
  geom_point(data = bayesresults, aes(x = Rank, y = 1/Categories), color= "forestgreen")+
  theme(
    plot.title = element_text(hjust = 0.5))

