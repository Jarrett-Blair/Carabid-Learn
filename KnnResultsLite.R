m = 1
for(m in 1:8){
  if(m == 1){
    results[m,1] = "Species"
    list = autoknn("Species", num.sptrain, num.spvalid, sptrainLabels, spvalidLabels, nrow(num.spvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    sp.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    speciespr = list$pr
  }
  if(m == 2){
    results[m,1] = "Group"
    list = autoknn("Group", num.grouptrain, num.groupvalid, grouptrainLabels, groupvalidLabels, nrow(num.groupvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    group.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    grouppr = list$pr
  }
  if(m == 3){
    results[m,1] = "Subgenus"
    list = autoknn("Subgenus", num.subgenustrain, num.subgenusvalid, subgenustrainLabels, subgenusvalidLabels, nrow(num.subgenusvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    subgenus.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    subgenuspr = list$pr
  }
  if(m == 4){
    results[m,1] = "Genus"
    list = autoknn("Genus", num.genustrain, num.genusvalid, genustrainLabels, genusvalidLabels, nrow(num.genusvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    genus.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    genuspr = list$pr
  }
  if(m == 5){
    results[m,1] = "Subtribe"
    list = autoknn("Subtribe", num.subtribtrain, num.subtribvalid, subtribtrainLabels, subtribvalidLabels, nrow(num.subtribvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    subtrib.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    subtribpr = list$pr
  }
  if(m == 6){
    results[m,1] = "Tribe"
    list = autoknn("Tribe", num.tribtrain, num.tribvalid, tribtrainLabels, tribvalidLabels, nrow(num.tribvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    trib.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    tribpr = list$pr
  }
  if(m == 7){
    results[m,1] = "Supertribe"
    list = autoknn("Supertribe", num.suptribtrain, num.suptribvalid, suptribtrainLabels, suptribvalidLabels, nrow(num.suptribvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    suptrib.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    suptribpr = list$pr
  }
  if(m == 8){
    results[m,1] = "Subfamily"
    list = autoknn("Subfamily", num.subfamtrain, num.subfamvalid, subfamtrainLabels, subfamvalidLabels, nrow(num.subfamvalid))
    results[m,2] = list$Accuracy
    results[m,3] = list$trueprob
    results[m,4] = list$falseprob
    results[m,5] = list$Precision
    results[m,6] = list$Recall
    results[m,7] = list$F1
    results[m,8] = list$k
    trueprobdistr = as.data.frame(list$trueprobdistr)
    falseprobdistr = as.data.frame(list$falseprobdistr)
    subfam.prcurve = pr.curve(list$trueprobdistr, list$falseprobdistr, curve = T)
    subfampr = list$pr
  }
}