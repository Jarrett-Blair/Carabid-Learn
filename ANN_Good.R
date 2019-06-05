autoann = function(rank, traindat, testdat, trainlab, testlab, numrow, numk = 25){
  #Data frame with number of observations in each category in test data
  rankabund = as.data.frame(table(testlab))
  
  #ANN
  i = 1 #value of k
  j = 1 #value of current testdat row
  n = 1 #Used for converting index numbers; goes from 1 to i
  x = 1 #Row of results table for each test point
  y = 1
  ann = list()
  pred = data.frame(matrix(NA, nrow = nrow(testdat), ncol = numk))
  vote = data.frame(matrix(NA, nrow = nrow(testdat), ncol = numk))
  for(i in 1:numk){
    ann[[i]] = nn2(traindat, query = testdat, k = i, treetype = "bd", searchtype = "priority") #Create list of length k containing each result
    for(j in 1:nrow(testdat)){
      for(n in 1:i){#converts index numbers category number (i.e. index number of levels(trainlab))
        ann[[i]]$nn.idx[j,n] = trainlab[ann[[i]]$nn.idx[j,n]] 
      }
      df = as.data.frame(table(unlist(ann[[i]]$nn.idx[j,])), stringsAsFactors=FALSE)
      df = df[order(df[,2], decreasing = T),]
      for(x in 1:length(df[,1])){
        df$Var1[x] = levels(trainlab)[as.numeric(df$Var1[x])]
        pred[j,x] = df$Var1[x]
        vote[j,x] = df[x,2]
      }
    }
  }
  mylist = list("ann" = ann, "pred" = pred, "vote" = vote, "test" = test, "test2" = test2)
  return(mylist)
}

list = autoann("Species", num.sptrain, num.spvalid, sptrainLabels, spvalidLabels, nrow(num.spvalid))
