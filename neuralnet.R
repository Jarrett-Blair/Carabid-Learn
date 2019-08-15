libary(neuralnet)
n = names(sptrainData)
f = as.formula(paste("SpeciesName ~", paste(n[!n %in% "SpeciesName"], collapse = " + ")))
soft.plus <- function(x) log(1 + exp(x))
nn = neuralnet(f, data = sptrainData, hidden = c(5,3), linear.output = F, threshold = 0.5, act.fct = soft.plus, rep = 10)

train = cbind(sptrainData[, 2:14], class.ind(as.factor(sptrainData$SpeciesName)))
pr.nn = compute(nn, train[,1:13])
pr.nn_ = pr.nn$net.result
original_values = max.col(train[,14:ncol(train)])
pr.nn_2 = max.col(pr.nn_)
mean(pr.nn_2 == original_values)

valid = cbind(spvalidData[, 2:14], class.ind(as.factor(spvalidData$SpeciesName)))
pr.nn = compute(nn, valid[,1:13])
pr.nn_ = pr.nn$net.result
original_values = max.col(valid[,14:ncol(valid)])
pr.nn_2 = max.col(pr.nn_)
mean(pr.nn_2 == original_values)

for(i in 1:nrow(pr.nn_)){
  top3accuracy[i,1] = which.max(pr.nn_[i,])
  top3accuracy[i,2] = which.max(pr.nn_[i, -c(top3accuracy[i,1])])
  m = c(top3accuracy[i,1], top3accuracy[i,2])
  top3accuracy[i,3] = which.max(pr.nn_[i, -c(top3accuracy[i,1], top3accuracy[i,2])])
}
top3accuracy = data.frame(original_values, top3accuracy)
top1 = 0
top2 = 0
top3 = 0
for(j in 1:length(original_values)){
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