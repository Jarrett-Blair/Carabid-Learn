library(class)
library(gmodels)
library(pvclass)
setwd("C:/Carabid_Data")

testData= read.csv("dorsal.norm.testData.genus.csv")
trainData = read.csv("dorsal.norm.trainData.genus.csv")
validData = read.csv("dorsal.norm.validData.genus.csv")

testData = testData[,-c(3,4,6,7,12,17:19)]
trainData = trainData[,-c(3,4,6,7,12,17:19)]
validData = validData[,-c(3,4,6,7,12,17:19)]

trainLabels = trainData[,1]
validLabels = validData[,1]
testLabels = testData[,1]

num.train = trainData[, sapply(trainData, is.numeric)]
num.valid = validData[, sapply(validData, is.numeric)]
num.test = testData[, sapply(testData, is.numeric)]

pred3 = knn(train = , test = num.valid, cl = trainLabels, k=3)
dfvalidLabels = data.frame(validLabels)
merge3 = data.frame(pred3, validLabels)
names(merge3) = c("Predicted", "Observed")
merge3


xtable3 = CrossTable(x = validLabels, y = pred3, prop.chisq = FALSE)
xtable10 = CrossTable(x = validLabels, y = pred10, prop.chisq = FALSE)

#Accuracy
i = 1
True = 0
while (i <= nrow(merge3)) {
  if (merge3[i,1] == merge3[i,j]){
    True = True+1
  }
  i = i+1
}
print(True/nrow(merge3))

#Automatic KNN
n = 1
pred = data.frame(matrix(NA, nrow = 614, ncol = 50))
for(n in 1:50){
  pred[, n] = knn(train = , test = num.valid, cl = trainLabels, k=n)
}

#K vs Accuracy
pred = data.frame(matrix(NA, nrow = 614, ncol = 50))
i = 1
j = 1
True = 0
accuracy = data.frame(matrix(NA, nrow = 50, ncol = 2))
accuracy[,1] = c(1:50)
for(j in 2:ncol(merge)){
  for (i in 1:nrow(merge)) {
    if (merge[i,1] == merge[i,j]){
      True = True+1
    }
  }
  accuracy[j-1,2] = True/nrow(merge)
  True = 0
  i = 1
}
names(accuracy) = c("K", "Accuracy")
plot(accuracy$K,accuracy$Accuracy, xlab = "K", ylab = "Accuracy", main = "k-Nearest Neighbors for Carabid Genera")

pvs1 = pvs.knn(num.valid, num.train, trainLabels, k = 3)
