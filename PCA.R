library("FactoMineR")
library(ggbiplot)
setwd("C:/Carabid_Data")
my.files <- list.files()
compiled.data <- NA
current.data <- NA

for(i in 1:length(my.files))
	{

	current.data <- read.csv(my.files[i])
	if(i>1){
		names(compiled.data) = names(current.data)
	}
	compiled.data <- rbind(compiled.data,current.data)

}
compiled.data = compiled.data[-1,]
carabid = compiled.data
spname = carabid$Species.Name
levels(spname)
table(spname)
carabid.numeric = carabid[,sapply(carabid, is.numeric)]
carabid.numeric$Individual = NULL
carabid.numeric$ROI = NULL
carabid.pca = prcomp(carabid.numeric, scale = TRUE,center=TRUE)
summary(carabid.pca)
screeplot(carabid.pca, npcs = 21, type = "lines")
results = PCA(carabid.numeric)
str(carabid.pca)
loadings <-  carabid.pca$rotation
head(loadings)
loadings <- as.data.frame(loadings)
loadings$PC1 #Change value to PC2, PC3 ... PCn to see each PC
row.names(loadings)#Helpful to match up values with each PC
pc.vals <- carabid.pca$x
pc.vals <- as.data.frame(pc.vals)
carabid$pc1 <- pc.vals$PC1
head(carabid)
boxplot(pc1~Species.Name,data=carabid)
