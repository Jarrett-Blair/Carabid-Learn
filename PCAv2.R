library("FactoMineR")
library(ggbiplot)
setwd("C:/Carabid_Data")

spname = carabid.dorsal$SpeciesName
genus = carabid.dorsal$Genus
#levels(spname)
#table(spname)

carabid.numeric = carabid.dorsal[,sapply(carabid.dorsal, is.numeric)]
carabid.numeric.1 = carabid.numeric[, -c(1:3,5,6,8,9,19,20)] 
#Numeric 1 includes Width, height, major and minor
carabid.numeric.2 = carabid.numeric.1[,-c(3:7)]
#Numeric two is only area, perim, angle, circ, ar, round and solid

carabid.pca.1 = prcomp(carabid.numeric.1, scale = TRUE,center=TRUE)
carabid.pca.2 = prcomp(carabid.numeric.2, scale = TRUE,center=TRUE)

summary(carabid.pca.1)
screeplot(carabid.pca.1, npcs = 21, type = "lines")
results = PCA(carabid.numeric.1)
#str(carabid.pca.1)
loadings <-  carabid.pca.1$rotation
#head(loadings)
loadings <- as.data.frame(loadings)

loadings$PC1 #Change value to PC2, PC3 ... PCn to see each PC
row.names(loadings)#Helpful to match up values with each PC
loadings$PC2
row.names(loadings)
loadings$PC3
row.names(loadings)
loadings$PC4
row.names(loadings)

pc.vals <- carabid.pca.1$x
pc.vals <- as.data.frame(pc.vals)
carabid.dorsal$pc1 <- pc.vals$PC1
carabid.dorsal$pc2 <- pc.vals$PC2
carabid.dorsal$pc3 <- pc.vals$PC3
carabid.dorsal$pc4 <- pc.vals$PC4
#head(carabid.dorsal)
boxplot(pc1~SpeciesName,data=carabid.dorsal)
boxplot(pc2~SpeciesName,data=carabid.dorsal)
boxplot(pc3~SpeciesName,data=carabid.dorsal)
boxplot(pc4~SpeciesName,data=carabid.dorsal)

boxplot(pc1~Genus,data=carabid.dorsal)
boxplot(pc2~Genus,data=carabid.dorsal)
boxplot(pc3~Genus,data=carabid.dorsal)
boxplot(pc4~Genus,data=carabid.dorsal)

n <- 33 #33 is number of genera in carabid dataset. Use different number if for species
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

ggbiplot(carabid.pca.1,ellipse=TRUE, groups=genus) + scale_colour_manual(values = col_vector) + guides(fill=guide_legend(nrow=2))
ggbiplot(carabid.pca.1,ellipse=TRUE, labels = NA, groups=genus) + scale_colour_manual(values = col_vector) + guides(fill=guide_legend(nrow=2))