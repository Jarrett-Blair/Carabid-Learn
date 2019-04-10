setwd("C:/Carabid_Data")
carabid = read.csv("carabid_both.csv")

spname = carabid$SpeciesName
spepithet = carabid$SpeciesEpithet
group = carabid$Group
subgenus = carabid$Subgenus
genus = carabid$Genus
subtribe = carabid$Subtribe
tribe = carabid$Tribe
supertribe = carabid$Supertribe
subfamily = carabid$Subfamily

levels(spname)
table(spname)

#carabid = carabid %>%
  # group_by(SpeciesName) %>%
  # filter(n() >= 20)

df = carabid
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(df))
sampleSizeValidation <- floor(fractionValidation * nrow(df))
sampleSizeTest       <- floor(fractionTest       * nrow(df))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
dfTraining   <- df[indicesTraining, ]
dfValidation <- df[indicesValidation, ]
dfTest       <- df[indicesTest, ]

#norm.trainData.species = norm.trainData.species[norm.trainData.species$SpeciesName != "Carabidae sp.", , drop=FALSE]
#norm.testData.species = norm.testData.species[norm.testData.species$SpeciesName != "Carabidae sp.", , drop=FALSE]
#norm.validData.species = norm.validData.species[norm.validData.species$SpeciesName != "Carabidae sp.", , drop=FALSE]

#This part is messy, but it allows all training sets to have the same observations, just with different snips depending on taxonomic level

'%!in%' <- function(x,y)!('%in%'(x,y))
#Snipping subfamily (cutoff = 50)
subfamtraining = subset(dfTraining, Subfamily %!in% c('indet.', 'Trechinae', 'Loricerinae'))
subfamtest = subset(dfTest, Subfamily %!in% c('indet.', 'Trechinae', 'Loricerinae'))
subfamvalid = subset(dfValidation, Subfamily %!in% c('indet.', 'Trechinae', 'Loricerinae'))

#Snipping supertribe (cutoff = 50)
suptribtraining = subset(dfTraining, Supertribe %!in% c('indet.', 'Bembidiini', 'Loricerini', ''))
suptribtest = subset(dfTest, Supertribe %!in% c('indet.', 'Bembidiini', 'Loricerini', ''))
suptribvalid = subset(dfValidation, Supertribe %!in% c('indet.', 'Bembidiini', 'Loricerini', ''))

#Snipping Tribe (cutoff = 50) (More observations in Tribe than Supertribe because 114 obs in Supertribe were '')
tribtraining = subset(dfTraining, Tribe %!in% c('indet.', 'Bembidiini', 'Loricerini', 'Abacetini', 'Cychrini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini'))
tribtest = subset(dfTest, Tribe %!in% c('indet.', 'Bembidiini', 'Loricerini', 'Abacetini', 'Cychrini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini'))
tribvalid = subset(dfValidation, Tribe %!in% c('indet.', 'Bembidiini', 'Loricerini', 'Abacetini', 'Cychrini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini'))

#Snipping subtribe (cutoff < 40)
subtribtraining = subset(dfTraining, Subtribe %!in% c('', 'indet.', 'Bembidiina', 'Loricerini', 'Abacetini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini', 'Anisodactylina', 'Lebiini', 'Stenolophina'))
subtribtest = subset(dfTest, Subtribe %!in% c('', 'indet.', 'Bembidiina', 'Loricerini', 'Abacetini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini', 'Anisodactylina', 'Lebiini', 'Stenolophina'))
subtribvalid = subset(dfValidation, Subtribe %!in% c('', 'indet.', 'Bembidiina', 'Loricerini', 'Abacetini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini', 'Anisodactylina', 'Lebiini', 'Stenolophina'))

#Snipping genus (cutoff = 25)
genustraining = subset(dfTraining, Genus %!in% c('Agonoleptus', 'Agonum', 'Bembidion', 'Carabidae', 'Cymindis', 'Dicaelus', 'Galerita', 'Harpalus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Platynus', 'Sphaeroderus'))
genustest = subset(dfTest, Genus %!in% c('Agonoleptus', 'Agonum', 'Bembidion', 'Carabidae', 'Cymindis', 'Dicaelus', 'Galerita', 'Harpalus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Platynus', 'Sphaeroderus'))
genusvalid = subset(dfValidation, Genus %!in% c('Agonoleptus', 'Agonum', 'Bembidion', 'Carabidae', 'Cymindis', 'Dicaelus', 'Galerita', 'Harpalus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Platynus', 'Sphaeroderus'))

#Snipping subgenus (cutoff = 25)
subgenustraining = subset(dfTraining, Subgenus %!in% c('indet.', 'Agonoleptus', 'Agostenus', 'Bembidion', 'Dicaelus', 'Europhilus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Paradicaelus', 'Phonius', 'Platynus', 'Progalertina', 'Pseudomaseus', 'Pseudoophonus', 'Semicampa', 'Sphaeroderus', 'Tarulus'))
subgenustest = subset(dfTest, Subgenus %!in% c('indet.', 'Agonoleptus', 'Agostenus', 'Bembidion', 'Dicaelus', 'Europhilus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Paradicaelus', 'Phonius', 'Platynus', 'Progalertina', 'Pseudomaseus', 'Pseudoophonus', 'Semicampa', 'Sphaeroderus', 'Tarulus'))
subgenusvalid = subset(dfValidation, Subgenus %!in% c('indet.', 'Agonoleptus', 'Agostenus', 'Bembidion', 'Dicaelus', 'Europhilus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Paradicaelus', 'Phonius', 'Platynus', 'Progalertina', 'Pseudomaseus', 'Pseudoophonus', 'Semicampa', 'Sphaeroderus', 'Tarulus'))

#Snipping group (cutoff < 20)
grouptraining = subset(dfTraining, Group %!in% c('indet.', '', 'adstrictus', 'alternatus', 'bicolor', 'conjunctus', 'decentis', 'faber', 'femoralis', 'furvus', 'harrisii', 'laticollis', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'purpuratus','pusillus', 'quadrimaculatum', 'sordis', 'stenostomus'))
grouptest = subset(dfTest, Group %!in% c('indet.', '', 'adstrictus', 'alternatus', 'bicolor', 'conjunctus', 'decentis', 'faber', 'femoralis', 'furvus', 'harrisii', 'laticollis', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'purpuratus','pusillus', 'quadrimaculatum', 'sordis', 'stenostomus'))
groupvalid = subset(dfValidation, Group %!in% c('indet.', '', 'adstrictus', 'alternatus', 'bicolor', 'conjunctus', 'decentis', 'faber', 'femoralis', 'furvus', 'harrisii', 'laticollis', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'purpuratus','pusillus', 'quadrimaculatum', 'sordis', 'stenostomus'))

#Snipping species (cutoff <20)
sptraining = subset(dfTraining, SpeciesEpithet %!in% c('indet.', 'sp.', 'alternatus', 'bicolor', 'conjunctus', 'coracinus', 'decentis', 'erythropus', 'faber', 'femoralis', 'furvus', 'gratiosum', 'harrisii', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'platyderus', 'purpuratus','pusillus', 'quadrimaculatum', 'sculptilis',  'sodalis', 'stenostomus'))
sptest = subset(dfTest, SpeciesEpithet %!in% c('indet.', 'sp.', 'alternatus', 'bicolor', 'conjunctus', 'coracinus', 'decentis', 'erythropus', 'faber', 'femoralis', 'furvus', 'gratiosum', 'harrisii', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'platyderus', 'purpuratus','pusillus', 'quadrimaculatum', 'sculptilis',  'sodalis', 'stenostomus'))
spvalid = subset(dfValidation, SpeciesEpithet %!in% c('indet.', 'sp.', 'alternatus', 'bicolor', 'conjunctus', 'coracinus', 'decentis', 'erythropus', 'faber', 'femoralis', 'furvus', 'gratiosum', 'harrisii', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'platyderus', 'purpuratus','pusillus', 'quadrimaculatum', 'sculptilis',  'sodalis', 'stenostomus'))

write.csv(genustest, "genustest.csv", row.names = F)
write.csv(genustraining, "genustrain.csv", row.names = F)
write.csv(genusvalid, "genusvalid.csv", row.names = F)
write.csv(grouptest, "grouptest.csv", row.names = F)
write.csv(grouptraining, "grouptrain.csv", row.names = F)
write.csv(groupvalid, "groupvalid.csv", row.names = F)
write.csv(sptest, "sptest.csv", row.names = F)
write.csv(sptraining, "sptrain.csv", row.names = F)
write.csv(spvalid, "spvalid.csv", row.names = F)
write.csv(subfamdftest, "subfamtest.csv", row.names = F)
write.csv(subfamdftraining, "subfamtrain.csv", row.names = F)
write.csv(subfamdfvalid, "subfamvalid.csv", row.names = F)
write.csv(subgenustest, "subgenustest.csv", row.names = F)
write.csv(subgenustraining, "subgenustrain.csv", row.names = F)
write.csv(subgenusvalid, "subgenusvalid.csv", row.names = F)
write.csv(suptribtest, "suptribtest.csv", row.names = F)
write.csv(suptribtraining, "suptribtrain.csv", row.names = F)
write.csv(suptribvalid, "suptribvalid.csv", row.names = F)
write.csv(subtribtest, "subtribtest.csv", row.names = F)
write.csv(subtribtraining, "subtribtrain.csv", row.names = F)
write.csv(subtribvalid, "subtribvalid.csv", row.names = F)
write.csv(tribtest, "tribtest.csv", row.names = F)
write.csv(tribtraining, "tribtrain.csv", row.names = F)
write.csv(tribvalid, "tribvalid.csv", row.names = F)


