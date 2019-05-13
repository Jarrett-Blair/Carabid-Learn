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

#levels(spname)
#table(spname)

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

i = 1
j = 1
for(i in 1:3){
  if(i == 1){
    df = dfTraining
  }
  if(i == 2){
    df = dfTest
  }
  if(i == 3){
    df = dfValidation
  }
  for(j in 1:8){
    if(j == 1){ ### Subfamily ###
      set = subset(df, Subfamily %!in% c('indet.', 'Trechinae', 'Loricerinae'))
      if(i == 1){
        subfamtrain = set
      }
      if(i == 2){
        subfamtest = set
      }
      if(i == 3){
        subfamvalid = set
      }
    }
    if(j == 2){ ### Supertribe ###
      set = subset(df, Supertribe %!in% c('indet.', 'Bembidiini', 'Loricerini', ''))
      if(i == 1){
        suptribtrain = set
      }
      if(i == 2){
        suptribtest = set
      }
      if(i == 3){
        suptribvalid = set
      }
    }
    if(j == 3){ ### Tribe ###
      set = subset(df, Tribe %!in% c('indet.', 'Bembidiini', 'Loricerini', 'Abacetini', 'Cychrini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini'))
      if(i == 1){
        tribtrain = set
      }
      if(i == 2){
        tribtest = set
      }
      if(i == 3){
        tribvalid = set
      }
    }
    if(j == 4){ ### Subtribe ###
      set = subset(df, Subtribe %!in% c('', 'indet.', 'Bembidiina', 'Loricerini', 'Abacetini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini', 'Anisodactylina', 'Lebiini', 'Stenolophina'))
      if(i == 1){
        subtribtrain = set
      }
      if(i == 2){
        subtribtest = set
      }
      if(i == 3){
        subtribvalid = set
      }
    }
    if(j == 5){ ### Genus ###
      set = subset(df, Genus %!in% c('Agonoleptus', 'Agonum', 'Bembidion', 'Carabidae', 'Cymindis', 'Dicaelus', 'Galerita', 'Harpalus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Platynus', 'Sphaeroderus'))
      if(i == 1){
        genustrain = set
      }
      if(i == 2){
        genustest = set
      }
      if(i == 3){
        genusvalid = set
      }
    }
    if(j == 6){ ### Subgenus ###
      set = subset(df, Subgenus %!in% c('indet.', 'Agonoleptus', 'Agostenus', 'Bembidion', 'Dicaelus', 'Europhilus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Paradicaelus', 'Phonius', 'Platynus', 'Progalertina', 'Pseudomaseus', 'Pseudoophonus', 'Semicampa', 'Sphaeroderus', 'Tarulus'))
      if(i == 1){
        subgenustrain = set
      }
      if(i == 2){
        subgenustest = set
      }
      if(i == 3){
        subgenusvalid = set
      }
    }
    if(j == 7){ ### Group ###
      set = subset(df, Group %!in% c('indet.', '', 'adstrictus', 'alternatus', 'bicolor', 'conjunctus', 'decentis', 'faber', 'femoralis', 'furvus', 'harrisii', 'laticollis', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'purpuratus','pusillus', 'quadrimaculatum', 'sordis', 'stenostomus'))
      if(i == 1){
        grouptrain = set
      }
      if(i == 2){
        grouptest = set
      }
      if(i == 3){
        groupvalid = set
      }
    }
    if(j == 8){ ### Species ###
      set = subset(df, SpeciesEpithet %!in% c('indet.', 'sp.', 'alternatus', 'bicolor', 'conjunctus', 'coracinus', 'decentis', 'erythropus', 'faber', 'femoralis', 'furvus', 'gratiosum', 'harrisii', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'platyderus', 'purpuratus','pusillus', 'quadrimaculatum', 'sculptilis',  'sodalis', 'stenostomus'))
      if(i == 1){
        sptrain = set
      }
      if(i == 2){
        sptest = set
      }
      if(i == 3){
        spvalid = set
      }
    }
  }
}

write.csv(genustest, "genustest.csv", row.names = F)
write.csv(genustrain, "genustrain.csv", row.names = F)
write.csv(genusvalid, "genusvalid.csv", row.names = F)
write.csv(grouptest, "grouptest.csv", row.names = F)
write.csv(grouptrain, "grouptrain.csv", row.names = F)
write.csv(groupvalid, "groupvalid.csv", row.names = F)
write.csv(sptest, "sptest.csv", row.names = F)
write.csv(sptrain, "sptrain.csv", row.names = F)
write.csv(spvalid, "spvalid.csv", row.names = F)
write.csv(subfamdftest, "subfamtest.csv", row.names = F)
write.csv(subfamdftrain, "subfamtrain.csv", row.names = F)
write.csv(subfamdfvalid, "subfamvalid.csv", row.names = F)
write.csv(subgenustest, "subgenustest.csv", row.names = F)
write.csv(subgenustrain, "subgenustrain.csv", row.names = F)
write.csv(subgenusvalid, "subgenusvalid.csv", row.names = F)
write.csv(suptribtest, "suptribtest.csv", row.names = F)
write.csv(suptribtrain, "suptribtrain.csv", row.names = F)
write.csv(suptribvalid, "suptribvalid.csv", row.names = F)
write.csv(subtribtest, "subtribtest.csv", row.names = F)
write.csv(subtribtrain, "subtribtrain.csv", row.names = F)
write.csv(subtribvalid, "subtribvalid.csv", row.names = F)
write.csv(tribtest, "tribtest.csv", row.names = F)
write.csv(tribtrain, "tribtrain.csv", row.names = F)
write.csv(tribvalid, "tribvalid.csv", row.names = F)


