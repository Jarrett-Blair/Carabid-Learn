setwd("C:/Carabid_Data/Carabid_Data")
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


df = carabid
fractionTraining   <- 0.80
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(df))
sampleSizeTest       <- floor(fractionTest       * nrow(df))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesTest  <- sort(sample(indicesNotTraining, size=sampleSizeTest))

# Finally, output the three dataframes for training, validation and test.
dfTraining   <- df[indicesTraining, ]
dfTest       <- df[indicesTest, ]

'%!in%' <- function(x,y)!('%in%'(x,y))

i = 1
j = 1
for(i in 1:2){
  if(i == 1){
    df = dfTraining
  }
  if(i == 2){
    df = dfTest
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
    }
    if(j == 2){ ### Supertribe ###
      set = subset(df, Supertribe %!in% c('indet.', 'Bembidiini', 'Loricerini', ''))
      if(i == 1){
        suptribtrain = set
      }
      if(i == 2){
        suptribtest = set
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
    }
    if(j == 4){ ### Subtribe ###
      set = subset(df, Subtribe %!in% c('', 'indet.', 'Bembidiina', 'Loricerini', 'Abacetini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini', 'Anisodactylina', 'Lebiini', 'Stenolophina'))
      if(i == 1){
        subtribtrain = set
      }
      if(i == 2){
        subtribtest = set
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
    }
    if(j == 6){ ### Subgenus ###
      set = subset(df, Subgenus %!in% c('indet.', 'Agonoleptus', 'Agostenus', 'Bembidion', 'Dicaelus', 'Europhilus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Paradicaelus', 'Phonius', 'Platynus', 'Progalertina', 'Pseudomaseus', 'Pseudoophonus', 'Semicampa', 'Sphaeroderus', 'Tarulus'))
      if(i == 1){
        subgenustrain = set
      }
      if(i == 2){
        subgenustest = set
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
    }
    if(j == 8){ ### Species ###
      set = subset(df, SpeciesEpithet %!in% c('indet.', 'sp.', 'alternatus', 'bicolor', 'conjunctus', 'coracinus', 'decentis', 'erythropus', 'faber', 'femoralis', 'furvus', 'gratiosum', 'harrisii', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'platyderus', 'purpuratus','pusillus', 'quadrimaculatum', 'sculptilis',  'sodalis', 'stenostomus'))
      if(i == 1){
        sptrain = set
      }
      if(i == 2){
        sptest = set
      }
    }
  }
}

setwd("C:/Carabid_Data/Carabid_Data/CrossValidCarabid")

write.csv(genustest, "genustest.csv", row.names = F)
write.csv(genustrain, "genustrain.csv", row.names = F)
write.csv(grouptest, "grouptest.csv", row.names = F)
write.csv(grouptrain, "grouptrain.csv", row.names = F)
write.csv(sptest, "sptest.csv", row.names = F)
write.csv(sptrain, "sptrain.csv", row.names = F)
write.csv(subfamtest, "subfamtest.csv", row.names = F)
write.csv(subfamtrain, "subfamtrain.csv", row.names = F)
write.csv(subgenustest, "subgenustest.csv", row.names = F)
write.csv(subgenustrain, "subgenustrain.csv", row.names = F)
write.csv(suptribtest, "suptribtest.csv", row.names = F)
write.csv(suptribtrain, "suptribtrain.csv", row.names = F)
write.csv(subtribtest, "subtribtest.csv", row.names = F)
write.csv(subtribtrain, "subtribtrain.csv", row.names = F)
write.csv(tribtest, "tribtest.csv", row.names = F)
write.csv(tribtrain, "tribtrain.csv", row.names = F)


