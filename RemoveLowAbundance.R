setwd("C:/Carabid_Data/Carabid_Data")
df = read.csv("carabid_both.csv")

spname = df$SpeciesName
spepithet = df$SpeciesEpithet
group = df$Group
subgenus = df$Subgenus
genus = df$Genus
subtribe = df$Subtribe
tribe = df$Tribe
supertribe = df$Supertribe
subfamily = df$Subfamily

'%!in%' <- function(x,y)!('%in%'(x,y))


subfam = subset(df, Subfamily %!in% c('indet.', 'Trechinae', 'Loricerinae'))
suptrib = subset(df, Supertribe %!in% c('indet.', 'Bembidiini', 'Loricerini', ''))
trib = subset(df, Tribe %!in% c('indet.', 'Bembidiini', 'Loricerini', 'Abacetini', 'Cychrini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini'))
subtrib = subset(df, Subtribe %!in% c('', 'indet.', 'Bembidiina', 'Loricerini', 'Abacetini', 'Galeritini', 'Licinini', 'Megacephalini', 'Platynini', 'Anisodactylina', 'Lebiini', 'Stenolophina'))
genus = subset(df, Genus %!in% c('Agonoleptus', 'Agonum', 'Bembidion', 'Carabidae', 'Cymindis', 'Dicaelus', 'Galerita', 'Harpalus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Platynus', 'Sphaeroderus'))
subgenus = subset(df, Subgenus %!in% c('indet.', 'Agonoleptus', 'Agostenus', 'Bembidion', 'Dicaelus', 'Europhilus', 'Loricera', 'Loxandrus', 'Oxypselaphus', 'Paradicaelus', 'Phonius', 'Platynus', 'Progalertina', 'Pseudomaseus', 'Pseudoophonus', 'Semicampa', 'Sphaeroderus', 'Tarulus'))
group = subset(df, Group %!in% c('indet.', '', 'adstrictus', 'alternatus', 'bicolor', 'conjunctus', 'decentis', 'faber', 'femoralis', 'furvus', 'harrisii', 'laticollis', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'purpuratus','pusillus', 'quadrimaculatum', 'sordis', 'stenostomus'))
species = subset(df, SpeciesEpithet %!in% c('indet.', 'sp.', 'alternatus', 'bicolor', 'conjunctus', 'coracinus', 'decentis', 'erythropus', 'faber', 'femoralis', 'furvus', 'gratiosum', 'harrisii', 'luctuosus', 'neglecta', 'nigrita', 'pensylvanicus', 'permundus', 'pilicornis', 'platyderus', 'purpuratus','pusillus', 'quadrimaculatum', 'sculptilis',  'sodalis', 'stenostomus'))


write.csv(subfam, "subfamily.csv", row.names = F)
write.csv(suptrib, "supertribe.csv", row.names = F)
write.csv(trib, "tribe.csv", row.names = F)
write.csv(subtrib, "subtribe.csv", row.names = F)
write.csv(genus, "genus.csv", row.names = F)
write.csv(subgenus, "subgenus.csv", row.names = F)
write.csv(group, "group.csv", row.names = F)
write.csv(species, "species.csv", row.names = F)




