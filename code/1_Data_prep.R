#### ------------------------------------------------------------------------------------ ####
#### Prepare BTBW diet data for analysis. Output to "Data" folder. 
##   1) Prepare and export species-level diet matrix and taxonomic reference database
##   2) Prepare and export family-level diet matrix
##   3) Prepare and export data for Malaise comparisons
##   4) Prepare and export data for Lep survey comparisons
##   5) Order-level data prep
#### ------------------------------------------------------------------------------------ ####
library(dplyr)
library(tidyr)

#### Load diet data 
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet"
setwd(paste0(folder,"./Data/"))
sp.rra <- read.csv("Arthropoda_rarefied23000_species-level_TaxonomyEdited.csv")


#### ---------------------- 1) Prep species-level matrix and taxonomy ----------------------------- ####
## If a species has <1% of reads in a sample, force to 0
sp.mat <- sp.rra %>% select(starts_with("tax.k"))

## Convert to presence/absence
sp.mat[sp.mat > 0] <- 1  
sp.mat <- as.data.frame(sp.mat)

## Column names are unwieldy: create a separate taxonomy reference database 
sp.taxonomy <- data.frame(Spp_ID = paste0("Spp", 1:ncol(sp.mat)),
                       Taxonomy = colnames(sp.mat))       
colnames(sp.mat) <- sp.taxonomy$Spp_ID

## Merge back to metadata
sp.mat$index <- sp.rra$index
sp.mat.meta <- sp.rra %>% select("index", Plate:BandCC) %>%
  left_join(y = sp.mat, by="index")


#### Create taxonomic reference database
## First, we need to split into multiple columns
col.add <- c(NA,NA,"K",NA,"P",NA,"C",NA,"O",NA,"F",NA,"G",NA,"S")
tax <- sp.taxonomy %>% separate(col=Taxonomy, into=col.add, sep="_")

## Clean it up (i.e., get rid of trailing characters like ".p")
rm <- c(".p",".c",".o",".f",".g",".s")
tax <- as.matrix(tax)              # convert to matrix for the loop
for(i in 1:length(tax)){            # no function for this, so I'm brute-forcing it
  if(substr(tax[i], nchar(tax[i])-1, nchar(tax[i])) %in% rm){  # if it ends in ".letter", remove ending
    tax[i] <- substr(tax[i],1,nchar(tax[i])-2)}
  if(substr(tax[i], nchar(tax[i]), nchar(tax[i])) == "."){   # if it ends in ".", remove ending
    tax[i] <- substr(tax[i],1,nchar(tax[i])-1)}
}
tax <- as.data.frame(tax)


#### Export species-level diet matrix and taxomonic reference
write.csv(sp.mat.meta, "Species_matrix.csv", row.names = F)
write.csv(tax, "Taxonomic_reference.csv", row.names = F)


## Optional: explore dataset
table(sp.mat.meta$Elevation)
table(sp.mat.meta$Period)
table(sp.mat.meta$Sex)
table(sp.mat.meta$Age)
table(sp.mat.meta$Age, sp.mat.meta$Elevation)  # All HY birds are late season high elev
table(sp.mat.meta$Age, sp.mat.meta$Period)
table(sp.mat.meta$Nest_Stage)

table(tax$F)


#### ------------------------- 2) Create and export family-level dataset ------------------------- ####
rm(list=setdiff(ls(), c("folder","tax","sp.mat.meta")))

sp.mat <- sp.mat.meta[,-c(1:10)]
names(sp.mat)[1] == "Spp1"        # check to make sure correct columns remain


#### Match each species ID to a family
# Strategy: flip dataset on its side and append family as a column, then flip back and assign colnames
t.sp <- as.data.frame(t(sp.mat))
t.sp$family <- tax$F[match(rownames(t.sp),tax$Spp_ID)]

t.sp <- t.sp[complete.cases(t.sp$family),]               # removes rows with unknown family
t.sp <- t.sp[-which(t.sp$family==""),]                   # remove blank rows, unknown family (-5 rows)

## Aggregate rows with the same family ID
t.agg <- aggregate(t.sp[,-ncol(t.sp)], by=list(t.sp$family), FUN=sum)
colSums(t.agg[,-1])==colSums(t.sp[,-ncol(t.sp)]) # check: Colsums should still be equal

## Translate back and organize
fam.mat <- as.data.frame(t(t.agg))
names(fam.mat) <- lapply(fam.mat[1, ], as.character)
fam.mat <- fam.mat[-1,]
rownames(fam.mat) <- rownames(sp.mat)
fam.mat[fam.mat > 0] <- 1                  # presence-absence conversion

## Append metadat and change to presence-absence
fam.mat.meta <- sp.mat.meta %>% select(index:BandCC) %>%
  bind_cols(y = fam.mat)

#### Export
write.csv(fam.mat.meta, "Family_matrix.csv", row.names=F)



#### -------------------- 3) Create and export dataset that matches Malaise results -------------------- ####

#### Load data
setwd(folder)
setwd("./Data")
m <- read.csv("Malaise.Order.2021.matrix.csv")

## Gather sample list for each taxonomic group
pent.sp <- tax$Spp_ID[which(tax$F == "Pentatomidae")]
hemi.sp <- tax$Spp_ID[which(tax$O == "Hemiptera")]
plec.sp <- tax$Spp_ID[which(tax$O == "Plecoptera")]
cole.sp <- tax$Spp_ID[which(tax$O == "Coleoptera")]
lepi.sp <- tax$Spp_ID[which(tax$O == "Lepidoptera")]
tipu.sp <- tax$Spp_ID[which(tax$F == "Tipulidae")]
rhag.sp <- tax$Spp_ID[which(tax$F == "Rhagionidae")]
dipO.sp <- tax$Spp_ID[which(tax$O == "Diptera" & tax$F != "Rhagionidae" & tax$F != "Tipulidae")] # Other Diptera
HyIch.sp <- tax$Spp_ID[which(tax$F == "Ichneumonidae")]  
HyO.sp <- tax$Spp_ID[which(tax$O == "Hymenoptera" & tax$F != "Ichneumonidae")] 
# Decided to lump all "Diptera:Other" .
# Spiders were excluded from this dataset due to sampling bias. 
# These groups are NOT represented in any fecal samples:
# Homoptera, Homoptera:Leafhopper, Tricoptera, Panorpidae. 

#dHFT.sp <- tax$Spp_ID[which(tax$F %in% c("Tabanidae","Muscidae","Sarcophagidae","Simuliidae","Syrphidae",
#            "Phoridae","Scathophagidae","Anthomyiidae","Tephritidae","Ulidiidae","Sciomyzidae","Agromyzidae",
#            "Pipunculidae","Calliphoridae"))]             # list of families from John
#dMT.sp <- tax$Spp_ID[which(tax$F %in% c("Bibionidae","Empididae","Cecidomyiidae","Psilidae","Sciaridae",
#            "Mycetophilidae","Xylophagidae","Blephariceridae","Trichoceridae","Asilidae","Hybotidae",
#            "Culicidae","Axymyiidae","Chironomidae"))]    # list of families from John


## No easy way to group columns for summing, so we have to do it manually.
malaise.mat.meta <- sp.mat.meta %>% select(index:BandCC)       # set up dataset to receive new columns
malaise.mat.meta$Hemiptera.Pentatomidae <- sp.mat[,names(sp.mat) == pent.sp]                # only 1 species
malaise.mat.meta$Hemiptera.Other <- apply(sp.mat[,names(sp.mat) %in% hemi.sp], 1, max)
malaise.mat.meta$Plecoptera <- apply(sp.mat[,names(sp.mat) %in% plec.sp], 1, max)
malaise.mat.meta$Coleoptera <- apply(sp.mat[,names(sp.mat) %in% cole.sp], 1, max)
malaise.mat.meta$Lepidoptera <- apply(sp.mat[,names(sp.mat) %in% lepi.sp], 1, max)
malaise.mat.meta$Diptera.Tipulidae <- apply(sp.mat[,names(sp.mat) %in% tipu.sp], 1, max)
malaise.mat.meta$Diptera.Rhagionidae <- apply(sp.mat[,names(sp.mat) %in% rhag.sp], 1, max)
malaise.mat.meta$Diptera.Other <- apply(sp.mat[,names(sp.mat) %in% dipO.sp], 1, max)
malaise.mat.meta$Hymenoptera.Ichneumonids <- apply(sp.mat[,names(sp.mat) %in% HyIch.sp], 1, max)
malaise.mat.meta$Hymenoptera.Not.Ichneumonids <- apply(sp.mat[,names(sp.mat) %in% HyO.sp], 1, max)

## Add in the columns with 0 detections
add.names <- c("Homoptera","Homoptera:Leafhopper","Trichoptera","Panorpidae")
add <- data.frame(matrix(data=0, nrow=nrow(malaise.mat.meta), ncol=length(add.names)))
names(add) <- add.names
malaise.mat.meta <- cbind(malaise.mat.meta, add)

## Export dataset
write.csv(malaise.mat.meta, "Malaise_fecal.csv", row.names=F)




#### ------------------ 4) Create and export dataset that matches Lep survey results -------------------- ####

#### Load data
setwd(folder)
setwd("./Data")
lep <- read.csv("Leps.Family.2021.matrix.csv")

## Gather sample list for each taxonomic group
# Doing this at the species-level to accomodate special cases and missing taxonomies
#arac.sp <- tax$Spp_ID[which(tax$C == "Arachnida")]  
geo.sp <- tax$Spp_ID[which(tax$F == "Geometridae")]
noct.sp <- tax$Spp_ID[which(tax$F == "Noctuidae")]
noto.sp <- tax$Spp_ID[which(tax$F == "Notodontidae")]
#ptcp.sp <- tax$Spp_ID[which(tax$F %in% c("Pyralidae","Tortricidae","Coleophoridae","Psychidae"))]

## No easy way to group columns for summing, so we have to do it manually.
lep.mat.meta <- sp.mat.meta %>% select(index:BandCC)                            # set up dataset to receive new columns
#lep.mat.meta$Arachnida <- apply(sp.mat[,names(sp.mat) %in% arac.sp], 1, max)
lep.mat.meta$Geometridae <- apply(sp.mat[,names(sp.mat) %in% geo.sp], 1, max)
lep.mat.meta$Noctuidae <- apply(sp.mat[,names(sp.mat) %in% noct.sp], 1, max)
lep.mat.meta$Notodontidae <- apply(sp.mat[,names(sp.mat) %in% noto.sp], 1, max)
#lep.mat.meta$Pyralidae.Tortricidae.Coleophoridae.Psychidae <- apply(sp.mat[,names(sp.mat) %in% ptcp.sp], 1, max)

## Export dataset
write.csv(lep.mat.meta, "Leps_fecal.csv", row.names=F)




#### ---------------------- 5) Order-level data prep ----------------------------- ####
library(vegan)
rm(list=ls())
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet"
setwd(paste0(folder,"./Data/"))

sp <- read.csv("Species_matrix.csv")
tax <- read.csv("Taxonomic_reference.csv")
sp.mat <- sp[,startsWith(colnames(sp), "Spp")]


#### Match each species ID to an order
t.sp <- as.data.frame(t(sp.mat))
t.sp$order <- tax$O[match(rownames(t.sp),tax$Spp_ID)]

t.sp <- t.sp[complete.cases(t.sp$order),]               # removes rows with unknown order
t.sp <- t.sp[-which(t.sp$order==""),]                   # remove blank rows, unknown order (-5 rows)

## Aggregate rows with the same order ID
t.agg <- aggregate(t.sp[,-ncol(t.sp)], by=list(t.sp$order), FUN=sum)
colSums(t.agg[,-1])==colSums(t.sp[,-ncol(t.sp)]) # check: Colsums should still be equal

## Translate back and organize
ord.mat <- as.data.frame(t(t.agg))
names(ord.mat) <- lapply(ord.mat[1, ], as.character)
ord.mat <- ord.mat[-1,]
rownames(ord.mat) <- rownames(sp.mat)
ord.mat <- mutate_all(ord.mat, function(x) as.numeric(as.character(x)))
ord.mat[ord.mat > 0] <- 1                  # presence-absence conversion

## Append metadata and change to presence-absence
ord.tab <- sp %>% select(index:BandCC) %>%
  bind_cols(y = ord.mat)


#### Export order-level matrix
write.csv(ord.tab, "Order_matrix.csv", row.names=F)


#### Calculate number of orders per sample
specnumber(ord.mat, MARGIN=1, groups=ord.tab$Period) 
ord.tab$N.orders <- specnumber(ord.mat, MARGIN=1)
hist(ord.tab$N.orders, breaks=20) 
summary(ord.tab$N.orders)











