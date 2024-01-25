#### ------------------------------------------------------------------------------------ ####
#### Calculate FOO tables and export
##  1) Species-level
##  2) Family-level
##  3) Order-level
#### ------------------------------------------------------------------------------------ ####
library(vegan)
library(stringr)
library(dplyr)
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/Data"
setwd(folder)
tax <- read.csv("Taxonomic_reference.csv")


#### ----------------------- 1) Calculate FOO table for all species ------------------- ####

#### Load data
sp <- read.csv("Species_matrix.csv")
sp.mat <- as.matrix(sp[,startsWith(colnames(sp), "Spp")])
table(specnumber(sp.mat, MARGIN=2)) 
freq <- specnumber(sp.mat, MARGIN=2) # creates an object with the number of times each species occurs  


#### Assemble dataset with FOO by Period
foo.final <- data.frame(Species=colnames(sp.mat), Total_freq=freq, Early=NA, Mid=NA, Late=NA)


## Divide dataset by species and state
early <- sp.mat[which(sp$Period=="Early"),]
mid <- sp.mat[which(sp$Period=="Mid"),]
late <- sp.mat[which(sp$Period=="Late"),]

## Calculate FOO for each period and add to dataframe
foo.early <- round(specnumber(early, MARGIN=2)/nrow(early),3)*100
foo.final$Early <- foo.early[match(foo.final$Species, names(foo.early))]

foo.mid <- round(specnumber(mid, MARGIN=2)/nrow(mid),3)*100
foo.final$Mid <- foo.mid[match(foo.final$Species, names(foo.mid))]

foo.late <- round(specnumber(late, MARGIN=2)/nrow(late),3)*100
foo.final$Late <- foo.late[match(foo.final$Species, names(foo.late))]

## Link to taxonomy
foo.final$Class <- tax$C[match(foo.final$Species, tax$Spp_ID)]
foo.final$Order <- tax$O[match(foo.final$Species, tax$Spp_ID)]
foo.final$Family <- tax$F[match(foo.final$Species, tax$Spp_ID)]
foo.final$Sp <- tax$S[match(foo.final$Species, tax$Spp_ID)]

## Sort by frequency and export
foo.final <- foo.final[order(-foo.final$Total_freq),]

setwd(folder)
setwd("../Output/")
write.csv(foo.final, "Species_FOO.csv", row.names = F)



#### ----------------------- 2) Calculate FOO table for all families ------------------- ####
rm(list=setdiff(ls(), c("folder","tax")))


#### Load data
setwd(folder)
fam <- read.csv("Family_matrix.csv")
fam.mat <- as.matrix(select(fam, Agelenidae:Xylophagidae))
table(specnumber(fam.mat, MARGIN=2)) 
freq <- specnumber(fam.mat, MARGIN=2) # creates an object with the number of times each species occurs  


#### Assemble dataset with FOO by Period
foo.final <- data.frame(Family=colnames(fam.mat), Total_freq=freq, Early=NA, Mid=NA, Late=NA)

## Divide dataset by species and state
early <- fam.mat[which(fam$Period=="Early"),]
mid <- fam.mat[which(fam$Period=="Mid"),]
late <- fam.mat[which(fam$Period=="Late"),]

## Calculate FOO for each period and add to dataframe
foo.early <- round(specnumber(early, MARGIN=2)/nrow(early),3)*100
foo.final$Early <- foo.early[match(foo.final$Family, names(foo.early))]

foo.mid <- round(specnumber(mid, MARGIN=2)/nrow(mid),3)*100
foo.final$Mid <- foo.mid[match(foo.final$Family, names(foo.mid))]

foo.late <- round(specnumber(late, MARGIN=2)/nrow(late),3)*100
foo.final$Late <- foo.late[match(foo.final$Family, names(foo.late))]

## Link to taxonomy
foo.final$Class <- tax$C[match(foo.final$Family, tax$F)]
foo.final$Order <- tax$O[match(foo.final$Family, tax$F)]

## Sort by frequency and export
foo.final <- foo.final[order(-foo.final$Total_freq),]

setwd(folder)
setwd("../Output/")
write.csv(foo.final, "Family_FOO.csv", row.names = F)


#### Looking more at age: Table of FOO by Family for HY vs SY/ASY vs All 
foo.final <- data.frame(Family=colnames(fam.mat), Total_freq=freq, All=NA, AHY=NA, HY=NA)
hy <- fam.mat[which(fam$Age =="HY"), ]
ahy <- fam.mat[which(fam$Age !="HY"), ]

foo.hy <- round(specnumber(hy, MARGIN=2)/nrow(hy),3)*100
foo.final$HY <- foo.hy[match(foo.final$Family, names(foo.hy))]

foo.ahy <- round(specnumber(ahy, MARGIN=2)/nrow(ahy),3)*100
foo.final$AHY <- foo.ahy[match(foo.final$Family, names(foo.ahy))]

foo.final$All <- round(foo.final$Total_freq / nrow(fam.mat), 3)*100

## Link to taxonomy
foo.final$Order <- tax$O[match(foo.final$Family, tax$F)]
foo.final <- foo.final %>%
  select(Order, everything())

## Sort by frequency and export
foo.final <- foo.final[order(-foo.final$Total_freq),]
setwd("~")
write.csv(foo.final, "Family_FOO_Age.csv", row.names = F)




#### ----------------------- 3) Calculate FOO table for all orders ----------------------- ####
## This section is meant to address specific needs for the results section
## No downstream analysis
rm(list=setdiff(ls(), c("folder","tax")))


#### Load data
setwd(folder)
ord <- read.csv("Order_matrix.csv")
ord.mat <- as.matrix(select(ord, Araneae:Trombidiformes))
ord.mat[ord.mat > 1] <- 1           # presence-absence conversion

table(specnumber(ord.mat, MARGIN=2)) 
freq <- specnumber(ord.mat, MARGIN=2) # creates an object with the number of times each species occurs  


#### Q1: What was the FOO of the top three orders across samples?
foo.all <- round(specnumber(ord.mat, MARGIN=2)/nrow(ord),3)*100
foo.all <- data.frame(Order = names(foo.all),
                      FOO = as.numeric(foo.all))
foo.all <- foo.all[order(foo.all$FOO, decreasing = T), ]

setwd(folder)
setwd("../Output/")
write.csv(foo.all, "Order_FOO.csv", row.names = F)


#### Q2: What was the FOO of the top 3 orders for HY and SY/ASY during the late survey period?
late <- ord.mat[which(ord$Period=="Late"), ]
late.meta <- ord[which(ord$Period=="Late"), ]
freq <- specnumber(late, MARGIN=2)

late.hy <- late[which(late.meta$Age == "HY"), ]
late.ahy <- late[which(late.meta$Age != "HY"), ]
foo.final <- data.frame(Order=colnames(ord.mat), Late_freq=freq, HY=NA, AHY=NA)

## Calculate FOO for each age and add to dataframe
foo.hy <- round(specnumber(late.hy, MARGIN=2)/nrow(late.hy),3)*100
foo.final$HY <- foo.hy[match(foo.final$Order, names(foo.hy))]

foo.ahy <- round(specnumber(late.ahy, MARGIN=2)/nrow(late.ahy),3)*100
foo.final$AHY <- foo.ahy[match(foo.final$Order, names(foo.ahy))]

## Sort by frequency and export
foo.final <- foo.final[order(-foo.final$Late_freq),]
setwd("~")
write.csv(foo.final, "Order_FOO_LatePeriod.csv", row.names = F)


