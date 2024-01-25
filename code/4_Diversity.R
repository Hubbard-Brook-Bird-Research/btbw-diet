#### ------------------------------------------------------------------------------------ ####
#### Calculate richness and diversity (3 Hill numbers) for samples grouped by survey period
##   Note: Interpolated to lowest common sample size, n = 24
#### ------------------------------------------------------------------------------------ ####
library(iNEXT)
library(dplyr)
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/Data"
setwd(folder)


#### Load data
sp <- read.csv("Species_matrix.csv")
sp$Period <- factor(sp$Period, levels=c("Early","Mid","Late"))
sp.mat <- as.matrix(sp[,startsWith(colnames(sp), "Spp")])
table(sp$Period)  # sample sizes differ enough that rarefaction is necessary

#### Data preparation
sp.mat.r <- aggregate(x = sp.mat, by = list(sp$Period), FUN = sum)

## First entry must be total number of samples
sp.mat.r$Group.1 <- as.vector(table(sp$Period))

## Create a list for each Period
w <- list(early=as.integer(sp.mat.r[1,]),
          mid=as.integer(sp.mat.r[2,]),
          late=as.integer(sp.mat.r[3,]))
str(w) # 0s are okay


#### Hill numbers
## Richness
mod0 <- iNEXT(w, q=0, datatype="incidence_freq", se=T, conf = 0.95, size=24)

## Shannon's Diversity
mod1 <- iNEXT(w, q=1, datatype="incidence_freq", se=T, conf = 0.95, size=24)

## Simpson's Diversity
mod2 <- iNEXT(w, q=2, datatype="incidence_freq", se=T, conf = 0.95, size=24)


#### Put numbers together for a table
(d1 <- mod1$iNextEst$size_based[which(mod1$iNextEst$size_based$t == 24), c(1:7)])
# order.q = Hill number, 1-3
# qD = diversity estimate
# qD.LCL,UCL = 95% CI on diversity estimate
# SC = sample coverage estimate with CIs

(d0 <- mod0$iNextEst$size_based[which(mod0$iNextEst$size_based$t == 24), c(1:7)])
(d2 <- mod2$iNextEst$size_based[which(mod2$iNextEst$size_based$t == 24), c(1:7)])
d <- rbind(d0, d1, d2)
names(d) <- c("Period","Sample_size","Method","Hill_number","Estimate","Lower_CI","Upper_CI")
d$Metric <- c(rep("Species richness",3), rep("Exponential Shannon Entropy",3), rep("Inverse Simpsons concentration",3))

setwd("../Output/")
write.csv(d, file="Diversity_rarefaction.csv", row.names = F)


