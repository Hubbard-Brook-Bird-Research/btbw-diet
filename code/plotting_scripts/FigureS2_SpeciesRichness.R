#### ------------------------------------------------------------------------------------ ####
#### Calculate per-sample richness and compare with a Kruskal-Walllice test
##   Create a quick beeswarm plot for the supplemental material
#### ------------------------------------------------------------------------------------ ####
library(vegan)
library(dplyr)
library(beeswarm)
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/Data"
setwd(folder)


#### Load data
sp <- read.csv("Species_matrix.csv")
sp$Period <- factor(sp$Period, levels=c("Early","Mid","Late"))
sp.mat <- as.matrix(sp[,startsWith(colnames(sp), "Spp")])


#### Calculate species richness, test for differences
specnumber(sp.mat, MARGIN=1, groups=sp$Period) # Total richness is similar between the three periods
sp.tab <- sp %>% select(Elevation:Age) %>%
  mutate(N.species = specnumber(sp.mat, MARGIN=1))
hist(sp.tab$N.species, breaks=20) # it's a better-looking distribution than I thought! Kruskal-Wallis will work. 

## Kruskal-Wallis test
kruskal.test(N.species ~ Period, data = sp.tab)
# Differences between groups are present but weak. p = 0.07

## Pairwise comparisons
boxplot(sp.tab$N.species ~ sp.tab$Period)
pairwise.wilcox.test(sp.tab$N.species, sp.tab$Period, p.adjust.method = "BH")
# Safe to ignore the warning
# It seems that the greatest difference is between Early and Late.


#### Plotting!
cols <- c("#31688EFF" ,"#35B779FF","#FD9567FF")  # blue, green, peach

setwd("../Output/")
pdf(file="Figure_persample.pdf",width=3.4,height=3.6)

par(mar=c(1,2.5,1,1), mgp=c(1.5,0.4,0), oma =c(1,1,2,1), las=1, tcl=-0.25, cex.axis=0.7, cex.lab=0.7)

#### PLOTTING
beeswarm(N.species ~ Period, data=sp.tab, col=cols, pch=16, method="center",ylim=c(0,50), bty="n",
         ylab="Species per sample", xlab="", xaxt="n", cex=0.7)
bxplot(N.species ~ Period, data=sp.tab, add=T, probs=0.5, lwd=1.2, width=0.7, col="black") 
axis(1, at=c(1,2,3), lab=c("Early","Mid","Late"))  


dev.off()



