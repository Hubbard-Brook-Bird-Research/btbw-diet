#### ------------------------------------------------------------------------------------ ####
#### Test beta dispersion, run PERMANOVA and NMDS at the family level.
##   1. Beta Dispersion
##   2. Final PERMANOVA model
##   3. NMDS
##   4. Workspace for additional tests
#### ------------------------------------------------------------------------------------ ####
library(vegan)
library(dplyr)

#### Load data
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet"
setwd(paste0(folder,"./Data/"))

sp <- read.csv("Family_matrix.csv")     # object sp originally was short for "species". 
sp.mat <- sp %>% select(!(index:BandCC))
sp$Period <- factor(sp$Period)
sp$Elevation <- factor(sp$Elevation)
sp$Age <- factor(sp$Age)
sp$Sex <- factor(sp$Sex)


#### Beta Dispersion
dis <- vegdist(sp.mat, method="raup")
mod.dis <- betadisper(dis, sp$Period, bias.adjust = T) # warning is okay. 
# This is similar to testing for homogeneity of variance before an ANOVA. 
anova(mod.dis)      
TukeyHSD(mod.dis)   # Period = Late is significantly different from the other Periods.
boxplot(mod.dis)
plot(mod.dis)       # You can see that the dispersion is greater

## Test beta dispersion for the other variables
test1 <- betadisper(dis, sp$Elevation, bias.adjust = T) # warning is okay. 
anova(test1)      
test2 <- betadisper(dis, sp$Age, bias.adjust = T) # warning is okay. 
anova(test2)      
# These other groups do not have significant differences in dispersion


#### PERMANOVA
fit <- adonis2(sp.mat ~ Period*Elevation + Age, data=sp, permutations=999, 
               method="raup", strata=sp$Sex, by = "terms")
# "terms" is the default. Variables are tests sequentially. 
fit    


#### NMDS
NMDS <- metaMDS(comm = sp.mat, distance = "raup", k=3, trymax=300) # stable 3-dim solution
# The NMDS has trouble converging when using k=2 dimensions
stressplot(NMDS)
NMDS              # Stress is pretty high.
#orditorp(NMDS,display="sites",col="red",air=0.01)  # Code to explore outliers
cols <- c("#0098FF","#FF7F00","#33A02C") #blue, orange, green


#### Quick plot to visualize Period variable
par(las=1, mar=c(4,4,1,8), oma=c(1,1,1,1))
plot(NMDS, display="sites", type="n", xlim=c(-0.5,0.5), choices = c(1,2)) 
points(NMDS, display="sites", pch=c(15,17,18)[as.numeric(sp$Period)], 
       col=cols[as.numeric(sp$Period)], cex=1.3, lwd=2, choices=c(1,2))
ordiellipse(NMDS, kind="se", conf=0.95, sp$Period, col=cols,lwd=2) # 95% CI
legend(x=0.55, y=-0.34, pch=c(15,17,18), col=cols,legend=c("Early","Late","Mid"),bty="n", y.intersp=1.2, xpd=T)
# If a review asks to see the other NMDS dimension, plot with choices=c(1,3)


#### Plot for the Elevation variable
par(las=1, mar=c(4,4,1,8), oma=c(1,1,1,1))
plot(NMDS, display="sites", type="n", xlim=c(-0.5,0.5))
points(NMDS, display="sites", pch=c(15,17,18)[as.numeric(sp$Elevation)], 
       col=cols[as.numeric(sp$Elevation)], cex=1.3, lwd=2, choices=c(1,2))
ordiellipse(NMDS, kind="se", conf=0.95, sp$Elevation, col=cols,lwd=2) # 95% CI
legend(x=0.55, y=-0.34, pch=c(15,17,18), col=cols,legend=c("High","Low","Mid"),bty="n", y.intersp=1.2, xpd=T)


#### Plot for the Age variable
par(las=1, mar=c(4,4,1,8), oma=c(1,1,1,1))
plot(NMDS, display="sites", type="n", xlim=c(-0.5,0.5))
points(NMDS, display="sites", pch=c(15,17,18)[as.numeric(sp$Age)], 
       col=cols[as.numeric(sp$Age)], cex=1.3, lwd=2, choices=c(1,2))
ordiellipse(NMDS, kind="se", conf=0.95, sp$Age, col=cols,lwd=2) # 95% CI
legend(x=0.55, y=-0.34, pch=c(15,17,18), col=cols,legend=c("ASY","HY","SY"),bty="n", y.intersp=1.2, xpd=T)


#### Plot for the Sex variable
cols <- c("#0098FF","#FF7F00") #blue, orange
par(las=1, mar=c(4,4,1,8), oma=c(1,1,1,1))
plot(NMDS, display="sites", type="n", xlim=c(-0.5,0.5))
points(NMDS, display="sites", pch=c(15,17,18)[as.numeric(sp$Sex)], 
       col=cols[as.numeric(sp$Sex)], cex=1.3, lwd=2, choices=c(1,2))
ordiellipse(NMDS, kind="se", conf=0.95, sp$Sex, col=cols, lwd=2) # 95% CI
legend(x=0.55, y=-0.34, pch=c(15,17,18), col=cols,legend=c("ASY","HY","SY"),bty="n", y.intersp=1.2, xpd=T)


#### Save and export statistical tests
setwd(paste0(folder,"/Output"))
save(mod.dis, fit, NMDS, file="Permanova_results.RData")



#### -------------------- Workspace for testing other modeling approaches ---------------------------- ####

#### What is the effect of removing HY birds? 
remove <- which(sp$Age=="HY")
sp.noHY <- sp[-remove,]
sp.mat.noHY <- sp.mat[-remove,]
sp.mat.noHY <- sp.mat.noHY[,which(colSums(sp.mat.noHY)>0)]  # method raup influenced by 0 colsums


#### Beta Dispersion
dis2 <- vegdist(sp.mat.noHY, method="raup")
mod.dis2 <- betadisper(dis2, sp.noHY$Period, bias.adjust = T) # warning is okay. 
# This is similar to testing for homogeneity of variance before an ANOVA. 
anova(mod.dis2)      
TukeyHSD(mod.dis2)   # Period = Late is significantly different from the other Periods.
boxplot(mod.dis2)
plot(mod.dis2)       # You can see that the dispersion is greater


#### PERMANOVA
fit2 <- adonis2(sp.mat.noHY ~ Period*Elevation + Age, data=sp.noHY, permutations=999, 
               method="raup", strata=sp.noHY$Sex, by = "terms")
fit2   # Effect of Period is weaker overall.  


#### NMDS
NMDS2 <- metaMDS(comm = sp.mat.noHY, distance = "raup", k=3, trymax=300) # stable 3-dim solution
NMDS              # Stress is pretty high.


