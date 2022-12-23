#### ------------------------------------------------------------------------------------ ####
#### Figure: 2 panel, horizontal plot with NMDS and beta dispersion results
#### ------------------------------------------------------------------------------------ ####
library(vegan)
library(dplyr)
#library(viridis)  # To help find colors
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet"

#### Load data
setwd(paste0(folder,"./Data/"))
sp <- read.csv("Family_matrix.csv")     # object sp originally was short for "species". 
setwd(paste0(folder,"/Output"))
load("Permanova_results.RData")

#### Organize data
sp.mat <- sp %>% select(!(index:BandCC))
sp$Period <- factor(sp$Period, levels=c("Early","Mid","Late"))
sp$Elevation <- factor(sp$Elevation)
sp$Age <- factor(sp$Age)
sp$Sex <- factor(sp$Sex)
sp$distances <- mod.dis$distances

## Colors are from a combination of viridis and magma palettes (b/c viridis yellow was too bright)
cols <- c("#31688EFF" ,"#35B779FF","#FD9567FF")  # blue, green, peach


#### PLOTTING
setwd(paste0(folder,"/Output/"))
pdf(file="Figure1_NMDS.pdf",width=5.5,height=3.5)

## Panel A
par(mfrow=c(1,2), las=1, tcl=-0.25, cex.axis=0.7, cex.lab=0.7,
    mgp=c(1.5,0.4,0), mar=c(5,3,2,1))

plot(NMDS, display="sites", type="n", xlim=c(-0.3,0.3), choices = c(1,2), xaxt="n", yaxt="n") 
axis(side=2, labels=c("-0.3","0.0","0.3"), at = c(-0.3,0,0.3))
axis(side=1, labels=c("-0.3","0.0","0.3"), at = c(-0.3,0,0.3), mgp=c(1.8,0.1,0), main="test")

grid(lty=1, lwd=0.5, col="#d3d3d380")
points(NMDS, display="sites", pch=c(15,16,17)[as.numeric(sp$Period)], 
       col=cols[as.numeric(sp$Period)], cex=c(0.8,0.8,0.8)[as.numeric(sp$Period)], 
       lwd=2, choices=c(1,2))
ordiellipse(NMDS, kind="se", conf=0.95, sp$Period, col=cols,lwd=1) # 95% CI
legend(x=0, y=0.43, xjust=0.5, pch=c(15,16,17), col=cols, cex=0.7, x.intersp=0.6,
       legend=c("Early","Mid","Late"), bty="n", horiz=T, xpd=NA)
title(main = "a", adj=0.015, line = -0.85, cex.main = 1.2)

## Panel B
boxplot(sp$distances ~ sp$Period, ylim=c(0,0.35), col=cols, xlab="", ylab="Distance to centroid", 
        outline = F, medlwd=1.5, yaxt="n", bty="l")
axis(side=2, labels=c("0.0","0.1","0.2","0.3"), at = c(0,0.1,0.2,0.3))
segments(x0=1.5-0.7, y0=0.265, x1=1.5+0.7, lwd=0.9)
segments(x0=3-0.4, y0=0.265, x1=3+0.4, lwd=0.9)
text(x=c(1.5,3), y=0.28, labels=c("a","b"), adj=0.5, cex=0.7)

title(main = "b", adj=0.015, line = -0.85, cex.main = 1.2)

dev.off()


