#### ------------------------------------------------------------------------------------ ####
#### Figure: 2 panel, vertically aligned plot to compare lep results vs FOO
##   We constrained this analysis to just 3 lep families with reliable data
#### ------------------------------------------------------------------------------------ ####
library(MetBrewer)
library(dplyr)
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/"


#### Load data
setwd(paste0(folder, "./Data"))
lep.all <- read.csv("Leps.Family.2021.matrix.csv")
setwd(paste0(folder, "./Output"))
fam <- read.csv("Family_FOO.csv")
fam <- fam[which(fam$Family %in% c("Geometridae","Noctuidae","Notodontidae")),]
fam$Family <- as.factor(fam$Family)

#### Organize Lep data
lep <- lep.all %>% select(Survey, Geometridae:Notodontidae) %>%
  group_by(Survey) %>%
  summarize(across(everything(), sum))
lep <- as.data.frame(t(lep))
names(lep) <- as.character(lep[1,])
lep <- lep[-1,]
lep$Family <- row.names(lep)

## Convert to proportional biomass (each family as a proportional of total for all 3 families)
lep$Early <- (as.numeric(lep$Early) / sum(as.numeric(lep$Early))) *100
lep$Late <- (as.numeric(lep$Late) / sum(as.numeric(lep$Late))) *100
lep$Middle <- (as.numeric(lep$Middle) / sum(as.numeric(lep$Middle))) *100
lep <- lep[, c("Family","Early","Middle","Late")]


#### Set up plotting colors and points
col <- met.brewer(name="Signac", n=10, type="discrete")
col
col <- data.frame(Family=levels(fam$Family),
                  col=col[c(6,8,9)])          # manually order the colors
fam$color <- col$col[match(fam$Family, col$Family)]
lep$color <- col$col[match(lep$Family, col$Family)]
# geos = purple
# noctuids = blue
# notodontids = green
fam$pch <- c(15,16,17)
lep$pch <- fam$pch[match(lep$Family, fam$Family)]
  
 

#### PLOTTING
pdf(file="Figure3_LepCompare.pdf", width=3.5,height=4.4)

par(mfrow=c(2,1), las=1, tcl=-0.25, cex.axis=0.7, cex.lab=0.7, cex.main=0.8, 
    mgp=c(1.5,0.4,0), mar=c(2,3,2,1))

## Panel A: Fecal FOO
plot(1:3, fam[1,3:5], xlim=c(0.8,3.2), type="n", ylim=c(0,100), bty="l", ylab="FOO", xlab="", xaxt="n")
axis(side=1, labels=c("Early","Mid","Late"), at = c(1:3), mgp=c(1.8,0.1,0))
for(i in 1:nrow(fam)){
  lines(1:3, fam[i,3:5], type="b", pch=fam$pch[i], lwd=1.5, col=fam$color[i], cex=0.8)
}
title(main="Black-throated Blue Warbler diet", )


## Panel B: Malaise Traps
plot(1:3, lep[1,2:4], xlim=c(0.8,3.2), type="n", ylim=c(0,100), bty="l", ylab="Proportional biomass (%)", xlab="", xaxt="n")
axis(side=1, labels=c("Early","Mid","Late"), at = c(1:3), mgp=c(1.8,0.1,0))
for(i in 1:nrow(lep)){
  lines(1:3, lep[i,2:4], type="b", pch=lep$pch[i], lwd=1.5, col=lep$color[i], cex=0.8)
}
title(main="Caterpillar survey")
legend(x=3.3, y=113, xjust=1, pch=lep$pch, col=lep$color, legend=lep$Family, bty="n", cex=0.7, xpd=NA)


dev.off()



