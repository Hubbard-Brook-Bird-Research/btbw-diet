#### ------------------------------------------------------------------------------------ ####
#### Figure: 3 panel, vertically aligned plot showing changes in family-level FOO.
#### ------------------------------------------------------------------------------------ ####
library(vegan)
library(MetBrewer)
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/Output"

#### Load data
setwd(folder)
fam <- read.csv("Family_FOO.csv")


#### Categorize the trends manually based on plot directions
fam$Category <- NA

## Visualize top 15
plot(1:3, fam[1,3:5], type="n", ylim=c(0,100), xlim=c(0.7,3))
for(i in 1:15){
  lines(1:3, fam[i,3:5], type="b", pch=16, lwd=2)
  text(x=0.7, y=fam[i,"Early"], labels=fam$Family[i], adj=0, cex=0.7)
}
lines(1:3, fam[15,3:5], type="b", pch=16, lwd=2, col="red")


## Manually add info on direction
fam$Category[1:15] <- c("Both","Both","Down","Down","Down",
                        "Down","Both","Both","Both","Down",
                        "Up","Both","Up","Up","Down")
fam2 <- fam[complete.cases(fam$Category),]
fam2$Order <- factor(fam2$Order)


#### Set up plotting colors
table(fam2$Order)
col <- met.brewer(name="Signac", n=10, type="discrete")
col
col <- data.frame(Order=levels(fam2$Order),
                  col=col[c(3,6,2,9,8)],          # manually order the colors
                  pch=c(16,17,18,21,15))
fam2$color <- col$col[match(fam2$Order, col$Order)]
fam2$pch <- col$pch[match(fam2$Order, col$Order)]
# spiders = red
# flies = purple
# leps = blue



#### PLOTTING
pdf(file="Figure2_FamilyFOO.pdf", width=3.5,height=5.7)

par(mfrow=c(3,1), las=1, tcl=-0.25, cex.axis=0.9, cex.lab=0.9, cex.main=1, 
    mgp=c(1.5,0.4,0), mar=c(3,3,2,2))

## Panel A: Up
l <- fam2[which(fam2$Category=="Up"),]
plot(1:3, fam2[1,3:5], xlim=c(0.8,3.2), type="n", ylim=c(0,100), bty="l", ylab="FOO", xlab="", xaxt="n")
axis(side=1, labels=c("Early","Mid","Late"), at = c(1:3), mgp=c(1.8,0.1,0))
for(i in 1:nrow(l)){
  lines(1:3, l[i,3:5], type="b", pch=l$pch[i], lwd=1.5, col=l$color[i])
}
title(main="Upward trend", )
legend("topleft", pch=col$pch, col=col$col, legend=col$Order, bty="n", cex=0.9)


## Panel B: Down
l <- fam2[which(fam2$Category=="Down"),]
plot(1:3, fam2[1,3:5], xlim=c(0.8,3.2), type="n", ylim=c(0,100), bty="l", ylab="FOO", xlab="", xaxt="n")
axis(side=1, labels=c("Early","Mid","Late"), at = c(1:3), mgp=c(1.8,0.1,0))
for(i in 1:nrow(l)){
  lines(1:3, l[i,3:5], type="b", pch=l$pch[i], lwd=1.5, col=l$color[i])
}
title(main="Downward trend")


## Panel C: Nonlinear
l <- fam2[which(fam2$Category=="Both"),]
plot(1:3, fam2[1,3:5], xlim=c(0.8,3.2), type="n", ylim=c(0,100), bty="l", ylab="FOO", xlab="", xaxt="n")
axis(side=1, labels=c("Early","Mid","Late"), at = c(1:3), mgp=c(1.8,0.1,0))
for(i in 1:nrow(l)){
  lines(1:3, l[i,3:5], type="b", pch=l$pch[i], lwd=1.5, col=l$color[i])
}
title(main="Other trend")


dev.off()



