#### ---------------------------------------------------------------------------------------- ####
#### Prepare data and run simple models of the relationship between survey results and FOO
##   1) Prepare data for Malaise comparison and run model
##   2) Export a quick plot to show the model relationship (supplementary material)
#### ---------------------------------------------------------------------------------------- ####
library(vegan)
library(dplyr)

folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/Data"
setwd(folder)


#### ---------------- 1) Prepare data for Malaise comparison and run model ----------------- ####

#### Load data
m.all <- read.csv("Malaise.Order.2021.matrix.csv")
f <- read.csv("Malaise_fecal.csv")
f$Period[which(f$Period == "Mid")] <- "Middle"     # needs to match the Malaise dataset
f.mat <- f %>% select(Hemiptera.Pentatomidae:Panorpidae, ,-Coleoptera)

## Dataframes to receive results
mod.data <- data.frame(Taxon=rep(names(f.mat), times=3), 
                       Season=rep(c("Early","Middle","Late"), each=ncol(f.mat)),
                       F.FOO=NA, M.Captures=NA)
mod.data$RowID <- paste(mod.data$Taxon, mod.data$Season, sep="_")
f.foo <- data.frame(Method=rep("Fecal", ncol(f.mat)), Tax.group=names(f.mat), Early=NA, Middle=NA, Late=NA)


#### Organize Malaise data and add to mod.data
m.all$Lepidoptera <- apply(m.all[, c("Lepidoptera.Adult","Lepidoptera.Larvae")], 1, sum)
m.all$Diptera.Other <- apply(m.all[, c("Diptera.Tachinidae","Diptera.Other.HFT","Diptera.Other.MT")], 1, sum)
m <- m.all %>% select(-c(Lepidoptera.Adult, Lepidoptera.Larvae,Arachnida, Coleoptera,
                      Diptera.Tachinidae, Diptera.Other.HFT, Diptera.Other.MT))

## Combine Malaise abundances per season
m <- m %>% select(Survey, Hemiptera.Pentatomidae:Panorpidae, Lepidoptera, Diptera.Other)
m <- m %>% group_by(Survey) %>%
  summarize(across(everything(), sum)) 

## Add to mod.data
m2 <- data.frame(Taxon=rep(names(m)[2:ncol(m)], times=3),
                Season=rep(m$Survey, each=length(names(m)[2:ncol(m)])),
                M.Captures=NA)
m2$RowID <- paste(m2$Taxon, m2$Season, sep="_")
m2$M.Captures <- c(as.numeric(m[1, 2:ncol(m)]),       # Force data to correct format
                   as.numeric(m[2 ,2:ncol(m)]),
                   as.numeric(m[3, 2:ncol(m)]))
mod.data$M.Captures <- m2$M.Captures[match(mod.data$RowID, m2$RowID)]

## For easier comparisons, convert # of captures to % of total captures in a season
mod.data$Capture.sum <- rep(c(sum(mod.data$M.Captures[mod.data$Season=="Early"]),
                              sum(mod.data$M.Captures[mod.data$Season=="Middle"]),
                              sum(mod.data$M.Captures[mod.data$Season=="Late"])),
                            each=nrow(f.foo))
mod.data$M.CaptureFreq <- round((mod.data$M.Captures / mod.data$Capture.sum) * 100, 3)


#### Calculate FOO for fecal samples, separated by season
f.mat.e <- f.mat[which(f$Period=="Early"),]
f.mat.m <- f.mat[which(f$Period=="Middle"),]
f.mat.l <- f.mat[which(f$Period=="Late"),]
f.foo$Early <- round(specnumber(f.mat.e, MARGIN=2)/nrow(f.mat.e),3)*100
f.foo$Middle <- round(specnumber(f.mat.m, MARGIN=2)/nrow(f.mat.m),3)*100
f.foo$Late <- round(specnumber(f.mat.l, MARGIN=2)/nrow(f.mat.l),3)*100

## Add to mod.data
f.foo2 <- data.frame(Taxon=rep(f.foo$Tax.group, times=3),
                     Season=rep(c("Early","Middle","Late"), each=nrow(f.foo)),
                     F.Foo=NA)
f.foo2$RowID <- paste(f.foo2$Taxon, f.foo2$Season, sep="_")
f.foo2$F.Foo <- c(f.foo$Early, f.foo$Middle, f.foo$Late)     # Force data to correct format
mod.data$F.FOO <- f.foo2$F.Foo[match(mod.data$RowID, f.foo2$RowID)]


#### MODELING!
hist(mod.data$F.FOO, breaks=10)
hist(mod.data$M.CaptureFreq, breaks=10)
hist(log(mod.data$M.CaptureFreq+0.1), breaks=10) # Note that we could model this as a lognormal
# Tests show that lognormal model produces same results. But plot + residuals are harder to interpret. 

mod1 <- lm(M.CaptureFreq ~ F.FOO, data=mod.data)
summary(mod1)
confint(mod1)

# Strong, positive relationship. That's good news! 
plot(mod.data$F.FOO, mod.data$M.CaptureFreq, pch=16, col="#00000080",
     ylab="Malaise capture frequency (%)", xlab="Diet frequency of occurrence")
abline(mod1, lwd=2)
text(M.CaptureFreq ~ F.FOO, labels=RowID, data=mod.data, cex=0.7)

## Save the data and model
mod.data$Residuals <- mod1$residuals
setwd(folder)
setwd("../Output")
save(mod.data, mod1, file="Malaise_compare.RData")

## Optional: convert m to dataset to look at changes across seasons
# m.new <- as.data.frame(t(m))
# names(m.new) <- m.new[1,]
# m.new <- m.new[-1,]
# m.new <- m.new[,c("Early","Middle","Late")]
# m.new$Early <- round(as.numeric(m.new$Early)/sum(as.numeric(m.new$Early)), 3)*100
# m.new$Middle <- round(as.numeric(m.new$Middle)/sum(as.numeric(m.new$Middle)), 3)*100
# m.new$Late <- round(as.numeric(m.new$Late)/sum(as.numeric(m.new$Late)), 3)*100



#### ------------------------ 2) Plot for supplemental material ------------------------- ####

## Predict
y.pred <- predict(mod1, newdata = data.frame(F.FOO=1:100), interval="confidence", level=0.95)
y.pred <- cbind(data.frame(F.FOO=1:100), y.pred)

## Plot
jpeg(file="FigureS3_MalaiseCompare.jpg", width=5.5, height=5, units = "in", res = 600)

par(las=1, tcl=-0.25, cex.axis=0.9, cex.lab=0.9, mgp=c(1.5,0.4,0), mar=c(4,4,3,3))
plot(jitter(mod.data$F.FOO), mod.data$M.CaptureFreq, pch=16, col="#00000080", cex=1.2,
     ylab="Malaise capture frequency (%)", xlab="Diet frequency of occurrence")
polygon(x = c(y.pred$F.FOO, rev(y.pred$F.FOO)), y = c(y.pred$upr, rev(y.pred$lwr)), col = "#00000040", border = NA)
lines(x = y.pred$F.FOO, y = y.pred$fit, lwd = 2) 

dev.off()






