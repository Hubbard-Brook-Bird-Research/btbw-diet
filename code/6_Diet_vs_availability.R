#### ------------------------------------------------------------------------------------ ####
#### Use econullnetr to test diet against availability based on arthropod surveys
#### ------------------------------------------------------------------------------------ ####
library(dplyr)
library(econullnetr)
folder <- "C:/Users/astil/Dropbox/____Projects____/BTBW Diet/"


#### Load data
setwd(paste0(folder, "./Data"))
lep.diet <- read.csv("Leps_fecal.csv")    # diet data formatted for comparison to lep surveys
lep.survey <- read.csv("Leps.Family.2021.matrix.csv")
m.diet <- read.csv("Malaise_fecal.csv")   # diet data formatted for comparison to malaise surveys
m.survey <- read.csv("Malaise.Order.2021.matrix.csv")


#### -------------------------- 1. Lepidoptera data (3 families) --------------------------------- ####

#### Organize datasets
lep.diet <- lep.diet %>% select(-c(index:Species, Sex:BandCC)) %>%
  mutate(Stratum = paste0(Period, "-", Elevation)) %>%
  select(Stratum, Period:Notodontidae)
lep.survey$Survey[which(lep.survey$Survey == "Middle")] <- "Mid"
lep.survey <- lep.survey %>% select(-c(Arachnida,Pyralidae.Tortricidae.Coleophoridae.Psychidae:Total)) %>%
  rename(Period = Survey) %>%
  mutate(Stratum = paste0(Period, "-", Elevation)) %>%
  select(Stratum, Period:Notodontidae)

## Notodontidae not detected in Early-High and Early-Mid surveys, which causes an error.
# Replace it with an arbitrarily low constant of 0.01 biomass. 
lep.survey$Notodontidae[which(lep.survey$Notodontidae==0)] <- 0.01


#### Run the null model
lep.null <- generate_null_net(lep.diet[, 2:5], lep.survey[, 3:5], sims = 999,
                              data.type = "names", summary.type = "sum",
                              c.samples = lep.diet$Stratum,
                              r.samples = lep.survey$Stratum,
                              prog.count = FALSE)
lep.links <- test_interactions(lep.null, signif.level = 0.95)

## Visualize network plot
mean.abunds <- colMeans(lep.survey[, 3:5]) 
plot_bipartite(lep.null, signif.level = 0.95, 
               edge.cols = c("#67a9cf", "#F7F7F7", "#ef8a62"),
               low.abun = mean.abunds, abuns.type = "independent", 
               low.abun.col = "black", high.abun.col = "black",
               high.lablength = 5, low.lablength = 5)

## Visualize "preference plots"
plot_preferences(lep.null, "Early", signif.level = 0.95, 
                 type = "counts", xlab = "Number of samples", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3, xlim=c(0,30))
plot_preferences(lep.null, "Mid", signif.level = 0.95, 
                 type = "counts", xlab = "Number of samples", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3)
plot_preferences(lep.null, "Late", signif.level = 0.95, 
                 type = "counts", xlab = "Number of samples", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3)



#### -------------------------- 2. Malaise data ( families) --------------------------------- ####

#### Organize datasets
m.diet <- m.diet %>% select(-c(index:Species, Sex:BandCC, Coleoptera)) %>%
  mutate(Stratum = paste0(Period, "-", Elevation)) %>%
  select(Stratum, Period:Panorpidae)

## Malaise survey data needs a bit more work
m.survey$Survey[which(m.survey$Survey == "Middle")] <- "Mid"
m.survey$Lepidoptera <- apply(m.survey[, c("Lepidoptera.Adult","Lepidoptera.Larvae")], 1, sum)
m.survey$Diptera.Other <- apply(m.survey[, c("Diptera.Tachinidae","Diptera.Other.HFT","Diptera.Other.MT")], 1, sum)
m.survey <- m.survey %>% 
  rename(Period = Survey) %>%
  mutate(Stratum = paste0(Period, "-", Elevation)) %>%
  select(-c(Year:Elevation, Total.individuals.4mm:Arachnida, Lepidoptera.Adult, Lepidoptera.Larvae,
                      Diptera.Tachinidae, Diptera.Other.HFT, Diptera.Other.MT, Coleoptera, Other, Comments))

m.survey <- m.survey[names(m.diet)]

## There are issues with rare taxa which aren't detected at all or are detected 0 times in certain strata
# To avoid basing results on these rare captures, only keep taxa with at least 15 total Malaise captures.
colSums(m.survey[, 3:15])
taxa.keep <- names(m.survey[, 3:15])[colSums(m.survey[, 3:15]) >= 15]

m.survey.filter <- m.survey %>% select(Stratum, Period, all_of(taxa.keep))

## There are still a few 0's which in this case we can replace with a low number. See ?generate_null_net.
m.survey.filter <- m.survey.filter %>%
  mutate(across(Plecoptera:Panorpidae, ~ replace(., . ==  0 , 0.1)))

## Match column names and rename columns to aid plotting labels
m.diet.filter <- m.diet[names(m.survey.filter)]

m.diet.rename <- m.diet.filter %>%
  rename(
    Diptera_Other = Diptera.Other,
    Diptera_Rhagionidae = Diptera.Rhagionidae,
    Diptera_Tipulidae = Diptera.Tipulidae,
    Hymenoptera_Ichneumonidae = Hymenoptera.Ichneumonids,
    Hymenoptera_Other = Hymenoptera.Not.Ichneumonids,
    Mecoptera_Panorpidae = Panorpidae)
m.survey.rename <- m.survey.filter %>%
  rename(
    Diptera_Other = Diptera.Other,
    Diptera_Rhagionidae = Diptera.Rhagionidae,
    Diptera_Tipulidae = Diptera.Tipulidae,
    Hymenoptera_Ichneumonidae = Hymenoptera.Ichneumonids,
    Hymenoptera_Other = Hymenoptera.Not.Ichneumonids,
    Mecoptera_Panorpidae = Panorpidae)


#### Run the null model
m.null <- generate_null_net(m.diet.rename[, 2:11], m.survey.rename[, 3:11], sims = 999,
                              data.type = "names", summary.type = "sum",
                              c.samples = m.diet.rename$Stratum,
                              r.samples = m.survey.rename$Stratum,
                              prog.count = FALSE)
m.links <- test_interactions(m.null, signif.level = 0.95)


## Visualize "preference plots"
plot_preferences(m.null, "Early", signif.level = 0.95, 
                 type = "counts", xlab = "Number of samples", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3, xlim=c(0,30))
plot_preferences(m.null, "Mid", signif.level = 0.95, 
                 type = "counts", xlab = "Number of samples", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3, oma=c(0,0,0,0))
plot_preferences(m.null, "Late", signif.level = 0.95, 
                 type = "counts", xlab = "Number of samples", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3)

