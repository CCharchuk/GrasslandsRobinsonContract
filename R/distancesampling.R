#distance sampling model 
#distance r
#g(r) = exp(-r^2/tau^2)
#tau^2 is the variance of the unfolded normal distribution describing the rate of distance decay
#assume r is measured without error and detection perfect at r=0
#tau = EDR for unlimited distance counts
# use subset with >= 2 distance intervals
#Standard practice is to model density as a function of tree cover, as this does not apply in grasslands
#we are going to use the null model of detection decay over distance. 
setwd("~/Documents/Employment/ECCC/Robinson_2017-18/BRobinsonContract/data")
pc <- read.csv("distancespp.csv")
library(Matrix)
library(mefa4)
names(pc)
xtDis <- Xtab(SumOfABUNDANCE ~ PKEY + DistanceID + GRASS_SPP, pc)
table(pc$DISTMETH)
#
spp <- as.matrix(xtDis$VESP)
spp <- as.data.frame(spp)
spp$PKEY <- row.names(spp)
#point counts containing target species
sppd <- pc[pc$GRASS_SPP=="VESP",]
spp <- spp[spp$PKEY%in%sppd$PKEY,]
spp <- plyr::join(spp, sppd, by = "PKEY")
spp$DistanceID <- NULL
spp <- unique(spp)

#bring in design matrix of distance interval endpoints
design <- read.csv("distancedesign.csv")
