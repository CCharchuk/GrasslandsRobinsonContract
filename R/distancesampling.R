#distance sampling model 
#distance r
#g(r) = exp(-r^2/tau^2)
#tau^2 is the variance of the unfolded normal distribution describing the rate of distance decay
#assume r is measured without error and detection perfect at r=0
#tau = EDR for unlimited distance counts
# use subset with >= 2 distance intervals
#Standard practice is to model density as a function of tree cover, as this does not apply in grasslands
#we are going to use the null model of detection decay over distance. 
library(detect)

#set working directory 
setwd("~/Documents/Employment/ECCC/Robinson_2017-18/BRobinsonContract/ECCC_2/data")
distcount <- read.csv("distcountmatrix.csv")
spp <- unique(distcount$GRASS_SPP)
#distdesign <- read.csv("distancedesign.csv")

distdesign <- read.table("DistanceQuery2.txt")
names(distdesign) <- c("PKEY","SumOfABUNDANCE","BehaviourID","GRASS_SPP","DistanceID","DISTMETH","DIST_END","Int1","Int2","Int3")
str(distdesign)
#distdesign2 <- read.csv("distancespp.csv")
#distdesign3 <- plyr::join(distdesign2, distdesign, by = c("DISTMETH"))
#distdesign3 <- distdesign3[distdesign3$PKEY%in%distcount$PKEY,]

#remove behaviour codes for non-singing indvs
table(distcount$BehaviourID)
distcount <- distcount[!distcount$BehaviourID=="2",]
distcount <- distcount[!distcount$BehaviourID=="4",]
distcount <- distcount[!distcount$BehaviourID=="5",]
#distdesign3 <- distdesign3[,c(1,6:9)]
#distdesign3 <- unique(distdesign3)

sppc <- distcount[distcount$GRASS_SPP=="GRSP",]
sppd <- distdesign[distdesign$PKEY%in%sppc$PKEY,]
sppd <- sppd[sppd$GRASS_SPP%in%sppc$GRASS_SPP,]
sppd <- unique(sppd[,c(1,6,8:10)])
#include continuous distance pc
#sppcontdist <- read.csv("ContDist.csv")
#sppcont <- sppcontdist[sppcontdist$GRASS_SPP=="VESP",]

#sppd <- distdesign3[distdesign3$PKEY%in%sppc$PKEY,]
#remove sampling designs that don't have distance intervals
#Remove DISTMETH D, F, O, X. Treat Z differently as cont dist
sppd <- sppd[!sppd$DISTMETH=="D",]
sppd <- sppd[!sppd$DISTMETH=="F",]
sppd <- sppd[!sppd$DISTMETH=="O",]
sppd <- sppd[!sppd$DISTMETH=="X",]
sppc <- sppc[sppc$PKEY%in%sppd$PKEY,]

#link up NAs 
fullmatrix <- plyr::join(sppd, sppc, by = "PKEY")

fullmatrix$MaxOfInterval1[is.na(fullmatrix$Int1)] <- NA
fullmatrix$MaxOfInterval2[is.na(fullmatrix$Int2)] <- NA
fullmatrix$MaxOfInterval3[is.na(fullmatrix$Int3)] <- NA

str(fullmatrix)
#extract counts and distance bins
sppcounts <- as.matrix(fullmatrix[,8:10])
sppdesign <- as.matrix(fullmatrix[,3:5])

dm1 <- cmulti(sppcounts | sppdesign ~ 1, type="dis")
dist_coef <- as.data.frame(t(coef(dm1)))
save(dist_coef, file="grsp_distcoef.rda")
