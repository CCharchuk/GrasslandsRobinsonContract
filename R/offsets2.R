install.packages("mefa4")
setwd("~/Documents/Employment/ECCC/Robinson_2017-18/BRobinsonContract/ECCC_2/data")
load("remcoef.rda")
load("ccsp_distcoef.rda")

dist_endpoints <- read.csv("Dist_Endpoints.csv")
#qpad approach to estimating density
library(mefa4)
#Expected count E[Y] at a given point count is the objective
#Equal to N p(tj)q(rk) where N is counted abundance, p is probability of singing given removal model, q is probability of detection given distance sampling
#Equal to D A p(tj)q(rk) where D is density given known area sampled A or estimated from EDR.
#q and p are estimable, A = pi r2k or effective area sampled can be estimated
#Idea - gather list of all PKEYs and append list of those without species present to list of those with species present
all_pc <- read.csv("all_pc.csv")
all_pc <- all_pc[,-1]
pc_endpoints <- read.csv("AllPointCounts.csv")
all_pc <- plyr::join(pc_endpoints, all_pc, by="PKEY")
pc_endpoints <- NULL
#going to set ARU recordings + method N to infinite dist
all_pc <- plyr::join(all_pc, dist_endpoints, by = "DISTMETH")
str(all_pc$MaxOfDIST_END)
all_pc$MaxOfDIST_END <- as.character(all_pc$MaxOfDIST_END)
all_pc$MaxOfDIST_END <- as.numeric(all_pc$MaxOfDIST_END)
summary(all_pc$MaxOfDIST_END)
#NAs are the "unk" class. Which is fine as we want to remove these anyways.
all_pc <- all_pc[!is.na(all_pc$MaxOfDIST_END),]
#species specific data
pcdat <- read.csv("spp_data.csv")
pcdat$logtau <- dist_coef$`log.tau_(Intercept)`
pcdat$tau <- exp(pcdat$logtau)
pcdat$sraint <- rem_coef$`log.phi_(Intercept)`
pcdat$srajday <- rem_coef$log.phi_JDAY
pcdat <- unique(pcdat[,c(2,13,16,28:29,31:34)])
#A <- ifelse(maxdist=unlimited, pitau^2, pi %*% maxdist^2)
#q <- ifelse(maxdist=unlimited, 1, tau)
distdesign <- read.table("DistanceQuery2.txt")
names(distdesign) <- c("PKEY","SumOfABUNDANCE","BehaviourID","GRASS_SPP","DistanceID","DISTMETH","DIST_END","Int1","Int2","Int3")
str(distdesign)
fulldata <- distdesign
distdesign <- NULL
fulldata <- fulldata[fulldata$GRASS_SPP=="VESP",]
fulldata <- fulldata[,-c(8:10)]
fulldata$DIST_END <- as.character(fulldata$DIST_END)
fulldata$DIST_END <- as.numeric(fulldata$DIST_END)
MaxDist <- aggregate(fulldata$DIST_END, list(fulldata$PKEY), max)
names(MaxDist) <- c("PKEY", "MAXDIST")
N <- aggregate(fulldata$SumOfABUNDANCE, list(fulldata$PKEY), sum)
names(N) <- c("PKEY", "N")
aggdata <- plyr::join(MaxDist, N, by="PKEY")
aggdata <- plyr::join(aggdata, pcdat)
#make sure there are no duplicates
aggdata <- unique(aggdata)
#Need to append with full point count list
aggdatafull <- plyr::join(all_pc, aggdata, by = "PKEY")
aggdatafull$N[is.na(aggdatafull$N)] <- 0
aggdatafull <- aggdatafull[,c(1:5,10:12, 40:43, 45, 50:53)]
aggdatafull <- unique(aggdatafull)
aggdatafull$logtau <- ccsp_dist_coef$`log.tau_(Intercept)`
aggdatafull$tau <- exp(aggdatafull$logtau)
aggdatafull$sraint <- ccsp_rem_coef$`log.phi_(Intercept)`
aggdatafull$srajday <- ccsp_rem_coef$log.phi_JDAY
aggdatafull$sratssr <- ccsp_rem_coef$log.phi_TSSR
aggdatafull$sratssrjday <- ccsp_rem_coef$`log.phi_TSSR:JDAY`


#Calculate area
aggdatafull$A <- ifelse(aggdatafull$MaxOfDIST_END=="Inf", pi*aggdatafull$tau^2, pi*aggdatafull$MAXDIST^2)
#Calculate perceptibility (q)
aggdatafull$q <- ifelse(aggdatafull$MaxOfDIST_END=="Inf", 1, (aggdatafull$tau^2/aggdatafull$MAXDIST^2)*(1-exp(-aggdatafull$MAXDIST^2/aggdatafull$tau^2)))
#Calculate probability of singing from sra
duration <- read.csv("totalduration.csv")
duration <- duration[,c(1,13)]
names(duration) <- c("DURMETH","MAXDUR")
aggdatafull <- plyr::join(aggdatafull,duration)
#phi and p calculations
phi <- exp(aggdatafull$sraint + (aggdatafull$srajday*aggdatafull$JDAY) + (aggdatafull$sratssr*aggdatafull$TSSR) + (aggdatafull$sratssrjday*aggdatafull$TSSR*aggdatafull$JDAY))

aggdatafull$p <- 1-exp(-(aggdatafull$MAXDUR*phi))

aggdatafull$offset <- log(aggdatafull$A) + log(aggdatafull$p) + log(aggdatafull$q)
save(aggdatafull, file="CCSPoffsets.rda")
