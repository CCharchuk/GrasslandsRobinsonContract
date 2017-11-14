install.packages("mefa")
#qpad approach to estimating density
install.packages("mefa4")
library(mefa4)
#Expected count E[Y] at a given point count is the objective
#Equal to N p(tj)q(rk) where N is counted abundance, p is probability of singing given removal model, q is probability of detection given distance sampling
#Equal to D A p(tj)q(rk) where D is density given known area sampled A or estimated from EDR.
#q and p are estimable, A = pi r2k or effective area sampled can be estimated
sra_int <- m3$coef[1]
sra_jday <- m3$coef[2]
tau <- dm1$coef
fulldata <- read.csv("spp_data.csv")
fulldata$tau <- tau
fulldata$sraint <- sra_int
fulldata$srajday <- sra_jday

#A <- ifelse(maxdist=unlimited, pitau^2, pi %*% maxdist^2)
#q <- ifelse(maxdist=unlimited, 1, tau)
fulldata <- distdesign
fulldata <- fulldata[fulldata$GRASS_SPP=="VESP",]
fulldata <- fulldata[,-c(8:10)]
fulldata$DIST_END <- as.character(fulldata$DIST_END)
fulldata$DIST_END <- as.numeric(fulldata$DIST_END)
MaxDist <- aggregate(fulldata$DIST_END, list(fulldata$PKEY), max)
names(MaxDist) <- c("PKEY", "MAXDIST")
N <- aggregate(fulldata$SumOfABUNDANCE, list(fulldata$PKEY), sum)
names(N) <- c("PKEY", "N")
plyr::join(MaxDist, N)
pcdat <- read.csv("spp_data.csv")                     
pcdat <- unique(pcdat[,c(2,13,16,28:29)])
aggdata <- plyr::join(MaxDist, N)
aggdata <- plyr::join(aggdata, pcdat)
aggdata <- unique(aggdata)
aggdata$tau <- exp(dm1$coef)
aggdata$sraint <- sra_int
aggdata$srajday <- sra_jday
#Calculate area
aggdata$A <- ifelse(aggdata$MAXDIST=="Inf", pi*aggdata$tau^2, pi*aggdata$MAXDIST^2)
#Calculate perceptibility (q)
aggdata$q <- ifelse(aggdata$MAXDIST=="Inf", 1, (aggdata$tau^2/aggdata$MAXDIST^2)*(1-exp(-aggdata$MAXDIST^2/aggdata$tau^2)))
#Calculate probability of singing from sra
duration <- read.csv("totalduration.csv")
duration <- duration[,c(1,13)]
names(duration) <- c("DURMETH","MAXDUR")
aggdata <- join(aggdata,duration)
aggdata$p <- 1-exp(-aggdata$MAXDUR - aggdata$sraint)
