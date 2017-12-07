#Calculate time since local sunrise (TSSR) for each PC
library(maptools)
?sunriset
#read in single species PC data
setwd("~/Documents/Employment/ECCC/Robinson_2017-18/BRobinsonContract/ECCC_2/data")
all_pc <- read.csv("AllPointCounts.csv")
#Calculate TSSR
#convert date and time to POSIX objects
all_pc$DateTime <- as.POSIXct(paste(all_pc$SurveyDate, all_pc$SurveyStartTime), format="%Y-%m-%d %H:%M")
all_pc$SurveyDate <- as.POSIXct(all_pc$SurveyDate, format="%Y-%m-%d")
#XY coords
#remove NAs
all_pc <- all_pc[!is.na(all_pc$SurveyDate),]
all_pc <- all_pc[!is.na(all_pc$X),]
all_pc <- all_pc[!is.na(all_pc$Y),]
coords <- as.matrix(all_pc[,c(10,11)])
#use maptools function (Lewin-Koh & Bivand 2012) to calculate sunrise time (Solymos et al. 2013)
spp_sunriset <- maptools::sunriset(coords, all_pc$SurveyDate, direction="sunrise", POSIXct.out=TRUE)
all_pc$sunriset <- spp_sunriset$time
#Now take the difference between survey time and sunrise time
#Negative values indicate survey was done before dawn, positive is after sunrise
all_pc$TSSR <- as.numeric(difftime(all_pc$DateTime, all_pc$sunriset, units="hours"))
check_na <- all_pc[is.na(all_pc$TSSR),]
#NAs are due to lack of data in SurveyStartTime
#Remove NAs
all_pc <- all_pc[!is.na(all_pc$TSSR),]
summary(all_pc$TSSR)
#center the data
all_pc$TSSR <- all_pc$TSSR/24

#Get julian date
all_pc$JDAY <- as.numeric(strftime(all_pc$SurveyDate, format="%j"))
summary(all_pc$JDAY)
#center the data
all_pc$JDAY <- all_pc$JDAY/365


all_pc <- all_pc[!all_pc$DURMETH=="J",]
table(all_pc$DURMETH)

all_pc$TSSR2 <- all_pc$TSSR^2
write.csv(all_pc, "all_pc.csv")

