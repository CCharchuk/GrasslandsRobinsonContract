#Calculate time since local sunrise (TSSR) for each PC
library(maptools)
?sunriset
#read in single species PC data
setwd("~/Documents/Employment/ECCC/Robinson_2017-18/BRobinsonContract/data")
spp_data <- read.csv("VESP_PC.csv")
#Calculate TSSR
#convert date and time to POSIX objects
spp_data$DateTime <- as.POSIXct(paste(spp_data$SurveyDate, spp_data$SurveyStartTime), format="%Y-%m-%d %H:%M")
spp_data$SurveyDate <- as.POSIXct(spp_data$SurveyDate, format="%Y-%m-%d")
#XY coords
coords <- as.matrix(spp_data[,c(10,11)])
#use maptools function (Lewin-Koh & Bivand 2012) to calculate sunrise time (Solymos et al. 2013)
spp_sunriset <- maptools::sunriset(coords, spp_data$SurveyDate, direction="sunrise", POSIXct.out=TRUE)
spp_data$sunriset <- spp_sunriset$time
#Now take the difference between survey time and sunrise time
#Negative values indicate survey was done before dawn, positive is after sunrise
spp_data$TSSR <- as.numeric(difftime(spp_data$DateTime, spp_data$sunriset, units="hours"))
check_na <- spp_data[is.na(spp_data$TSSR),]
#NAs are due to lack of data in SurveyStartTime
#Remove NAs
spp_data <- spp_data[!is.na(spp_data$TSSR),]
summary(spp_data$TSSR)
#center the data
spp_data$TSSR <- spp_data$TSSR/24

#Get julian date
spp_data$JDAY <- as.numeric(strftime(spp_data$SurveyDate, format="%j"))
summary(spp_data$JDAY)
#center the data
spp_data$JDAY <- spp_data$JDAY/365


spp_data <- spp_data[!spp_data$DURMETH=="J",]
table(spp_data$DURMETH)

spp_data$TSSR2 <- spp_data$TSSR^2
write.csv(spp_data, "spp_data.csv")

