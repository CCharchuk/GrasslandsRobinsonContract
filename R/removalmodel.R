#setwd("E:/ECCC/data")
setwd("~/Documents/ECCC/data")
library(detect)
library(plyr)
#Load count data
counts <- read.csv("VESP_counts.csv")
names(counts)
names(counts) <- c("PKEY", "GRASS_SPP", "Int1","Int2","Int3","Int4","Int5","Int6","Int7","Int8","Int9","Int10","DURMETH")
#Loud design data
design <- read.csv("removaldesign.csv")
design$DURMETH <- design$DURATIONCODE
design$DURATIONCODE <- NULL
#Join data based on DURMETH
fullmatrix <- plyr::join(counts, design, by = "DURMETH")
#Subset data that has >=2 intervals
#Remove DURMETH A B D E J T AA Ya II
table(fullmatrix$DURMETH)
fullmatrix <- fullmatrix[!fullmatrix$DURMETH%in%c("A", "B", "D", "E", "J", "T", "AA", "Ya", "II"),]
fullmatrix$DURMETH <- factor(fullmatrix$DURMETH)
design <- fullmatrix[,c(15:24)]
#change 0 to NA 
fullmatrix$Int1[is.na(fullmatrix$Interval1)] <- NA
fullmatrix$Int2[is.na(fullmatrix$Interval2)] <- NA
fullmatrix$Int3[is.na(fullmatrix$Interval3)] <- NA
fullmatrix$Int4[is.na(fullmatrix$Interval4)] <- NA
fullmatrix$Int5[is.na(fullmatrix$Interval5)] <- NA
fullmatrix$Int6[is.na(fullmatrix$Interval6)] <- NA
fullmatrix$Int7[is.na(fullmatrix$Interval7)] <- NA
fullmatrix$Int8[is.na(fullmatrix$Interval8)] <- NA
fullmatrix$Int9[is.na(fullmatrix$Interval9)] <- NA
fullmatrix$Int10[is.na(fullmatrix$Interval10)] <- NA
counts <- fullmatrix[,c(1:13)]
Y <- as.matrix(counts[,c(3:12)])

#Adding covariates to model
covars <- read.csv("spp_data.csv")
covars <- covars[covars$PKEY%in%counts$PKEY,]
covars <- covars[,c(2,28,29,30)]
covars <- unique(covars)
#relink count data with point counts that we have TSSR data fro
counts <- counts[counts$PKEY%in%covars$PKEY,]
#
Y <- as.matrix(counts[,c(3:12)])
fullmatrix <- fullmatrix[fullmatrix$PKEY%in%counts$PKEY,]
design <- fullmatrix[,c(15:24)]
D <- as.matrix(design)
TSSR <- covars$TSSR
##models
m1 <- cmulti(Y | D ~ 1, type="rem")
coef(m1)
summary(m1)
#TSSR model
m2 <- cmulti(Y | D ~ TSSR, type="rem")
summary(m2)
#JDAY model
JDAY <- covars$JDAY
m3 <- cmulti(Y | D ~ JDAY, type="rem")
summary(m3)
m4 <- cmulti(Y | D ~ TSSR + JDAY, type="rem")
summary(m4)
m5 <- cmulti(Y | D ~ TSSR*JDAY, type="rem")
summary(m5)
bic=BIC(m1,m2,m3,m4,m5)
bic
exp(m3$coef)
