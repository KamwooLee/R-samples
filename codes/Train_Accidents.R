# Statistical Modeling
# Author: Kamwoo Lee
# Last modified: 9/28/2016



#############################################
#
#
# Train Accidents
#
#
#############################################

# Library
library(lattice)
library(gplots)
library(boot)
library(MASS)


source("AccidentInput.R")
source("SPM_Panel.R")
source("PCAplots.R")
source("TestSet.R")
source("http://www.phaget4.org/R/myImagePlot.R")

# Read & Clean data
# First - a list of data frames for each year of accident data
acts <- file.inputl(datadir)
sapply(acts, dim)
# Next a data frame with all accidents from all years from 2001 - 2015 with columns that are consistent for all of these years
# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
# the combined data frame
totacts <- combine.data(acts, comvar)
dim(totacts)
# ~Read & Clean data


# factorize Categorical Variables
# Type
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))
# Typeq
totacts$TYPEQ <- as.numeric(totacts$TYPEQ)
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work", "Single", "CutofCars", "Yard", "Light", "Maint"))
# Cause
totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)
# Weather
totacts$Weather <- factor(totacts$WEATHER, labels = c("clear", "cloudy", "rain", "fog", "sleet", "snow"))
contrasts(totacts$Weather)
# RainSnow
totacts$RainSnow <- rep("No", nrow(totacts))
totacts$RainSnow[which(totacts$Weather == "rain" | totacts$Weather == "snow")] <- "Yes"
totacts$RainSnow <- as.factor(totacts$RainSnow)
contrasts(totacts$RainSnow)<-matrix(c(1,0),nrow=2)
colnames(contrasts(totacts$RainSnow)) <-matrix(c("Yes"),ncol=1)
contrasts(totacts$RainSnow)
# Visiblty
totacts$Visiblty <- factor(totacts$VISIBLTY, labels = c("dawn", "day", "dusk", "dark"))
contrasts(totacts$Visiblty)<-matrix(c(1,0,0,0,  0,0,1,0,  0,0,0,1),nrow=4)
colnames(contrasts(totacts$Visiblty)) <-matrix(c("dawn","dusk","dark"),ncol=3)
contrasts(totacts$Visiblty)
# Dark
totacts$Dark <- rep("No", nrow(totacts))
totacts$Dark[which(totacts$Visiblty == "dark")] <- "Yes"
totacts$Dark <- as.factor(totacts$Dark)
contrasts(totacts$Dark)<-matrix(c(1,0),nrow=2)
colnames(contrasts(totacts$Dark)) <-matrix(c("Yes"),ncol=1)
contrasts(totacts$Dark)
# Derail
totacts$Derail <- rep("No", nrow(totacts))
totacts$Derail[which(totacts$TYPE == "Derailment")] <- "Yes"
totacts$Derail <- as.factor(totacts$Derail)
contrasts(totacts$Derail)<-matrix(c(1,0),nrow=2)
colnames(contrasts(totacts$Derail)) <-matrix(c("Yes"),ncol=1)
contrasts(totacts$Derail)
# HwyRail
totacts$HwyRail <- rep("No", nrow(totacts))
totacts$HwyRail[which(totacts$TYPE == "Hwy-Rail")] <- "Yes"
totacts$HwyRail <- as.factor(totacts$HwyRail)
contrasts(totacts$HwyRail)<-matrix(c(1,0),nrow=2)
colnames(contrasts(totacts$HwyRail)) <-matrix(c("Yes"),ncol=1)
contrasts(totacts$HwyRail)
# Region
totacts$Region <- factor(totacts$REGION)
contrasts(totacts$Region)
# BrokenRail
totacts$BrokenRail <- rep("No", nrow(totacts))
totacts$BrokenRail[which(totacts$CAUSE == "T201"|
                         totacts$CAUSE == "T202"|
                         totacts$CAUSE == "T203"|
                           totacts$CAUSE == "T204"|
                           totacts$CAUSE == "T207"|
                           totacts$CAUSE == "T208"|
                           totacts$CAUSE == "T210"|
                           totacts$CAUSE == "T211"|
                           totacts$CAUSE == "T212"|
                           totacts$CAUSE == "T218"|
                           totacts$CAUSE == "T220"|
                           totacts$CAUSE == "T221")] <- "Yes"
totacts$BrokenRail <- as.factor(totacts$BrokenRail)
contrasts(totacts$BrokenRail)<-matrix(c(1,0),nrow=2)
colnames(contrasts(totacts$BrokenRail)) <-matrix(c("Yes"),ncol=1)
contrasts(totacts$BrokenRail)

# Imo
totacts$Imo <- factor(totacts$IMO)
# Rcl
totacts$Rcl <- factor(totacts$RCL)
# Timehr24
totacts$Timehr24 <- totacts$TIMEHR
totacts$Timehr24[(totacts$AMPM == "PM") & (totacts$TIMEHR < 12)] <- totacts$TIMEHR[(totacts$AMPM == "PM") & (totacts$TIMEHR < 12)] + 12
totacts$Timehr24 <- factor(totacts$Timehr24)



# Numeric 
totacts$Engrs <- as.numeric(totacts$ENGRS)
totacts$Firemen <- as.numeric(totacts$FIREMEN)
totacts$Conductr <- as.numeric(totacts$CONDUCTR)
totacts$Brakemen <- as.numeric(totacts$BRAKEMEN)
totacts$Workers <- totacts$Engrs + totacts$Firemen + totacts$Conductr + totacts$Brakemen
totacts$Enghr <- as.numeric(totacts$ENGHR)
totacts$Cdtrhr <- as.numeric(totacts$CDTRHR)

# Add log variables
totacts$Casualties <- totacts$TOTINJ + totacts$TOTKLD
totacts$logCslt <- log10(totacts$Casualties)
totacts$logAccdmg <- log10(totacts$ACCDMG+1)
totacts$logTrnspd <- log10(totacts$TRNSPD+1)
totacts$logCars <- log10(totacts$CARS)
totacts$logTons <- log10(totacts$TONS+1)

# Add sqrt variables
totacts$sqrtCslt <- sqrt(totacts$Casualties)
totacts$sqrtAccdmg <- sqrt(totacts$ACCDMG)
totacts$sqrtTrnspd <- sqrt(totacts$TRNSPD)
totacts$sqrtCars <- sqrt(totacts$CARS)
totacts$sqrtTons <- sqrt(totacts$TONS)



# Build a data frame xdmg with only extreme accidents for ACCDMG and Remove duplicates
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
rownames(xdmgnd) <- NULL
table(xdmgnd$CAUSE)
table(xdmgnd$Cause)

# build a data frame cdmg with only accidents that have casualties and Remove duplicates
cdmg <- totacts[totacts$Casualties > 0,]
cdmgnd <- cdmg[!(duplicated(cdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
rownames(cdmgnd) <- NULL
table(cdmgnd$CAUSE)



mean(xdmgnd[xdmgnd$Derail == "Yes", "ACCDMG"])
mean(xdmgnd[xdmgnd$Derail == "No", "ACCDMG"])
#############################################
#
# 1. Problem Description
#
#############################################

#############################################
# 1.1 Situation
#############################################


par(mfrow = c(1,2))
# Total Number of accident per Year
plot(2001:2015, as.numeric(table(totacts$IYR)), type = "l", ylab = "Count", xlab = "Year", main = "Total Number of accident per Year")


# Total Damage per Year
plot(2001:2015, tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum), type = "l", ylab = "Damage ($)", xlab = "Year", main = "Total Damage per Year")
par(mfrow = c(1,1))


# Total Casualties per Year
plot(2001:2015, tapply(totacts$Casualties, as.factor(totacts$YEAR), sum), type = "l", ylab = "Casualties (Death + Injury)", xlab = "Year", main = "Total Damage per Year")



# Frequency of accident types
par(mfrow = c(1,2))
barplot(table(totacts$TYPE))
barplot(log10(table(totacts$TYPE))) #logarithmic scale
par(mfrow = c(1,1))


# Frequency of car types and Cause
par(mfrow = c(1,2))
barplot(table(totacts$TYPEQ))
barplot(table(totacts$Cause))
par(mfrow = c(1,1))


# Ordered damage costs
nrow(totacts)
x <- c(1:49436)
y <- totacts[order(totacts$ACCDMG), "ACCDMG"]
plot(x,y, type="o", xlab="accidents ordered by ACCDMG", ylab="Damage costs")
# damage distribution
dmgbox <- boxplot(totacts$ACCDMG)
# 13% of accidents are extreme
length(dmgbox$out)/nrow(totacts)
# extreme damages take up 75% of total damage
sum(xdmg$ACCDMG) /sum(totacts$ACCDMG)


# 5% of accidents has casualties
barplot(log10(table(totacts$TYPE)))
totacts$Casualties <- totacts$TOTINJ + totacts$TOTKLD
boxplot(totacts$Casualties)
length(totacts$Casualties[totacts$Casualties>0])/length(totacts$Casualties)




nrow(totacts[(totacts$ACCDMG > dmgbox$stats[5] & totacts$Casualties>0),])
sum(totacts$ACCDMG > dmgbox$stats[5])
sum(totacts$Casualties>0)


# Histogram
hist(log10(totacts$ACCDMG))
hist(log10(totacts$TOTKLD))
hist(log10(totacts$TOTINJ))
hist(log10(totacts$Casualties))



# Barplot
barplot(table(totacts$TYPE))
barplot(log10(table(totacts$TYPE)))






# Boxplot
# Boxplot for accident damage
boxplot(totacts$ACCDMG)
# Boxplot for accident damage by year
boxplot(ACCDMG ~ IYR, data = totacts)
boxplot(ACCDMG ~ yr, data = cbind(totacts, yr = totacts$IYR+2000), ylab = "Damage", xlab = "Year", main = "Boxplot for accident damage")

# Total Number of accident per Year   
plot(2001:2015, tapply(totacts$IYR, as.factor(totacts$YEAR), length), type = "l", ylab = "Count", xlab = "Year", main = "Total Number of accident per Year")    











plot(cdmgnd$Weather, cdmgnd$Casualties, pch=19)

table(totacts$Cause, totacts$TYPE) 
table(totacts$Cause[totacts$Casualties > 0], totacts$TYPE[totacts$Casualties > 0]) 
table(totacts$Cause[totacts$Casualties > 0], totacts$TYPE[totacts$Casualties > 0]) / table(totacts$Cause, totacts$TYPE) *100
round(table(totacts$TYPEQ[totacts$Casualties > 5], totacts$TYPE[totacts$Casualties > 5]) / table(totacts$TYPEQ, totacts$TYPE) *100)


dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
table(xdmg$Cause, xdmg$TYPE) / table(totacts$Cause, totacts$TYPE) *100
round(table(xdmgnd$TYPEQ, xdmgnd$TYPE) / table(totacts$TYPEQ, totacts$TYPE) *100)
round(table(cdmgnd$TYPEQ, cdmgnd$TYPE) / table(totacts$TYPEQ, totacts$TYPE) *100)

plot(totacts$BRAKEMEN[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])
plot(totacts$BRAKEMEN, totacts$Casualties)
plot(totacts$FIREMEN[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])
plot(totacts$ENGRS[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])
plot(totacts$ENGHR[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])
plot(totacts$CDTRHR[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])
plot(totacts$CDTRHR, totacts$Casualties)
plot(totacts$BRAKEMEN, totacts$Casualties)

plot(totacts$TEMP[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])
plot(totacts$TIMEHR[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])

plot(totacts$TONS[totacts$TYPE == "Hwy-Rail"], totacts$Casualties[totacts$TYPE == "Hwy-Rail"])


plot(cdmgnd$Weather[cdmgnd$TYPE == "Hwy-Rail"], cdmgnd$TRNSPD[cdmgnd$TYPE == "Hwy-Rail"])

plot(cdmgnd$Weather, cdmgnd$TRNSPD)




plot(totacts$BRAKEMEN[totacts$TYPE == "HeadOn"], totacts$ACCDMG[totacts$TYPE == "HeadOn"])
plot(totacts$TEMP[totacts$TYPE == "HeadOn"], totacts$ACCDMG[totacts$TYPE == "HeadOn"])


plot(totacts$BRAKEMEN[totacts$TYPE == "Derailment"], totacts$ACCDMG[totacts$TYPE == "Derailment"])
plot(totacts$RMAN1[totacts$TYPE == "Derailment"], totacts$logAccdmg[totacts$TYPE == "Derailment"])
plot(totacts$Cause[totacts$TYPE == "Derailment"], totacts$logAccdmg[totacts$TYPE == "Derailment"])


length(totacts[totacts$ACCDMG < 100000, "ACCDMG"]) / nrow(totacts)


#############################################
# 1.2 Goal
#############################################





#############################################
# 1.3 Metrics
#############################################

xdmgnd.pca <- princomp(xdmgnd[-c(458, 3250, 1968, 5738),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)
xdmgnd.pca <- princomp(xdmgnd[-c(5738, 3250, 458),c("CARS","CARSDMG", "TEMP","TRNSPD", "HIGHSPD", "TONS", "EQPDMG", "TRKDMG", "ACCDMG", "TOTINJ", "TOTKLD")], cor = T)
xdmgnd.pca <- princomp(xdmgnd[-c(458,5738,3250,1968),c("EQPDMG", "TRKDMG", "ACCDMG", "TOTINJ", "TOTKLD")], cor = T)
biplot(xdmgnd.pca)
barplot(xdmgnd.pca$loadings[,1])
barplot(xdmgnd.pca$loadings[,2])

cdmgnd.pca <- princomp(cdmgnd[-c(285,220),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)
cdmgnd.pca <- princomp(cdmgnd[-c(85,2702),c("CARS","CARSDMG", "TEMP","TRNSPD", "HIGHSPD", "TONS", "EQPDMG", "TRKDMG", "ACCDMG", "TOTINJ", "TOTKLD")], cor = T)
cdmgnd.pca <- princomp(cdmgnd[-c(85,2702),c("EQPDMG", "TRKDMG", "ACCDMG", "TOTINJ", "TOTKLD")], cor = T)
biplot(cdmgnd.pca)
barplot(cdmgnd.pca$loadings[,1])
barplot(cdmgnd.pca$loadings[,2])




#############################################
# 1.4 Hypotheses
#############################################


uva.pairs(xdmgnd[,c("logAccdmg", "Brakemen", "Firemen", "Engrs", "Conductr")])
uva.pairs(cdmgnd[cdmgnd$TYPE == "Hwy-Rail",c("logCslt", "Brakemen", "Firemen", "Engrs", "Conductr")])



uva.pairs(xdmgnd[,c("logAccdmg", "Timehr24", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1", "MIDMAN1", "RMAN1", "RREM1", "CABOOSE1")])
uva.pairs(cdmgnd[,c("Casualties", "TIMEHR", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1", "MIDMAN1", "RMAN1", "RREM1", "CABOOSE1")])
uva.pairs(xdmgnd[cdmgnd$TYPE == "Derailment" & cdmgnd$TYPEQ == "Freight" ,c("logAccdmg", "Timehr24", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1", "MIDMAN1", "RMAN1", "RREM1", "CABOOSE1")])
uva.pairs(cdmgnd[cdmgnd$TYPE == "Hwy-Rail" & cdmgnd$TYPEQ == "Freight"  ,c("logCslt", "Timehr24", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1", "MIDMAN1", "RMAN1", "RREM1", "CABOOSE1")])


# DERAILMENT type accidents increases ACCDMG when an accident happens.
table(xdmgnd$CAUSE)
round(table(xdmgnd$TYPEQ, xdmgnd$TYPE) / table(totacts$TYPEQ, totacts$TYPE) *100)

# HWY-RAIL type accidents increases number of Casualties when an accident happens.
table(cdmgnd$CAUSE)
round(table(cdmgnd$TYPEQ, cdmgnd$TYPE) / table(totacts$TYPEQ, totacts$TYPE) *100)

# Time of day affects HWY-RAIL type accidents.
hist(cdmgnd$Timehr24[cdmgnd$TYPE == "Hwy-Rail"])
uva.pairs(cdmgnd[cdmgnd$TYPE == "Hwy-Rail",c("Casualties", "Timehr24", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1", "MIDMAN1", "RMAN1", "RREM1", "CABOOSE1")])
interaction.plot(cdmgnd$TIMEHR, cdmgnd$TYPE, cdmgnd$Casualties)




xyplot(logAccdmg~TONS | Cause, data = xdmgnd, type = c("p", "r"))
xyplot(Casualties~Dark | Weather, data = cdmgnd[cdmgnd$TYPE == "Hwy-Rail",], type = c("p", "r"))
xyplot(Casualties~Dark | Weather, data = cdmgnd, type = c("p", "r"))
xyplot(Casualties~Weather | Dark, data = cdmgnd, type = c("p", "r"))



xyplot(TRNSPD~Weather, data = cdmgnd[cdmgnd$TYPE == "Hwy-Rail",], type = c("p", "r"))



interaction.plot(xdmgnd$TYPE, xdmgnd$BRAKEMEN, xdmgnd$ACCDMG)


xyplot(Casualties~TRNSPD | Visiblty, data = cdmgnd, type = c("p", "r"))

myImagePlot(table(xdmgnd$Cause, xdmgnd$TYPE), title = "No. of Accidents by Cause and Type of Accident")


# Visibility affects casualties
TrainSpeed<-cdmgnd$TRNSPD
TrainSpeed[which(cdmgnd$TRNSPD<30)]<- 0
TrainSpeed[which(cdmgnd$TRNSPD>=30)]<- 1
TrainSpeed <- factor(TrainSpeed, labels= c("low train speed", "high train speed"))
interaction.plot(TrainSpeed, cdmgnd$Visiblty, cdmgnd$Casualties, xlab="Train Speed", ylab="Mean Casualties", trace.label= "Visibility" )


TrainSpeed <- cdmgnd$TRNSPD[cdmgnd$TYPE == "Hwy-Rail"]
TrainSpeed[which(TrainSpeed<30)]<- 0
TrainSpeed[which(TrainSpeed>=30)]<- 1
TrainSpeed <- factor(TrainSpeed, labels= c("low train speed", "high train speed"))
interaction.plot(TrainSpeed, 
                 cdmgnd$Visiblty[cdmgnd$TYPE == "Hwy-Rail"], 
                 cdmgnd$Casualties[cdmgnd$TYPE == "Hwy-Rail"], xlab="Train Speed", ylab="Mean Casualties", trace.label= "Visibility" )

TrainSpeed<-cdmgnd$TRNSPD
TrainSpeed[which(cdmgnd$TRNSPD<40)]<- 0
TrainSpeed[which(cdmgnd$TRNSPD>=40)]<- 1
TrainSpeed <- factor(TrainSpeed, labels= c("low train speed", "high train speed"))
interaction.plot(TrainSpeed, cdmgnd$Dark, cdmgnd$Casualties, xlab="Train Speed", ylab="Mean Casualties", trace.label= "Dark" )


#############################################
#
# 2. Approach
#
#############################################

#############################################
# 2.1 Data
#############################################

hist(totacts$ACCDMG%%1000)
head(totacts$ACCDMG, 200)

hist(totacts$TRKDMG%%100)
head(totacts$TRKDMG, 100)

hist(totacts$EQPDMG%%10000)
head(totacts$EQPDMG, 100)

hist(totacts$TIMEMIN)
head(totacts$TIMEMIN, 50)

hist(totacts$TRNSPD)
head(totacts$TRNSPD, 50)


hist(totacts$TONS10)
head(totacts$TONS, 50)

nrow(totacts[totacts$TRNSPD == 0, ])
nrow(cdmgnd[cdmgnd$TRNSPD == 0, ])
nrow(xdmgnd[xdmgnd$TRNSPD == 0, ])
nrow(totacts)
nrow(totacts[totacts$TONS == 0, ]) / nrow(totacts)
nrow(cdmgnd[cdmgnd$TONS == 0, ])
nrow(xdmgnd[xdmgnd$TONS == 0, ])


#############################################
# 2.2 Analysis
#############################################

# select predictors
uva.pairs(xdmgnd[,c("logAccdmg", "Timehr24", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1", "MIDMAN1", "RMAN1", "RREM1", "CABOOSE1")])
uva.pairs(xdmgnd[,c("ACCDMG", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1")])
uva.pairs(cdmgnd[,c("Casualties", "CARS", "TEMP", "TRNSPD", "TONS", "HEADEND1")])
uva.pairs(cdmgnd[,c("sqrtCslt", "TRNSPD", "TONS", "CARS", "TEMP", "HEADEND1")])


# build linear model
lm.cas1 = lm(sqrtCslt ~ (TRNSPD + TONS + TEMP + Visiblty)^2, data=totacts)
summary(lm.cas1)

lm.cas1 = lm(Casualties ~ (TRNSPD + TONS + TEMP + Visiblty)^2, data=cdmgnd)
summary(lm.cas1)


lm.cas1 = lm(sqrtCslt ~ (Visiblty + CARS + TEMP + TRNSPD)^2, data=cdmgnd)
summary(lm.cas1)

lm.cas1 = lm(Casualties ~ (Visiblty + CARS + TEMP + TRNSPD)^2, data=cdmgnd)
summary(lm.cas1)


lm.cas1 = lm(Casualties ~ (HwyRail + CARS + TEMP + TRNSPD)^2, data=cdmgnd)
summary(lm.cas1)

lm.cas1 = lm(sqrtCslt ~ HwyRail, data=cdmgnd)
summary(lm.cas1)


lm.cas1 = lm(Casualties ~ (HwyRail + Visiblty + CARS + TEMP + TRNSPD + HEADEND1)^2, data=cdmgnd)
summary(lm.cas1)
lm.cas1 = lm(sqrtCslt ~ (HwyRail + Visiblty + CARS + TEMP + TRNSPD + HEADEND1)^2, data=cdmgnd)
summary(lm.cas1)
lm.cas1 = lm(logCslt ~ (HwyRail + Visiblty + Weather + CARS + TEMP + TRNSPD + HEADEND1)^2, data=cdmgnd)
summary(lm.cas1)




lm.cas1 = lm(ACCDMG ~ (Derail + TRNSPD + CARS + TONS)^2, data=xdmgnd)
summary(lm.cas1)


lm.cas1 = lm(ACCDMG ~ (Derail + Region + TRNSPD + TONS)^2, data=xdmgnd)
summary(lm.cas1)




lm.cas1 = lm(Casualties ~ (TRNSPD + CARS + TEMP + Visiblty)^2, data=cdmgnd)
summary(lm.cas1)


lm.cas1 = lm(sqrtCslt ~ (TRNSPD + CARS + Visiblty)^2 , data=totacts)
summary(lm.cas1)

lm.cas1 = lm(sqrtCslt ~ (TRNSPD + CARS + TEMP + Visiblty)^2 , data=cdmgnd)
summary(lm.cas1)

lm.cas1 = lm((Casualties)^(-0.5) ~ TRNSPD + CARS + TEMP + TYPE + Workers, data=cdmgnd)
summary(lm.cas1)
lm.cas1 = lm((Casualties)^(-0.5) ~ TRNSPD+Visiblty, data=cdmgnd[cdmgnd$CAUSE=="M302",])
summary(lm.cas1)
lm.cas1 = lm((Casualties)^(-0.5) ~ TRNSPD+Visiblty, data=totacts[totacts$CAUSE=="M302",])
summary(lm.cas1)
lm.cas1 = lm(Casualties ~ TRNSPD + CARS + TEMP + TYPE + Workers, data=cdmgnd)
summary(lm.cas1)

lm.cas1 = lm(Casualties ~ REGION, data=cdmgnd)
summary(lm.cas1)

lm.cas1 = lm(ACCDMG ~ TRNSPD + CARS + TYPE + REGION, data=xdmgnd)
summary(lm.cas1)

lm.cas1 = lm(Casualties ~ TRNSPD + Weather + Visiblty, data=cdmgnd)
summary(lm.cas1)
lm.cas1 = lm(logCslt ~ TRNSPD + Weather + Visiblty, data=cdmgnd)
summary(lm.cas1)

lm.cas1 = lm(ACCDMG ~ TRNSPD + Weather + Visiblty, data=xdmgnd)
summary(lm.cas1)

lm.cas1 = lm(ACCDMG ~ (Derail + TRNSPD + TONS + HEADEND1)^2, data=xdmgnd)
summary(lm.cas1)

lm.cas1 = lm(ACCDMG ~ (Derail + Region + TRNSPD)^2, data=xdmgnd)
summary(lm.cas1)

lm.cas1 = lm(logAccdmg ~ TRNSPD + (Weather + Visiblty)^2, data=xdmgnd)
summary(lm.cas1)

lm.cas1 = lm(ACCDMG ~ TRNSPD + (Weather + Visiblty)^2, data=xdmgnd)
summary(lm.cas1)


lm.cas2 = lm(Casualties ~ TRNSPD + TONS + CARS + TEMP + HEADEND1, data=cdmgnd)
summary(lm.cas2)




ACCDMG +1 ~ (ENGRS_N + CONDUCTR_N + BRAKEMEN_N + I((ENGHR*60)+ENGMIN + (CDTRHR*60)+CDTRMIN))^2
lm.cas1 = lm(ACCDMG +1 ~ (Engrs + Conductr + Brakemen + I(Enghr + Cdtrhr))^2, data=totacts)
lm.cas1 = lm(ACCDMG +1 ~ (Engrs + Conductr + Brakemen + I(Enghr + Cdtrhr))^2, data=xdmgnd)

# stepwise regression
lm.cas1.step <- step(lm.cas1) 
summary(lm.cas1.step)


# partial F test
anova(lm.cas1,lm.cas1.step)


# AIC:
AIC(lm.cas1)
AIC(lm.cas2)
AIC(lm.cas2.step)


# BIC:
AIC(lm.cas1,k=log(nrow(cdmgnd)))
AIC(lm.cas1,k=log(nrow(cdmgnd)))
AIC(lm.cas1.step,k=log(nrow(cdmgnd)))












#############################################
#
# 3. Evidence
#
#############################################
#Diagnostics plot
par(mfrow=c(2,2))
plot(lm.cas1, labels.id = NULL)
#plot(lm.cas1)
par(mfrow=c(1,1)) 

#Diagnostics plot
par(mfrow=c(2,2))
plot(lm.cas1.step, labels.id = NULL)
#plot(lm.cas1)
par(mfrow=c(1,1)) 


# remove outliers
cdmgnd[220,]
cdmgnd[2182,]
cdmgnd <- cdmgnd[-c(220),]
xdmgnd <- xdmgnd[-c(5738),]
cdmgnd[220, c("Casualties", "CARS", "TEMP", "TRNSPD")]
cdmgnd[2182, ]
cdmgnd[2182, c("Casualties", "CARS", "TEMP", "TRNSPD")]
# go to 2-2
xdmgnd[5738, c("ACCDMG", "CARS", "TONS", "TRNSPD")]

# transformation
boxcox(lm.cas1.step) #box-cox plot
boxcox(lm.cas1.step, plotit=T, lambda=seq(-2,2,by=0.5))
boxcox(lm.cas1.step,plotit=F) #values
max(boxcox(lm.cas1.step, plotit = F)$y)
boxcox(lm.cas1.step, plotit = F)$x[which.max(boxcox(lm.cas1.step, plotit = F)$y)] 
# the best lambda for transformation is -0.5

#The best lambda
L<-boxcox(lm.cas1.step, plotit = F)$x[which.max(boxcox(lm.cas1.step, plotit = F)$y)] 
L
#The best lambda
lm.cas1.step.boxcox<-lm((Casualties)^L ~  HwyRail + TRNSPD + HwyRail:TRNSPD, data=cdmgnd)

lm.cas1.step.boxcox<-lm((Casualties)^L ~ TRNSPD + CARS + TEMP + Visiblty + TRNSPD:TEMP + 
                          TRNSPD:Visiblty + CARS:Visiblty, data=cdmgnd)

lm.cas1.step.boxcox<-lm((ACCDMG+1)^L ~ Engrs + Conductr + Brakemen + I(Enghr + Cdtrhr) + Engrs:Conductr + Engrs:Brakemen + Brakemen:I(Enghr + Cdtrhr), data=totacts)
summary(lm.cas1.step.boxcox)

lm.cas1.step.boxcox<-lm((ACCDMG+1)^L ~ Engrs + Conductr + Brakemen + I(Enghr + 
                                                                         Cdtrhr) + Engrs:Conductr + Engrs:Brakemen + Conductr:I(Enghr + 
                                                                                                                                  Cdtrhr) + Brakemen:I(Enghr + Cdtrhr), data=xdmgnd)
summary(lm.cas1.step.boxcox)                                                                                                                                 



lm.cas1.step.boxcox<-lm((ACCDMG)^L ~ Derail + TRNSPD + CARS + TONS + Derail:TRNSPD + 
                          Derail:TONS + TRNSPD:TONS + CARS:TONS, data=xdmgnd)



lm.cas1.step.boxcox<-lm((ACCDMG)^L ~Derail + Region + TRNSPD + TONS + HEADEND1 + 
  Derail:Region + Derail:TRNSPD + Derail:TONS + Derail:HEADEND1 + 
  Region:TRNSPD + Region:TONS + Region:HEADEND1 + TRNSPD:TONS + 
  TONS:HEADEND1, data=xdmgnd)

summary(lm.cas1.step)
summary(lm.cas1.step.boxcox)


#Diagnostics plot
par(mfrow=c(2,2))
plot(lm.cas1.step.boxcox, labels.id = NULL)
#plot(lm.cas1.step)
par(mfrow=c(1,1)) 


lm.cas1.step.log <- lm(logCslt ~ TRNSPD + CARS + HEADEND1 + Visiblty + TRNSPD:HEADEND1 + 
                          TRNSPD:Visiblty + CARS:Visiblty, data=cdmgnd)

plot((cdmgnd$TRNSPD+1)^0.5, (cdmgnd$Casualties)^0.5)

plot((cdmgnd$CARS)^0.5, (cdmgnd$Casualties)^0.5)

plot((cdmgnd$Visiblty), (cdmgnd$Casualties)^0.5)



nrow(cdmgnd[cdmgnd$TRNSPD == 0,])
nrow(totacts[cdmgnd$TRNSPD == 0,])
nrow(totacts[xdmgnd$TRNSPD == 0,])
nrow(cdmgnd)
nrow(totacts)
check <- cdmgnd[cdmgnd$TRNSPD == 0,]






### cross validation

# You need to use glm (a funciton to estimate generalized linear model) instead of lm. Don't be confused by generalized linear models. 
# Because lm is a special case of glm, glm function can be used to estimate lm models as long as you set parameters correctly. 
lm.cas1.step.cv<-glm(Casualties ~ HwyRail + CARS + TEMP + TRNSPD + HwyRail:CARS + 
                       HwyRail:TEMP + HwyRail:TRNSPD + CARS:TRNSPD + TEMP:TRNSPD,
                   data=cdmgnd)
# Cross-validation:
lm.cas1.step.cv.err<-cv.glm(cdmgnd, lm.cas1.step.cv, K=10)
lm.cas1.step.cv.err$delta # we only need first one (the raw cross-validation estimate of prediction error)


lm.cas1.step.boxcox.cv<-glm((Casualties)^L ~ HwyRail + CARS + TEMP + TRNSPD + HwyRail:CARS + 
                              HwyRail:TEMP + HwyRail:TRNSPD + CARS:TRNSPD + TEMP:TRNSPD,
                     data=cdmgnd)
# Cross-validation:
lm.cas1.step.boxcox.cv.err<-cv.glm(cdmgnd, lm.cas1.step.cv, K=10)
lm.cas1.step.boxcox.cv.err$delta # we only need first one (the raw cross-validation estimate of prediction error)



# You need to use glm (a funciton to estimate generalized linear model) instead of lm. Don't be confused by generalized linear models. 
# Because lm is a special case of glm, glm function can be used to estimate lm models as long as you set parameters correctly. 
lm.cas1.step.cv<-glm(ACCDMG ~ Derail + Region + TRNSPD + TONS + HEADEND1 + 
                       Derail:Region + Derail:TRNSPD + Derail:TONS + Derail:HEADEND1 + 
                       Region:TRNSPD + Region:TONS + Region:HEADEND1 + TRNSPD:TONS + 
                       TONS:HEADEND1,
                     data=xdmgnd)
# Cross-validation:
lm.cas1.step.cv.err<-cv.glm(xdmgnd, lm.cas1.step.cv, K=10)
lm.cas1.step.cv.err$delta # we only need first one (the raw cross-validation estimate of prediction error)


lm.cas1.step.boxcox.cv<-glm((ACCDMG)^L ~ Derail + Region + TRNSPD + TONS + HEADEND1 + 
                              Derail:Region + Derail:TRNSPD + Derail:TONS + Derail:HEADEND1 + 
                              Region:TRNSPD + Region:TONS + Region:HEADEND1 + TRNSPD:TONS + 
                              TONS:HEADEND1,
                            data=xdmgnd)
# Cross-validation:
lm.cas1.step.boxcox.cv.err<-cv.glm(xdmgnd, lm.cas1.step.cv, K=10)
lm.cas1.step.boxcox.cv.err$delta # we only need first one (the raw cross-validation estimate of prediction error)









## Model 1
test.size<-1/3
cdmgnd.data<-test.set(cdmgnd,test.size)
nrow(cdmgnd.data$test)
nrow(cdmgnd.data$train)

# before transformation
# Build model with train set:
lm.cas1.step.train <- lm(Casualties ~ HwyRail + TRNSPD + HwyRail:TRNSPD, cdmgnd.data$train)

# predict with lm models:
lm.cas1.step.pred <- predict(lm.cas1.step.train, newdata=cdmgnd.data$test) 

# compute PMSE:
pmse.lm.cas1.step <- mse(lm.cas1.step.pred, cdmgnd.data$test$Casualties)


# after transformation
# Build model with train set:
lm.cas1.step.boxcox.train <- lm(logCslt ~ HwyRail + TRNSPD + HwyRail:TRNSPD, cdmgnd.data$train)

# predict with lm models:
lm.cas1.step.boxcox.pred <- predict(lm.cas1.step.boxcox.train, newdata=cdmgnd.data$test) 

# compute PMSE:
pmse.lm.cas1.step.boxcox <- mse(10^(lm.cas1.step.boxcox.pred), cdmgnd.data$test$Casualties)




## Model 2
test.size<-1/3
xdmgnd.data<-test.set(xdmgnd,test.size)
nrow(xdmgnd.data$test)
nrow(xdmgnd.data$train)

# before transformation
# Build model with train set:
lm.cas1.step.train <- lm(ACCDMG ~ Derail + Region + TRNSPD + TONS + HEADEND1 + 
                                  Derail:Region + Derail:TRNSPD + Derail:TONS + Derail:HEADEND1 + 
                                  Region:TRNSPD + Region:TONS + Region:HEADEND1 + TRNSPD:TONS + 
                                  TONS:HEADEND1, xdmgnd.data$train)

# predict with lm models:
lm.cas1.step.pred <- predict(lm.cas1.step.train, newdata=xdmgnd.data$test) 

# compute PMSE:
pmse.lm.cas1.step <- mse(lm.cas1.step.pred, xdmgnd.data$test$ACCDMG)


# after transformation
# Build model with train set:
lm.cas1.step.boxcox.train <- lm((ACCDMG)^L ~ Derail + Region + TRNSPD + TONS + HEADEND1 + 
                                Derail:Region + Derail:TRNSPD + Derail:TONS + Derail:HEADEND1 + 
                                Region:TRNSPD + Region:TONS + Region:HEADEND1 + TRNSPD:TONS + 
                                TONS:HEADEND1, xdmgnd.data$train)

# predict with lm models:
lm.cas1.step.boxcox.pred <- predict(lm.cas1.step.boxcox.train, newdata=xdmgnd.data$test) 

# compute PMSE:
pmse.lm.cas1.step.boxcox <- mse((lm.cas1.step.boxcox.pred)^(1/L), xdmgnd.data$test$ACCDMG)












#***********************************************************
#
#			Extreme Points
#
#***********************************************************
# Get the values in the box plot
dmgbox <- boxplot(totacts$ACCDMG)

# extreme points
length(dmgbox$out)

# What proportion of accidents are extreme?
length(dmgbox$out)/nrow(totacts)

# Create a data frame with just the extreme ACCDMG accidents
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

#there are two accident where the accident is equal the max
which(totacts$ACCDMG == max(totacts$ACCDMG))

# Proportion of costs
sum(xdmg$ACCDMG) /sum(totacts$ACCDMG)

# Look at the graphs of these extreme accidents
hist(xdmg$ACCDMG)
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

# also plot number of accidents per year.
plot(1:15, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")

# Frequency of accident types
barplot(table(xdmg$TYPE)) 
#compare with the totacts plot
barplot(table(totacts$TYPE)) 

# Frequency of train types
barplot(table(xdmg$TYPEQ)) 

# Frequency of Causes
barplot(table(xdmg$Cause)) 


# For Casualties (TOTINJ + TOTKLD)
xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# Remove 9/11
which(xdmg$ACCDMG > 15e6)
xdmg <- xdmg[-164,]
xdmg$YEAR[which(xdmg$TOTKLD == max(xdmg$TOTKLD))]

# Remove duplicates
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# Reset rownames (observation #s) for sequential numbering
rownames(xdmgnd)
rownames(xdmgnd) <- NULL







#***********************************************************
#
#			Principal Components
#
#***********************************************************

# Principal Components with the Correlation Matrix for extreme data 2 (metrics)
# only quantitative data
xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View data in the first 2 PC
biplot(xdmgnd.pca)

# Remove outliers in component 2
xdmgnd.pca <- princomp(xdmgnd[-c(3249, 457, 1967, 5737),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View the first 2 PC without ouliers
biplot(xdmgnd.pca)

# Variance plot
screeplot(xdmgnd.pca, main = "Variance for PC of Metrics")

# Loadings
barplot(xdmgnd.pca$loadings[,1])
barplot(xdmgnd.pca$loadings[,2])

# Cumulative variance
cumplot(xdmgnd.pca, col = "blue")



# Cost severity metric
xdmgnd.pca <- princomp(xdmgnd[-c(5737),c("EQPDMG", "TRKDMG","ACCDMG")], cor = T)
biplot(xdmgnd.pca)
xdmgnd.pca
xdmgnd.pca$loadings[,1]
xdmgnd.pca$loadings[,2]


#***********************************************************
#
#		Possible predictors of damage	
#
#***********************************************************

# just possible choice, we need try others
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")])

# PCA

pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")], cor = T )










pairs(~  ACCDMG + TRNSPD + TONS + CARS + TIMEHR + TEMP, data = xdmgnd)
pairs(~  log10(ACCDMG) + TRNSPD + TONS + CARS + TIMEHR + TEMP, data = xdmgnd)

xyplot(log(ACCDMG)~ TIMEHR + TONS, main = "Damage vs. Killed Conditioned on Cause", xlab = "Total Killed", ylab = "Total Accident Damage", data = totacts)
biplot(pred.pca)