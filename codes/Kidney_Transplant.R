# Statistical Modeling
# Author: Kamwoo Lee
# Last modified: 12/6/2016



#***************************************************************
#***************************************************************
#
#  Designs to improve numbers of Treated Kidney Transplant Patients
#
#***************************************************************
#***************************************************************


#***************************************************************
#
#  Read in the data
#
#***************************************************************

uva <- read.table("UVAxplant.csv", sep = ",", header = T)

mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

duke <- read.table("Dukexplant.csv", sep = ",", header = T)

unc <- read.table("UNCxplant.csv", sep = ",", header = T)

r11donor<-read.table("R11donor.csv", sep = ",", header = T)

r11xplant<-read.table("R11xplant.csv", sep = ",", header = T)



#***************************************************************
#
#  Source files and libraries
#
#***************************************************************

# Source 
setwd(sourcedir)

source("TSbootfunctions.R")
source("Transplant.plots.R")
source("LMEfunctions.R")

library(boot)
library(forecast)
library(MASS)
library(lattice)
library(lme4) # you will need to install if you don't have it.


#********************************
#
#	Combining Data
#
#********************************

# Years

currentYear <- 2016

nYear <- length(uva$Year)

# Look at the differences in the DF

ncol(uva)
ncol(mcv)
ncol(duke)
ncol(unc)


names(uva)
names(mcv)
names(duke)
names(unc)

# Find commmon variables
comvar <- intersect((intersect(names(uva), names(mcv))),(intersect(names(uva), names(unc))))

# just take the xplants
comvar <- comvar[1:7]

# Create a unified DF
xplant <- rbind(uva[,comvar], mcv[,comvar], duke[,comvar], unc[,comvar])

summary(xplant)

dim(xplant)

# Create a school variable
SchoolNames <- c("UVA", "MCV", "Duke", "UNC")


# Add school to the DF
xplant$School <- c(rep("UVA", 29), rep("MCV", 29), rep("Duke", 29), rep("UNC", 29))

# make it a factor

xplant$School <- factor(xplant$School)

# Add number of years to the DF

xplant$nYears <- xplant$Year - 1987

# Check the new DF

summary(xplant)

# A cleaner DF without the partial year
# So, remove the partial year

xplantC <- xplant[-c(seq(29,116,29)),]

# Check this DF

summary(xplantC)

str(xplantC)


#***************************************************************
#
#  LME models for Kidney Transplants
#
#***************************************************************

# graphics

# What are your conclusions from these plots?

bwplot(Kidney~School, data = xplantC)

# Center plot

center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")


############################
# Simple model of varying intercepts by school
############################

xplant.lme1 <- lmer(Kidney~ 1 + (1|School) , data = xplantC, REML = T)

# What is the relationship between coefficients, fixed effects and random effects?

coef(xplant.lme1)

fixef(xplant.lme1)

ranef(xplant.lme1)

# plot of random effects

# What do you conclude?

dotplot(ranef(xplant.lme1,  condVar = T))

# Plot the intercepts

# Is this a good model?

center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")

sapply(1:4, function(i){
	abline(h = coef(xplant.lme1)$School[i,1])
	text(2016, coef(xplant.lme1)$School[i,1], levels(xplantC$School)[i])
})


# Diagnostics

# is this a good model?

plot(xplant.lme1) # residuals vs. fits

qqnorm(residuals(xplant.lme1))
qqline(residuals(xplant.lme1))

# Any serial correlation?

# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(xplant.lme1)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(xplant.lme1)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood
# Why?

xplant.lme1 <- lmer(Kidney~ 1 + (1|School) , data = xplantC, REML = F)

############################
# A model with number of years
# Varying intercept
############################

xplant.lme2 <- lmer(Kidney~ nYears + (1|School), data = xplantC, REML = T)

# get the coefficients, fixed & random effects

coef(xplant.lme2)

fixef(xplant.lme2)

ranef(xplant.lme2)

# plot of random effects
# what do you observe?

dotplot(ranef(xplant.lme2,  condVar = T))

# Plot the center intercepts with constant slope
# Good model?

center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")


sapply(1:4, function(i){
	abline(a = coef(xplant.lme2)$School[i,1] - 1988*coef(xplant.lme2)$School[i,2], b = coef(xplant.lme2)$School[i,2], col = c( "blue3" , "purple" , "lightblue3","orange")[i])
})

	

# Diagnostics
# Good model?

plot(xplant.lme2) # residuals vs. fits
f <- fitted(xplant.lme2)
r <- residuals(xplant.lme2)
plot(f,r)

qqnorm(residuals(xplant.lme2))
qqline(residuals(xplant.lme2))

# Serial correlation?

# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(xplant.lme2)[seq(1,26)+((x-1)*26)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(xplant.lme2)[seq(1,26)+((x-1)*26)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood

xplant.lme2 <- lmer(Kidney~ nYears + (1|School), data = xplantC, REML = F)


# Compare lme1 to lme2

anova(xplant.lme1, xplant.lme2) #notice no test = "Chi"

#########################
# Model with years
# varying intercepts and slopes
#########################

# xy plots

xyplot(Kidney~nYears|School, data = xplantC,type = c("p", "r"))

# the model with varying slopes and intercepts

xplant.lme3 <- lmer(Kidney~ nYears + (nYears|School), data = xplantC, REML = T)

# get the coefficients, fixed & random effects

coef(xplant.lme3)

fixef(xplant.lme3)

ranef(xplant.lme3)

# plot of random effects
# what do you observe?

dotplot(ranef(xplant.lme3,  condVar = T), scales = list(relation = "free"))

# Diagnostics
# Good model?

plot(xplant.lme3) # residuals vs. fits

qqnorm(residuals(xplant.lme3))
qqline(residuals(xplant.lme3))

# Plot the center intercepts with constant slope

center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")


sapply(1:4, function(i){
	abline(a = coef(xplant.lme3)$School[i,1] - 1988*coef(xplant.lme3)$School[i,2], b = coef(xplant.lme3)$School[i,2], col = c( "blue3" , "purple" , "lightblue3","orange")[i])
})


# Serial correlation?
# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(xplant.lme3)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(xplant.lme3)[seq(1,26)+((x-1)*26)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood

xplant.lme3 <- lmer(Kidney~ nYears + (nYears|School), data = xplantC, REML = F)

# Compare lme2 to lme3

anova(xplant.lme2, xplant.lme3) #notice no test = "Chi"

#########################
# Model with AR1
# varying intercepts and slopes
# for years
#########################

# Need a new DF with lag 1

xplantL1 <- data.frame(xplantC[-seq(1,4*28, by =28),], KidneyL1 = xplantC$Kidney[-seq(28, 4*28, by = 28)])

head(xplantL1[,c("Kidney", "KidneyL1")])

# Model with lag 1

xplant.lme5 <- lmer(Kidney~ nYears + KidneyL1+ (nYears|School), data = xplantL1, REML = F)

# get the coefficients, fixed & random effects

coef(xplant.lme5) 

# compare with xplant.lme3

fixef(xplant.lme5)

ranef(xplant.lme5)

# plot of random effects

dotplot(ranef(xplant.lme5,  condVar = T), scales = list(relation = "free"))

# Diagnostics
# Good model?

plot(xplant.lme5) # residuals vs. fits

qqnorm(residuals(xplant.lme5))
qqline(residuals(xplant.lme5))


# Serial correlation?
# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(xplant.lme5)[seq(1,27)+((x-1)*27)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(xplant.lme5)[seq(1,27)+((x-1)*27)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood

xplant.lme5 <- lmer(Kidney~ nYears + KidneyL1 +(nYears|School), data = xplantL1, REML = F)


# Compare lme3 to lme5

# need xplant.lme3 to use the same DF

xplant.lme3 <- lmer(Kidney~ nYears + (nYears|School), data = xplantL1, REML = F)


anova(xplant.lme3, xplant.lme5) #notice no test = "Chi"


#*****************************
#
#	Prediction
#
#*****************************


newkdata <- data.frame(Kidney =0, nYears = 29, KidneyL1 = xplantC$Kidney[28], School = "UVA")

################
# xplant.lme5

# prediction formulas

mm <- model.matrix(terms(xplant.lme5), newkdata)

xplant.lme5.pred <- mm %*% t(coef(xplant.lme5)[[1]][which(row.names(ranef(xplant.lme5)$School) == newkdata$School),])


#######
# Bootstrapping prediction and CI
# ignore warnings of convergence problems

kidboot.pred <- lmeboot("Kidney", xplantL1, formula(xplant.lme5), predict(xplant.lme5), residuals(xplant.lme5), newkdata, "School", "UVA", 200)

quantile(kidboot.pred, c(.025, .5, .975))

hist(kidboot.pred)
abline(v = median(kidboot.pred), col = "red", lwd = 2)


# Second method for bootstrapping
# the prediction and CI


# prediction function

mypredict <- function(.)
{
	mm <- model.matrix(terms(.), newkdata)
	mm %*% t(coef(.)[[1]][which(row.names(ranef(.)$School) == "UVA"),])
}

lme5.boot <- bootMer(xplant.lme5, mypredict, nsim =200)

lme5.boot.pred <- apply(lme5.boot$t,2, quantile, c(0.025, 0.5, 0.975))


# plot the prediction 
# with the TS

plot(xplantC$Year[1:28], xplantC$Kidney[1:28], type = "b", pch = 19, xlim = c(1988, 2017), ylab = "Number of Transplants", xlab = "Year", main = "Kidney Transplants at UVA")

# bootstrap 1 prediction

points(2016,quantile(kidboot.pred, c(.5)) , col = "red", pch = 19)

# CI

segments(2016, quantile(kidboot.pred, c(.5)), 2016, quantile(kidboot.pred, c(.975)), col = "red")
segments(2016, quantile(kidboot.pred, c(.5)), 2016, quantile(kidboot.pred, c(.025)), col = "red")


# bootstrap 2 prediction

points(2016.5, lme5.boot.pred[2] , col = "green", pch = 19)

# CI

segments(2016.5, lme5.boot.pred[2], 2016.5, lme5.boot.pred[1], col = "green")
segments(2016.5, lme5.boot.pred[2], 2016.5, lme5.boot.pred[3], col = "green")


# Current

points(2016, xplant$Kidney[29], col = "blue", pch = 19)


# Twice current

points(2016, 2*xplant$Kidney[29], col = "cyan", pch = 19)

# legend

legend(1990, 100, legend = c("Current", "2 X Current", "Bootstrap 1", "Bootstrap 2"), col = c("blue", "cyan", "red", "green"), pch = 19)

#***************************************************************
#
#  LME models for Liver Transplants
#
#***************************************************************

# graphics

bwplot(Liver~School, data = xplantC)


# Plot of centers


center.plot(cbind( uva$Liver[-nYear], unc$Liver[-nYear], mcv$Liver[-nYear], duke$Liver[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Liver")



############################
# Simple model of varying intercepts by school
############################


Lxplant.lme1 <- lmer(Liver~ 1 + (1|School) , data = xplantC, REML = T)

# Get coefficients, fixed and random effects

coef(Lxplant.lme1)

fixef(Lxplant.lme1)

ranef(Lxplant.lme1)

# plot of random effects

dotplot(ranef(Lxplant.lme1,  condVar = T))

# Diagnostics
# Good model?

plot(Lxplant.lme1) # residuals vs. fits

qqnorm(residuals(Lxplant.lme1))
qqline(residuals(Lxplant.lme1))

# Serial correlation?
# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(Lxplant.lme1)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF


par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(Lxplant.lme1)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood

Lxplant.lme1 <- lmer(Liver~ 1 + (1|School) , data = xplantC, REML = F)

############################
# A model with number of years
# Varying intercept
############################


Lxplant.lme2 <- lmer(Liver~ nYears + (1|School), data = xplantC, REML = T)

# Get coefficients, fixed and random effects

coef(Lxplant.lme2)

fixef(Lxplant.lme2)

ranef(Lxplant.lme2)

# plot of random effects

dotplot(ranef(Lxplant.lme2,  condVar = T))

# Diagnostics
# Good model?

plot(Lxplant.lme2) # residuals vs. fits

qqnorm(residuals(Lxplant.lme2))
qqline(residuals(Lxplant.lme2))

# Serial correlation?
# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(Lxplant.lme2)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF


par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(Lxplant.lme2)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood

Lxplant.lme2 <- lmer(Liver~ nYears + (1|School), data = xplantC, REML = F)


# Compare lme1 to lme2

anova(Lxplant.lme1, Lxplant.lme2) #notice no test = "Chi"

#########################
# Model with years
# varying intercepts and slopes
#########################

# xy plots

xyplot(Liver~nYears|School, data = xplantC,type = c("p", "r"))


# the model with varying slopes and intercepts

Lxplant.lme3 <- lmer(Liver~ nYears + (nYears|School), data = xplantC, REML = T)

# Get coefficients, fixed and random effects

coef(Lxplant.lme3)

fixef(Lxplant.lme3)

ranef(Lxplant.lme3)

# plot of random effects

dotplot(ranef(Lxplant.lme3,  condVar = T))

# Diagnostics
# Good model?

plot(Lxplant.lme3) # residuals vs. fits

qqnorm(residuals(Lxplant.lme3))
qqline(residuals(Lxplant.lme3))

# Serial correlation?
# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	acf(residuals(Lxplant.lme3)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
	pacf(residuals(Lxplant.lme3)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# Get the model with maximum likelihood

Lxplant.lme3 <- lmer(Liver~ nYears + (nYears|School), data = xplantC, REML = F)


# Compare lme2 to lme3

anova(Lxplant.lme2, Lxplant.lme3) #notice no test = "Chi"

#***************************************************************


#***************************************************************
#
#  Part 2:  Predicting the Number of Liver Transplants
#	 Poisson Regression
#
#***************************************************************
#***************************************************************

# Ref
# http://glmm.wikidot.com/faq
# http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/12284

#**************************************
#
#		Mixture Models with Poisson
#			Regression
#
#
#**************************************

# Mixture model with varying intercepts

Lxplant.lme1 <- glmer(Liver~ 1 + (1|School), data = xplantC, family = poisson)

# Get coefficients, fixed and random effects
coef(Lxplant.lme1)
ranef(Lxplant.lme1)
fixef(Lxplant.lme1)

# plot the random effects with SE

dotplot(ranef(Lxplant.lme1, condVar = T))

# Residual vs. fitted plot

plot(Lxplant.lme1)

## How does this model perform with regards to model diagnostics?

# Adding number of years as a predictor

Lxplant.lme2 <- glmer(Liver~ nYears + (1|School), data = xplantC, family = poisson)

# Get summary

summary(Lxplant.lme2)

# Get coefficients, fixed and random effects
coef(Lxplant.lme2)
ranef(Lxplant.lme2)
fixef(Lxplant.lme2)

# Dotplot of ranef

dotplot(ranef(Lxplant.lme2, condVar= T))

# Residual vs. fitted plot

plot(Lxplant.lme2)

# Serial correlation 

# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
  acf(residuals(Lxplant.lme2)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
  pacf(residuals(Lxplant.lme2)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

## Is there any serial correlation we need to model?

# Compare to Lxplant.lme1
anova(Lxplant.lme1, Lxplant.lme2) 

##  Which model do you choose between lme1 and lme2 and why?


##########################################
# Model with varying intercepts and slopes

Lxplant.lme3 <- glmer(Liver~ nYears + (nYears|School), data = xplantC, family = poisson)

coef(Lxplant.lme3)
ranef(Lxplant.lme3)
fixef(Lxplant.lme3)

# Compare to varying intercepts only

anova(Lxplant.lme2, Lxplant.lme3)

##  Which model do you choose between lme2 and lme3 and why?

#########################
# Model with AR1
# varying intercepts and slopes
# for years
#########################

# Need a new DF with lag 1

xplantL1 <- data.frame(xplantC[-seq(1,4*28, by =28),], LiverL1 = xplantC$Liver[-seq(28, 4*28, by = 28)])

head(xplantL1[,c("Liver", "LiverL1")])

# Model with lag 1 no nYears

Lxplant.lme5 <- glmer(Liver~ LiverL1+ (LiverL1|School), data = xplantL1, family = poisson)

# coefficients, random and fixed effects
coef(Lxplant.lme5)
ranef(Lxplant.lme5)
fixef(Lxplant.lme5)

# plot of random effects

dotplot(ranef(Lxplant.lme5,  condVar = T),scales = list(relation = "free"))

# Diagnostics

plot(Lxplant.lme5) # residuals vs. fits

qqnorm(residuals(Lxplant.lme5))
qqline(residuals(Lxplant.lme5))

## How does this model perform with regards to model diagnostics?

# Serial correlation?
# ACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
  acf(residuals(Lxplant.lme5)[seq(1,27)+((x-1)*27)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

# PACF

par(mfrow = c(2,2))
sapply(1:4, function(x){
  pacf(residuals(Lxplant.lme5)[seq(1,27)+((x-1)*27)], main = SchoolNames[x])
})
par(mfrow = c(1,1))

## Is there any serial correlation we need to model after including a lag of 1 in our model?

# Get the model with maximum likelihood

Lxplant.lme5 <- glmer(Liver~ LiverL1+ (LiverL1|School), data = xplantL1, family = poisson, REML = F)

# Test the model against just a varying intercepts model

Lxplant.lme4 <- glmer(Liver~ LiverL1+ (1|School), data = xplantL1, family = poisson)

anova(Lxplant.lme4,Lxplant.lme5)

## Which model do we choose between lme4 and lme5 and why?

#*****************************
#
#	Prediction
#
#*****************************

newLdata <- data.frame(Liver = rep(0, 4), LiverL1 = xplantC$Liver[seq(28,112,28)], School = c("UVA","MCV", "Duke", "UNC"))

predict(Lxplant.lme5, newdata = newLdata, type = "response")

#************************

# Overdispersion

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(Lxplant.lme5)


#************************************
#
#	Poisson Regression - GLM
#	Overdispersion with Quasipoisson
#
#************************************

# A basic poisson model

uva.liv.glm1  <- glm(Liver~. , data = xplantL1[which(xplantL1$School == "UVA"), c("Liver" ,"LiverL1")], family = poisson)

# Get a summary

summary(uva.liv.glm1)

# Model utility test

uva.liv.glm1.null <- glm(Liver~1 , data = xplantL1[which(xplantL1$School == "UVA"), c("Liver" ,"LiverL1")], family = poisson)
anova(uva.liv.glm1.null,uva.liv.glm1 ,test="Chi")


# Dispersion test

sum(resid(uva.liv.glm1, type = "pearson")^2/uva.liv.glm1$df.residual)


#################################
# Correcting for overdispersion

# Quasi-poisson model
# unpooled model

uva.liv.glm2  <- glm(Liver~. , data = xplantL1[which(xplantL1$School == "UVA"), c("Liver" ,"LiverL1")], family = quasipoisson)

# Get the summary

summary(uva.liv.glm2)

# Model utility test

uva.liv.glm.null  <- glm(Liver~1 , data = xplantL1[which(xplantL1$School == "UVA"), c("Liver" ,"LiverL1")], family = quasipoisson)

anova(uva.liv.glm.null, uva.liv.glm2, test = "Chi")

# Diagnostics

par(mfrow = c(2,2))
plot(uva.liv.glm2)
par(mfrow = c(1,1))

# Serial Corrlation?

uva.liv.res <- residuals(uva.liv.glm1, type = "pearson")

par(mfrow = c(1,2))
acf(uva.liv.res)
pacf(uva.liv.res)
par(mfrow = c(1,1))

# prediction 

newLdata <- data.frame(LiverL1 = xplantC[28, "Liver"], School = "UVA")


liv.pred <- predict(uva.liv.glm1, newdata = newLdata, type = "response", se.fit = T)

# prediction plot

plot(xplantC$Year[which(xplantC$School == "UVA")], xplantC$Liver[which(xplantC$School == "UVA")], type = "b", xlim = c(1988, 2017))

lines(xplantC$Year[which(xplantC$School == "MCV")], xplantC$Liver[which(xplantC$School == "MCV")], type = "b", col = "green")


lines(xplantC$Year[which(xplantC$School == "Duke")], xplantC$Liver[which(xplantC$School == "Duke")], type = "b", col = "red2")


lines(xplantC$Year[which(xplantC$School == "UNC")], xplantC$Liver[which(xplantC$School == "UNC")], type = "b", col = "blue2")


points(2016, liv.pred$fit, pch = 19)

segments(2016, liv.pred$fit, 2016, liv.pred$fit + 1.96*liv.pred$se.fit)

segments(2016, liv.pred$fit, 2016, liv.pred$fit - 1.96*liv.pred$se.fit)


# Pooled Model
uva.liv.glm4 <- glm(Liver~ LiverL1 + School , data = xplantL1, family = quasipoisson)

summary(uva.liv.glm4)
uva.liv.glm.null  <- glm(Liver~1 , data = xplantL1, family = quasipoisson)

anova(uva.liv.glm.null, uva.liv.glm4, test = "Chi")

# Diagnostics
par(mfrow = c(2,2))
plot(uva.liv.glm4)
par(mfrow = c(1,1))

# Serial correlation?

uva.liv.res <- residuals(uva.liv.glm4, type = "pearson")

par(mfrow = c(1,2))
acf(uva.liv.res)
pacf(uva.liv.res)
par(mfrow = c(1,1))



#*******************

#prediction

newLdata <- data.frame(nYears = 29, LiverL1 = xplantC[28, "Liver"], School = "UVA")

liv.pred <- predict(uva.liv.glm4, newdata = newLdata, type = "response", se.fit = T)

# Plot

plot(xplantC$Year[1:28], xplantC$Liver[1:28], type = "b", xlim = c(1988, 2017))

points(2016, liv.pred$fit, pch = 19)

segments(2016, liv.pred$fit, 2016, liv.pred$fit + 1.96*liv.pred$se.fit)

segments(2016, liv.pred$fit, 2016, liv.pred$fit - 1.96*liv.pred$se.fit)


#********************

# Compare pooled, unpooled and mixture model predictions



