setwd("~/Documents/2016_Fall_Class/SYS 6021_Statistical Modeling I/Projects/Final Project/")

# data preparation
primedu.data <- scan()

primedu.data <- matrix(primedu.data, byrow=T, ncol=16)
dimnames(primedu.data) <- list(country, year)


# infmr in 2015
infmr <- inftmr.data[complete.cases(inftmr.data), ]
hist(infmr)

# QQ plot
qqnorm(infmr, main="infmr") 
qqline(infmr)


# transform for symmetry
infmr.ll <- ll(sort(infmr), 9)
m <- median(infmr)
xx <- ((infmr.ll$u - m)^2 + (infmr.ll$l - m)^2)/(4*m)
yy <- (infmr.ll$u + infmr.ll$l)/2 - m
rr <- run.rrline(xx,yy)
plot(xx,yy)
abline(rr$a, rr$b, col='red')
# 1-p = slope, p = 0.8 close enough 1 -> log transformation


# log transformation
qqnorm(log(infmr), main="infmr") 
qqline(log(infmr))

par(mfrow = c(1,2))
qqnorm(infmr, main="Infant Mortality Ratio") 
qqline(infmr)
qqnorm(log(infmr), main="Log of IMR") 
qqline(log(infmr))
par(mfrow = c(1,1))


# median polish
inftmr.data.mp <- inftmr.data[complete.cases(inftmr.data), ]
inftmr.data.mp <- inftmr.data.mp[order(inftmr.data.mp[,16]), ]

rname.mp <- rownames(inftmr.data.mp)
cname.mp <- colnames(inftmr.data.mp)

mpol.infmr <- medpolish(inftmr.data.mp)
prmedpol(mpol.infmr, 2, rlab=rname.mp, clab=cname.mp)

n <- nrow(mpol.infmr$residuals)
m <- ncol(mpol.infmr$residuals)

plot(1, type="n", xlab="Column", ylab="Row", ylim=c(1, n), xlim=c(1,m), sub="blue=positive; red=negative", main="Infant mortality: Residuals (Row: Countries, Column: Year)")
for (i in 1:n)
  for (j in 1:m)
    points(j, i, pch = 8, col=ifelse(sign(mpol.infmr$residuals[i,j])==1, "blue", "red"), cex=0.5)


mpol.loginfmr <- medpolish(log(inftmr.data.mp))
prmedpol(mpol.loginfmr, 2, rlab=rname.mp, clab=cname.mp)


## multiple regression
# inftmr.data : infant mortality rate
# DPTimmun.data : DTP immunization
# primeedu.data : Survival rate to the last grade of primary education (female)
# imprvwater.data : imporoved water source (rural)
# pvtyhr.data : poverty headcount ratio at 1.90 a day
# prncare.data : pregnant women receiving prenatal care
# accelec.data : access to electricity
# attdskill.data : births attended by skilled health staff
# prevHIV.data : prevalence of HIV
# GINI.data : GINI index
# rrlpvtg.data : rural poverty gap


library(abind)
anal.dataset <- abind(inftmr.data, pvtyhr.data, rrlpvtg.data, primedu.data, imprvwater.data, attdskill.data, DPTimmun.data, prevHIV.data, along=3)
anal.dataset <- aperm(anal.dataset, c(1, 3, 2))
indicator <- c("InfMR", "PvtyR", "RuralGap", "PrimeEdu", "ImprvdWater", "BirthbySkill", "DTP", "HIV")
dimnames(anal.dataset) <- list(country, indicator, year)



## situation
inftmr.ts <- anal.dataset[,1,]
boxplot(inftmr.ts, main="Glabal Infant Mortality Rate", ylab = "number of deaths per 1,000 births", xlab = "year")





## analysis
inftmr.data




anal.df <- cbind(array(inftmr.data), array(pvtyhr.data), array(rrlpvtg.data), array(primedu.data), array(imprvwater.data), array(attdskill.data), array(DPTimmun.data), array(prevHIV.data))
anal.df <- data.frame(anal.df)
colnames(anal.df) <- c("InfMR", "PvtyR", "RuralGap", "PrimeEdu", "ImprvdWater", "BirthbySkill", "DTP", "HIV")
anal.df.rn <- anal.df[complete.cases(anal.df), ]
uva.pairs(anal.df)

lm1 <- lm(InfMR~., data=anal.df)
summary(lm1)
#Diagnostics plot
par(mfrow=c(2,2))
plot(lm1, labels.id = NULL)
#plot(lm.cas1)
par(mfrow=c(1,1)) 




lm2 <- lm(InfMR~(.)^2, data=anal.df)
summary(lm2)


lm2.step <- step(lm2) 
summary(lm2.step)


# AIC:
AIC(lm1)
AIC(lm2)
AIC(lm2.step)


# BIC:
AIC(lm1,k=log(nrow(anal.df)))
AIC(lm2,k=log(nrow(anal.df)))
AIC(lm2.step,k=log(nrow(anal.df)))





par(mfrow=c(2,2))
plot(lm2.step, labels.id = NULL)
par(mfrow=c(1,1)) 






# transformation
boxcox(lm1) #box-cox plot
boxcox(lm1, plotit=T, lambda=seq(-2,2,by=0.5))
boxcox(lm1,plotit=F) #values
max(boxcox(lm1, plotit = F)$y)
boxcox(lm1, plotit = F)$x[which.max(boxcox(lm1, plotit = F)$y)] 
# the best lambda for transformation is 0.3

lm2 <- lm(log(InfMR)~., data=anal.df)
summary(lm2)
#Diagnostics plot
par(mfrow=c(2,2))
plot(lm2, labels.id = NULL)
#plot(lm.cas1)
par(mfrow=c(1,1)) 








# QQ plot
par(mfrow = c(2,4))
qqnorm(inftmr.data, main="infmr") 
qqline(inftmr.data)
qqnorm(pvtyhr.data, main="pvtyhr") 
qqline(pvtyhr.data)
qqnorm(primedu.data, main="primeedu") 
qqline(primedu.data)
qqnorm(imprvwater.data, main="water") 
qqline(imprvwater.data)
qqnorm(GINI.data, main="GINI") 
qqline(GINI.data)
qqnorm(attdskill.data, main="attdskill") 
qqline(attdskill.data)
qqnorm(prevHIV.data, main="HIV") 
qqline(prevHIV.data)
qqnorm(DPTimmun.data, main="DTP") 
qqline(DPTimmun.data)
par(mfrow = c(1,1))



reg.data <- anal.dataset[complete.cases(anal.dataset[,,15]),,15]
uva.pairs(reg.data)
reg.data <- anal.dataset[,,16]
lm(reg.data[,1]~ reg.data[,2]+reg.data[,3]+reg.data[,4]+reg.data[,5]+reg.data[,6]+reg.data[,7])






library(ggplot2)


## time series analysis
AGO.ts <- data.frame(t(anal.dataset["AGO",,]))
CAF.ts <- data.frame(t(anal.dataset["CAF",,]))
TCD.ts <- data.frame(t(anal.dataset["TCD",,]))
MLI.ts <- data.frame(t(anal.dataset["MLI",,]))
NGA.ts <- data.frame(t(anal.dataset["NGA",,]))
SLE.ts <- data.frame(t(anal.dataset["SLE",,]))
SOM.ts <- data.frame(t(anal.dataset["SOM",,]))

par(mfrow = c(2,1))
acf(AGO.ts$InfMR)
pacf(AGO.ts$InfMR)
par(mfrow = c(1,1))

AGO.lm <- lm(InfMR ~ ., data=AGO.ts)
summary(AGO.lm)
plot(AGO.lm$residuals)

















# robust correlation
library(robust)
robust.correlation <- function(data, order=c(1,2,3))
{
  data <- aperm(data, order)
  size <- dim(data)
  dname <- dimnames(data)
  n = size[1]
  m = size[2]
  
  rcorr <- matrix(0, nrow = n, ncol = m, dimnames=list(dname[[1]], dname[[2]]))
  pcorr <- matrix(0, nrow = n, ncol = m, dimnames=list(dname[[1]], dname[[2]]))
  
  for(i in 1:n)
  {
    data.slice <- t(data[i,,])
    
    med <- colMedians(data.slice, na.rm=T)
    for(j in 1:m)
    {
      rcorr[i,j] <- median((data.slice[,1]-med[1])*(data.slice[,j]-med[j]),na.rm=T)/
                    ((median(abs(data.slice[,1]-med[1]),na.rm=T))*(median(abs(data.slice[,j]-med[j]),na.rm=T)))
      pcorr[i,j] <- cor(data.slice[,1],data.slice[,j], use="pairwise.complete.obs", method="pearson")
    }
  }
  
  result <- list()
  result$rcorr <- rcorr
  result$pcorr <- pcorr
  return(result)
  
}


## analysis by year
result <- robust.correlation(anal.dataset, order=c(3,2,1))
rcorr.byyear <- result$rcorr
round(colMedians(rcorr.byyear, na.rm=T),3)

# bootstrap country (analysis by year)
NB <- 200

bs.rcorr.byyear <- array(0,dim=c(length(year),ncol=length(indicator),NB), dimnames=list(year, indicator,c(1:NB)))
nn <- length(country)

for (i in 1:NB) {
  ii <- sample(c(1:nn),nn,replace=T)
  bs.dataset <- anal.dataset[ii,,]
  result <- robust.correlation(bs.dataset, order=c(3,2,1))
  bs.rcorr.byyear[,,i] <- result$rcorr
}

rcorr.byyear <- rowMeans(bs.rcorr.byyear, na.rm=T, dims=2)
round(rcorr.byyear, 3)
round(colMedians(rcorr.byyear, na.rm=T),3)
bs.se <- apply(bs.rcorr.byyear,c(1,2),sd, na.rm=T)/sqrt(nn)
round(rcorr.byyear - qt(0.95, df=nn-1) * bs.se, 2)
round(rcorr.byyear + qt(0.95, df=nn-1) * bs.se, 2)



## analysis by country
result <- robust.correlation(anal.dataset)
rcorr.bycountry <- result$rcorr

# bootstrap year (analysis by country)
NB <- 200

bs.rcorr.bycountry <- array(0,dim=c(length(country),ncol=length(indicator),NB), dimnames=list(country, indicator,c(1:NB)))
nn <- length(year)

for (i in 1:NB) {
  ii <- sample(c(1:nn),nn,replace=T)
  bs.dataset <- anal.dataset[,,ii]
  result <- robust.correlation(bs.dataset, order=c(1,2,3))
  bs.rcorr.bycountry[,,i] <- result$rcorr
}

rcorr.bycountry <- rowMeans(bs.rcorr.bycountry, na.rm=T, dims=2)
bs.se <- apply(bs.rcorr.bycountry,c(1,2),sd, na.rm=T)/sqrt(nn)
rcorr.bycountry - qt(0.95, df=nn-1) * bs.se
rcorr.bycountry + qt(0.95, df=nn-1) * bs.se






# boxplot
boxplot(abs(rcorr.byyear), ylim = c(0,2.2), main="Correlation Median Estimator")
boxplot(abs(rcorr.byyear), ylim = c(0,1.3), main="Correlation Median Estimator by Bootstrapping")

boxplot(abs(rcorr.bycountry))




# robust regression










## analysis by year
# 2011 data for example
reg.data <- cbind(inftmr.data[,12], pvtyhr.data[,12], DPTimmun.data[,12], imprvwater.data[,12], GINI.data[,12], attdskill.data[,12], prevHIV.data[,12], rrlpvtg.data[,12])
reg.data <- cbind(inftmr.data[,12], pvtyhr.data[,12], rrlpvtg.data[,12], primedu.data[,12], imprvwater.data[,12], attdskill.data[,12], DPTimmun.data[,12], prevHIV.data[,12])

# median 2011 - 2015
install.packages("matrixStats")
library(matrixStats)
reg.data <- cbind(rowMedians(inftmr.data[,12:16], na.rm=T), rowMedians(pvtyhr.data[,12:16], na.rm=T), rowMedians(DPTimmun.data[,12:16], na.rm=T), rowMedians(imprvwater.data[,12:16], na.rm=T), rowMedians(GINI.data[,12:16], na.rm=T), rowMedians(attdskill.data[,12:16], na.rm=T), rowMedians(prevHIV.data[,12:16], na.rm=T), rowMedians(rrlpvtg.data[,12:16], na.rm=T))
reg.data <- cbind(rowMedians(inftmr.data[,12:16], na.rm=T), rowMedians(pvtyhr.data[,12:16], na.rm=T), rowMedians(DPTimmun.data[,12:16], na.rm=T), rowMedians(imprvwater.data[,12:16], na.rm=T), rowMedians(GINI.data[,12:16], na.rm=T), rowMedians(attdskill.data[,12:16], na.rm=T), rowMedians(prevHIV.data[,12:16], na.rm=T), rowMedians(rrlpvtg.data[,12:16], na.rm=T))

colnames(reg.data) <- c("infm", "pvtr", "DPT", "imprwtr", "GINI", "attdskill", "HIV", "rrlgap")
rownames(reg.data) <- country
reg.data <- reg.data[complete.cases(reg.data), ]

source("SPM_Panel.R")
uva.pairs(reg.data[,])

uva.pairs(log(reg.data[,]))




save.image(file = "projectdata.RData")














rrline1 <- function(x,y) {
  n <- length(x); nmod3 <- n%%3;
  if (nmod3 == 0) n3 <- n/3;
  if (nmod3 == 1) n3 <- (n - 1)/3;
  if (nmod3 == 2) n3 <- (n + 1)/3;
  x.order <- order(x)
  medxL <- median(x[x.order][1:n3])
  medxR <- median(rev(x[x.order])[1:n3])
  medyL <- median(y[x.order][1:n3])
  medyR <- median(rev(y[x.order])[1:n3])
  slope1 <- (medyR - medyL)/(medxR - medxL)
  int1 <- median(y - slope1 * x)
  newy <- y - slope1 * x - int1
  sumres <- sum(abs(newy))
  list(a = int1, b = slope1, sumres = sumres, res = newy) 
}


run.rrline <- function (xx, yy, iter = 5) { 
  out.coef <- matrix(0, iter, 3)
  ll <- (1:length(xx))[!is.na(xx) & !is.na(yy)] 
  n <- length(ll); x <- xx[ll]; y <- yy[ll]; newy <- y
  for (i in 1:iter) {
    rr <- rrline1(x, newy)
    out.coef[i, ] <- c(rr$a, rr$b, rr$sumres) 
    newy <- rr$res 
  }
  dimnames(out.coef) <- list(format(1:iter),c("a","b","|res|")) 
  aa <- sum(out.coef[, 1]); bb <- sum(out.coef[, 2]);
  cc <- sum(abs(y - aa - bb * x))
  res <- yy - aa - bb * xx
  out.coef <- rbind(out.coef, c(aa, bb, cc)) 
  #print(round(out.coef, 5))
  list(a = aa, b = bb, res = res, coef = out.coef) 
}


prmedpol <- function (fit, digits = 3, rlab, clab) 
{ 
  if (missing(rlab))
    rlab <- format(1:length(fit$row))
  if (missing(clab))
    clab <- format(1:length(fit$col)) 
  prmatrix(round(rbind(cbind(fit$res, fit$row), c(fit$col, fit$overall)), digits), rowlab = c(rlab, "Col"), collab = c(clab, "Row")) 
  invisible() 
}


ll <- function(data, k) {
  n <- length(data)
  d <- rep(0, k); l <- rep(0, k); u <- rep(0, k); s <- rep(0, k)
  
  for (i in 1:k) {
    if (i == 1) d[i] <- (1+n)/2
    else d[i] <- (1+floor(d[i-1]))/2
    l[i] <- (data[floor(d[i])] + data[ceiling(d[i])])/2
    u[i] <- (data[floor(1+n-d[i])] + data[ceiling(1+n-d[i])])/2
    s[i] <- u[i] - l[i]
  }
  
  results <- list()
  results$d <- d
  results$l <- l
  results$u <- u
  results$s <- s
  return(results)
}



