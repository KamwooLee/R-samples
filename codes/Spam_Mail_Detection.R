# Statistical Modeling
# Author: Kamwoo Lee
# Last modified: 11/11/2016



#*****************************
#
# Load the data & source files
#
#*****************************

##load the 'ham_ts.csv' and 'spam_ts.csv' into ham and spam variables repectively
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')

##use the ts() command to get a time series of ham amount using ham data from all years with the exception of the last 6 weeks.
ham.ts<-ts(ham$count[1:464])

##use the ts() command to get a time series of spam amount using spam data from all years, but disclude the final 2 weeks (this will be your test set)
spam.ts<-ts(spam$count)

##load the forecast library; first run command 'install.packages("forecast")'
library(forecast)



#*****************************
# Observations
#*****************************

plot(spam.ts)

plot(ham.ts)



#*****************************
# Trend
#*****************************

## Spam
time.spam<-c(1:(length(spam.ts)-7))
spam.trend<-lm(spam.ts[time.spam]~time.spam)
plot(spam.ts[time.spam], type = "l")
abline(spam.trend, col = "red")

par(mfrow=c(2,2))
plot(spam.trend, labels.id = NULL)
par(mfrow=c(1,1))


## Ham
time.ham<-c(1:(length(ham.ts)-7))
ham.trend<-lm(ham.ts[1:457]~time.ham)
plot(ham.ts[time.ham], type = "l")
abline(ham.trend, col = "red")



#*****************************
# seasonality
#*****************************

## Spam
# periodogram from spam.ts
pg.spam<-spec.pgram(spam.ts,spans=9,demean=T,log='no')
# Find the peak
max.omega.spam<-pg.spam$freq[which(pg.spam$spec==max(pg.spam$spec))]
# Where is the peak?
max.omega.spam
# What is the period?
1/max.omega.spam


## Ham
# periodogram for ham.ts
pg.ham<-spec.pgram(ham.ts,spans=9,demean=T,log='no')
# Find the peak
max.omega.ham<-pg.ham$freq[which(pg.ham$spec==max(pg.ham$spec))] 
# Where is the peak?
max.omega.ham
# What is the period (1/max.omega)?
1/max.omega.ham

ham7=lag(ham.ts[1:457],-7)
ham.ts.7 = ts.intersect(ham.ts, ham7=lag(ham.ts,-7), dframe=TRUE)
ham.season <- lm(ham.ts~ ham7, data=ham.ts.7)
summary(ham.season)

par(mfrow=c(2,2))
plot(ham.season, labels.id = NULL)
par(mfrow=c(1,1))





#*****************************
# AR, MA & ARIMA Models  
#***************************** 

## Spam
#get the residuals from the spam.trend model above and store in e.ts:
e.ts.spam<-ts(spam.trend$residuals)
#plot the residuals for the spam.trend model
plot(e.ts.spam, ylab = "Residuals from Trend Model")
#Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals spam.trend
par(mfrow=c(1,2))
acf(e.ts.spam, main="ACF of Residuals from spam.trend")
pacf(e.ts.spam,main="PACF of Residuals from spam.trend")
par(mfrow=c(1,1))

#auto.arima from 'forecast' library- Automatically find p, q, & d terms
spam.auto <- auto.arima(e.ts.spam, trace=TRUE)
summary(spam.auto)
AIC(spam.auto)
tsdiag(spam.auto,gof.lag=20)

#arma(3,1) p=3, q=1
spam.arma31 <- arima(e.ts.spam, order=c(3,0,1))
summary(spam.arma31)
AIC(spam.arma31)
tsdiag(spam.arma31,gof.lag=20)

#arma(1,1) p=1, q=1
spam.arma11 <- arima(e.ts.spam, order=c(1,0,1))
summary(spam.arma11)
AIC(spam.arma11)
tsdiag(spam.arma11,gof.lag=20)

#arima(2,1) p=2, q=1
spam.arma21 <- arima(e.ts.spam, order=c(2,0,1))
summary(spam.arma21)
AIC(spam.arma21)
tsdiag(spam.arma21,gof.lag=20)





## Ham
#get the residuals from the spam.trend model above and store in e.ts:
e.ts.ham <- ts(ham.season$residuals)
#plot the residuals for the ham.trend model
plot(e.ts.ham, ylab = "Residuals from Seasonality Model")
#Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals ham.seasonality
par(mfrow=c(1,2))
acf(e.ts.ham, main="ACF of Residuals from ham.season")
pacf(e.ts.ham,main="PACF of Residuals from ham.season")
par(mfrow=c(1,1))

#auto.arima from 'forecast' library- Automatically find p, q, & d terms
ham.auto <- auto.arima(e.ts.ham, trace=TRUE)
summary(ham.auto)
AIC(ham.auto)
tsdiag(ham.auto,gof.lag=20)

#arma(2,1) p=2, q=1
ham.arma21 <- arima(e.ts.ham, order=c(2,0,1))
summary(ham.arma21)
AIC(ham.arma21)
tsdiag(ham.arma21,gof.lag=20)

#arma(1,1) p=1, q=1
ham.arma11 <- arima(e.ts.ham, order=c(1,0,1))
summary(ham.arma11)
AIC(ham.arma11)
tsdiag(ham.arma11,gof.lag=20)

#arima(2,1,1) p=2, d=1, q=1
ham.arima211 <- arima(e.ts.ham, order=c(2,1,1))
summary(ham.arima211)
AIC(ham.arima211)
tsdiag(ham.arima211,gof.lag=20)





#*****************************
# Prediction performance
#***************************** 

## Spam
#the next week or the test period in days
next.week.time<-c((length(spam.ts)-6):(length(spam.ts)))
#the test data frame
next.week<-data.frame(time.spam = next.week.time, count = spam.ts[next.week.time])
#the actual time series for the test period
next.week.ts <- spam.ts[next.week.time]
#Prediction for the next week
E_Y.pred<-predict(spam.trend,newdata=next.week)
e_t.pred<-forecast(spam.arma31,h=7) #spam.arma31
next.week.prediction<-E_Y.pred+e_t.pred$mean
mean((next.week.prediction-next.week$count)^2) #MSE

E_Y.pred<-predict(spam.trend,newdata=next.week)
e_t.pred<-forecast(spam.arma11,h=7) #spam.arma11
next.week.prediction<-E_Y.pred+e_t.pred$mean
mean((next.week.prediction-next.week$count)^2) #MSE

E_Y.pred<-predict(spam.trend,newdata=next.week)
e_t.pred<-forecast(spam.arma21,h=7) #spam.arma21
next.week.prediction<-E_Y.pred+e_t.pred$mean
mean((next.week.prediction-next.week$count)^2) #MSE



## Ham
#the next week or the test period in days
next.week.time<-c((length(ham7)-6):(length(ham7)))
#the test data frame
next.week<-ham.ts.7[next.week.time, ]
#the actual time series for the test period
next.week.ts <- ham.ts.7[next.week.time, ]
#Prediction for the next week
E_Y.pred<-predict(ham.season,newdata=next.week)
e_t.pred<-forecast(ham.arma21,h=7) #ham.arma21
next.week.prediction<-E_Y.pred+e_t.pred$mean
mean((next.week.prediction-next.week$ham.ts)^2) #MSE

E_Y.pred<-predict(ham.season,newdata=next.week)
e_t.pred<-forecast(ham.arma11,h=7) #ham.arma11
next.week.prediction<-E_Y.pred+e_t.pred$mean
mean((next.week.prediction-next.week$ham.ts)^2) #MSE

E_Y.pred<-predict(ham.season,newdata=next.week)
e_t.pred<-forecast(ham.arima211,h=7) #ham.arima211
next.week.prediction<-E_Y.pred+e_t.pred$mean
mean((next.week.prediction-next.week$ham.ts)^2) #MSE


