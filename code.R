#libraries
library(foreign)
library(FinTS)
library(forecast)
library(TSA)

#open data set
data(airmiles)
plot(airmiles)
airl=log(airmiles)
plot(airl)
airl.p=window(airl,end=c(2001,8)) #pre-intervention data set
plot(airl.p)
length(airl.p)

#determine the ARIMA model form for the pre-intervention data set
Acf(diff(airl.p))
pacf(diff(airl.p))
boxplot(airl~cycle(airl.p))

#build pre-intervention model and check diagnostics
air.m0=arima(airl.p,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
air.m0
tsdiag(air.m0)


#generate the predited value
after.pred=predict(air.m0,45)
plot(after.pred$pred)
airl.a=window(airl,start=c(2001,9))
d=airl.a-after.pred$pred
plot(d)
acf(d)
pacf(d)

#specify the data frame for xtransf in arimax function
s11=1*(seq(airl)==69) 
s11 

#fit the intervention model
air.m1=arimax(airl,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),xtransf=cbind(s11,s11), transfer=list(c(0,0),c(1,0)),method='ML')
air.m1
tsdiag(air.m1)

#graph and understand the model
plot(ts(s11*(-0.129)+filter(s11,filter=.8901,method='recursive',side=1)*(-0.2419),frequency=12,start=1996),ylab='9/11 Effects',type='h')
abline(h=0)

#add the outliers to improve the fit
Dec96=1*(seq(airmiles))==12
Jan97=1*(seq(airmiles))==13
Dec02=1*(seq(airmiles))==84
air.m2=arimax(airl,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),xtransf=cbind(s11,s11,Dec96,Jan97,Dec02),transfer=list(c(0,0),c(1,0),c(0,0),c(0,0),c(0,0)),method='ML')
air.m2
tsdiag(air.m2)