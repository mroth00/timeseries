library(quantmod)
library(fGarch)
library(fBasics)
# Read in the data

emc <- read.csv("C:/Users/Mansi/Desktop/statistics class SEM1/math 265/project/emc.csv")
emc=emc[-1006,]
tail(emc)
hpq <- read.csv("C:/Users/Mansi/Desktop/statistics class SEM1/math 265/project/hpq.csv")
tail(hpq)
hpq=hpq[-1006,]
qqq <- read.csv("C:/Users/Mansi/Desktop/statistics class SEM1/math 265/project/qqq.csv")
qqq=qqq[-1006,]
tail(qqq)

# Just get the log returns
emc_ln_rtn=log(emc$Daily_Return+1)
hpq_ln_rtn=log(hpq$Daily_Return+1)
qqq_ln_rtn=log(qqq$Daily_Return+1)

# Part (a)

basicStats(emc_ln_rtn)
basicStats(hpq_ln_rtn)
basicStats(qqq_ln_rtn)

# Part (b)

t.test(emc_ln_rtn)
t.test(hpq_ln_rtn)
t.test(qqq_ln_rtn)

# Part (c)

period=1005

s.emc <- skewness(emc_ln_rtn)/sqrt(6/period)
s.hpq <- skewness(hpq_ln_rtn)/sqrt(6/period)
s.qqq <- skewness(qqq_ln_rtn)/sqrt(6/period)

# skewness hypothesis tests
pv.emc <- 2*(1-pnorm(abs(s.emc)))
pv.hpq <- 2*(1-pnorm(abs(s.hpq)))
pv.qqq <- 2*(1-pnorm(abs(s.qqq)))

print(paste("EMC Skewness Statistic:",s.emc[1]))
print(paste("EMC p-value:", pv.emc))   

print(paste("HPQ Skewness Statistic:",s.hpq[1]))
print(paste("HPQ p-value:", pv.hpq))   

print(paste("QQQ Skewness Statistic:",s.qqq[1]))
print(paste("QQQ p-value:", pv.qqq))   


# Part (d)
k.emc <- kurtosis(emc_ln_rtn)
k.hpq <- kurtosis(hpq_ln_rtn)
k.qqq <- kurtosis(qqq_ln_rtn)

pv.emc <- 2*(1-pnorm(abs(k.emc)))
print(paste("EMC Kurtosis Statistic:",k.emc[1]))
print(paste("EMC p-value:",pv.emc))

pv.hpq <- 2*(1-pnorm(abs(k.hpq)))
print(paste("HPQ Kurtosis Statistic:",k.hpq[1]))
print(paste("HPQ p-value:",pv.hpq))

pv.qqq <- 2*(1-pnorm(abs(k.qqq)))
print(paste("QQQ Kurtosis Statistic:",k.qqq[1]))
print(paste("QQQ p-value:",pv.qqq))

# kurtosis hypothesis tests

# Part (e)

d1=density(emc_ln_rtn)
plot(d1$x,d1$y,xlab='returns',ylab='density',main='Log EMC Daily Returns',type='l')

d2=density(hpq_ln_rtn)
plot(d2$x,d2$y,xlab='returns',ylab='density',main='Log HPQ Daily Returns',type='l')

d3=density(qqq_ln_rtn)
plot(d3$x,d3$y,xlab='returns',ylab='density',main='Log QQQ Daily Returns',type='l')


##############################################################
#### ...random bits of code for next couple parts    #########x

#part (e) says to ' choose a distribution, guassian or t or..', 
#... i don't know if this makes sense or not, if we should expect normality, but 
# i'm taking this Jarque-Bera test from the blue Tsay book p. 10 and p.13
#then there are box-ljung tests and dickey fuller tests below too..
#turn to percentages
 
p_emc_ln_rtn<- emc_ln_rtn*100
p_hpq_ln_rtn <- hpq_ln_rtn*100
p_qqq_ln_rtn <- qqq_ln_rtn*100

#  the Jarque-bera normality test
normalTest(p_emc_ln_rtn, method='jb')
normalTest(p_hpq_ln_rtn, method='jb')
normalTest(p_qqq_ln_rtn, method='jb')

# Part (f)
# attempt at model estimations, first trying HPQ only

# 3 versons of the augmented dickey fuller test
# check for non-stationarity (existing unit roots)

library(fUnitRoots)

adfTest(emc_ln_rtn, lag=1, type='c')
adfTest(emc_ln_rtn, lag=1, type='nc')
adfTest(emc_ln_rtn, lag=1, type='ct')

adfTest(hpq_ln_rtn, lag=1, type='c')
adfTest(hpq_ln_rtn, lag=1, type='nc')
adfTest(hpq_ln_rtn, lag=1, type='ct')

adfTest(qqq_ln_rtn, lag=1, type='c')
adfTest(qqq_ln_rtn, lag=1, type='nc')
adfTest(qqq_ln_rtn, lag=1, type='ct')

# conclusion - no unit root - series has stationarity

# i was just trying to get things labeled by date here..almost worked..

#tried this AR(11) model, seems ok
ts.emc <-ts(emc_ln_rtn,frequency=365,start=c(2011,1))
plot(ts.emc, main="Log returns EMC", xlab="Time", ylab="Log Returns")
m1 <-arima(emc_ln_rtn,order=c(11,1,0),include.mean=FALSE)
tsdiag(m1)
m1

ts.hpq <- ts(hpq_ln_rtn,frequency=365,start=c(2011,1))
plot(ts.hpq, main="Log Returns HPQ", xlab= "Time", ylab="Log Returns")
m2 <- arima(hpq_ln_rtn, order= c(1,0,1), include.mean=FALSE)
tsdiag(m2)
m2

ts.qqq <-ts(qqq_ln_rtn,frequency=365,start=c(2011,1))
plot(ts.qqq, main="Log Returns QQQ", xlab="Time", ylab="Log Returns")
m3 <-arima(qqq_ln_rtn, order= c(2,0,1), include.mean=FALSE)
tsdiag(m3)

#EMC : ACF and PACF, Box- Ljung
emc.acf <- acf(emc_ln_rtn)
emc.pacf <-pacf(emc_ln_rtn)
emc.diff.pacf<- pacf(diff(emc_ln_rtn))

# HPQ :  ACF and PACF, Box-Ljung
hpq.acf <- acf(hpq_ln_rtn)
hpq.pacf <- pacf(hpq_ln_rtn)
hpq.diff.pacf <- pacf(diff(hpq_ln_rtn))

#QQQ : ACF and PACF, Box- Ljung
qqq.acf <- acf(qqq_ln_rtn)
qqq.pacf<- pacf(qqq_ln_rtn)
qqq.diff.pacf <- pacf(diff(qqq_ln_rtn))

# Perform Box-Pierce test for serial correlations
Box.test(emc_ln_rtn)
Box.test(hpq_ln_rtn) 
Box.test(qqq_ln_rtn)

# Perform Box-Ljung test for serial correlations.
Box.test(emc_ln_rtn,type='Ljung')
Box.test(hpq_ln_rtn,type='Ljung') 
Box.test(qqq_ln_rtn,type='Ljung')

# fail to reject Null Hyp... 

#g) forecasts

m1p=predict(m1,30)

lcl=m1p$pred-1.96*m1p$se
lcl
ucl=m1p$pred+1.96*m1p$se
ucl

m2p=predict(m2,30)
lcl=m2p$pred-1.96*m2p$se
lcl
ucl=m2p$pred+1.96*m2p$se
ucl

m3p=predict(m3,30)
lcl=m3p$pred-1.96*m3p$se
lcl
ucl=m3p$pred+1.96*m3p$se
ucl

#h)Determine ARCH effect in the log return series


Box.test(m1$residuals,lag=10,type='Ljung')
Box.test(m2$residuals,lag=10,type='Ljung')
Box.test(m3$residuals,lag=10,type='Ljung')


Box.test(m1$residuals^2,lag=10,type='Ljung')
Box.test(m2$residuals^2,lag=10,type='Ljung')
Box.test(m3$residuals^2,lag=10,type='Ljung')

#i) The parameters for garch needs to be decided, the model coefficients
#are not significant with garch(1,1), garch(1,0), garch(0,1) and for arch as well.
library(fGarch)
mm1=garchFit(~arma(1,1)+garch(1,1),data=(emc_ln_rtn),trace=F)
summary(mm1)

# No arch Effect for hpq dont bother

mm2=garchFit(~arma(1,1)+garch(1,1),data=(hpq_ln_rtn),trace=F)
summary(mm2)


mm3=garchFit(~arma(2,1)+garch(1,0),data=(qqq_ln_rtn),trace=F)
summary(mm3)

plot(mm1)
plot(mm2)
plot(mm3)

#j
mmm1=garchFit(~arma(1,1)+garch(1,1),data=(emc_ln_rtn),trace=F,cond.dist="std")
summary(mmm1)
plot(mmm1)

mmm2=garchFit(~arma(1,1)+garch(1,1),data=(hpq_ln_rtn),trace=F,cond.dist="std")
summary(mmm2)
plot(mmm2)

mmm3=garchFit(~arma(2,1)+garch(1,1),data=(qqq_ln_rtn),trace=F,cond.dist="std")
summary(mmm3)
plot(mmm3)

#k) 1 step ahead forecast with 95% interval forecasts
pm1=predict(mmm1,1)
pm1
pm2=predict(mmm2,1)
pm2
pm3=predict(mmm3,1)
pm3

#l) Determine cross correlations

par(mfcol=c(3,1))
plot(mm1,which=7)
mtext("EMC",side=3,cex=0.6)
plot(mm2,which=7)
mtext("HP",side=3,cex=0.6)
plot(mm3,which=7)
mtext("NASDAQ 100",side=3,cex=0.6)

par(mfcol=c(1,3))
plot(coredata(emc_ln_rtn),coredata(hpq_ln_rtn),xlab="EMC",ylab="HP",type="p",pch="16",lwd=2,col="blue",main="Scatterplot of EMC and HP")
abline(h=0,v=0)

plot(coredata(hpq_ln_rtn),coredata(qqq_ln_rtn),xlab="HP",ylab="NASDAQ 100",type="p",pch="16",lwd=2,col="blue",main="Scatterplot of HP and NASDAQ 100")
abline(h=0,v=0)

plot(coredata(emc_ln_rtn),coredata(qqq_ln_rtn),xlab="EMC",ylab="NASDAQ 100",type="p",pch="16",lwd=2,col="blue",main="Scatterplot of EMC and NASDAQ 100")
abline(h=0,v=0)

#m) Using a 30 day moving average and plotting covariances and correlations 

library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(rmgarch)
options(digits=4)

# plot returns
plot(emc_ln_rtn)
plot(hpq_ln_rtn)
plot(qqq_ln_rtn)

# scatterplot of returns
plot( coredata(emc_ln_rtn), coredata(hpq_ln_rtn), xlab="EMC", ylab="HPQ", 
      type="p", pch=16, lwd=2, col="blue")
abline(h=0,v=0)
plot(coredata(hpq_ln_rtn), coredata(qqq_ln_rtn), xlab="HP", ylab="NASDAQ 100",
     type="p", pch=16, lwd=2, col="blue")
abline(h=0,v=0)
plot(coredata(emc_ln_rtn), coredata(qqq_ln_rtn), xlab="EMC", ylab="NASDAQ 100",
     type="p", pch=16, lwd=2, col="blue")
abline(h=0,v=0)

#part c (made changes in the code from th example but unable to run these codes)
# compute rolling correlations
#

symbol.vec = c("EMC", "HPQ","QQQ")
getSymbols(symbol.vec, from ="2011-01-04", to = "2014-12-24",src="yahoo")


# extract adjusted closing prices
EMC = EMC[, "EMC.Adjusted", drop=F]
HPQ = HPQ[, "HPQ.Adjusted", drop=F]
QQQ = QQQ[, "QQQ.Adjusted", drop=F]

EMC.ret = CalculateReturns(EMC, method="log")
HPQ.ret = CalculateReturns(HPQ, method="log")
QQQ.ret = CalculateReturns(QQQ, method="log")

EMC.ret = EMC.ret[-1,]
HPQ.ret = HPQ.ret[-1,]
QQQ.ret = QQQ.ret[-1,]
colnames(EMC.ret) ="EMC"
colnames(HPQ.ret) = "HPQ"
colnames(QQQ.ret) = "QQQ"


chart.RollingCorrelation(EMC.ret, HPQ.ret,width=30)
chart.RollingCorrelation(HPQ.ret, QQQ.ret,width=30)
chart.RollingCorrelation(EMC.ret, QQQ.ret,width=30)

# create combined data series
EMC.HPQ.ret = merge(EMC.ret,HPQ.ret)


cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(EMC.HPQ.ret), FUN=cov.fun, width=30,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(EMC.HPQ.ret), FUN=cor.fun, width=20,
                     by.column=FALSE, align="right")

par(mfrow=c(2,1))
plot(roll.cov, main="20-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(EMC.HPQ.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="20-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(EMC.HPQ.ret)[1,2], lwd=2, col="red")

