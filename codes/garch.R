setwd("C:/Users/Tianyi/Desktop/project")

library(aTSA)
library(quantmod)
library(tseries)
library(fGarch)
library(mgcv)
library(rugarch)
library(forecast)

#################################################################################
#function to find optimal arima order

aic_optimal_order <- function(time_series) {
  n = length(time_series)
  p = c(0:5)
  q = c(0:5)
  d = c(0:3)
  df_aic = data.frame(matrix(nrow = length(p)*length(q)*length(d), ncol = 4, 0))
  index = 1
  
  for(i in 1:length(p)){
    for(j in 1:length(q)){
      for(k in 1:length(d)){
        tryCatch(
          {
            modijk = arima(time_series, order = c(p[i],d[k],q[j]), method='ML')
            aic = modijk$aic - 2*(p[i]+q[j]+1) + 2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
          }, error = function(e) {
            aic = 999999
          }
        )
        df_aic[index, 1] = p[i]
        df_aic[index, 2] = q[j]
        df_aic[index, 3] = d[k]
        df_aic[index, 4] = aic
        index = index + 1
      }
    }  
  }
  
  colnames(df_aic) = c("p", "q", "d", "aic")
  plot(df_aic$aic,ylab="AIC values")
  row_min = df_aic[which.min(df_aic$aic),]
  return(row_min)
}

###########################################################################################

refine_garch_order <- function(time_series,a,b) {
final.bic = Inf
final.order = c(0,0)
for (p in 0:6) for (q in 0:6){
  tryCatch(
    {
      spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                        mean.model=list(armaOrder=c(a, b), include.mean=T),
                        distribution.model="std")    
      fit = ugarchfit(spec, time_series,solver="hybrid")
      current.bic = infocriteria(fit)[2]
    }, error = function(e) {
      current.bic = 999999
    }
  )
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(p, q)
  }
  
  }
return(c(final.order,final.bic))
}
###########################################################################################

refine_arma_order <- function(time_series,a,b) {
  
  final.bic = Inf
  final.order = c(0,0)
  for (p in 0:6) for (q in 0:6){
    
    tryCatch(
      {
        spec = ugarchspec(variance.model=list(garchOrder=c(a,b)),
                          mean.model=list(armaOrder=c(p, q), include.mean=T),
                          distribution.model="std")    
        fit = ugarchfit(spec, time_series,solver="hybrid")
        current.bic = infocriteria(fit)[2]
      }, error = function(e) {
        current.bic = 999999
      }
    )
    if (current.bic < final.bic) {
      final.bic = current.bic
      final.order = c(p, q)
    }
    
  }
  return(c(final.order,final.bic))
}
###########################################################################################
###########################################################################################

refine_garch_order_forcing <- function(time_series,a,b) {
  final.bic = Inf
  final.order = c(0,0)
  for (p in 1:6) for (q in 1:6){
    tryCatch(
      {
        spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                          mean.model=list(armaOrder=c(a, b), include.mean=T),
                          distribution.model="std")    
        fit = ugarchfit(spec, time_series,solver="hybrid")
        current.bic = infocriteria(fit)[2]
      }, error = function(e) {
        current.bic = 999999
      }
    )
    if (current.bic < final.bic) {
      final.bic = current.bic
      final.order = c(p, q)
    }
    
  }
  return(c(final.order,final.bic))
}
###########################################################################################


data = read.csv("garch_data.csv")

#prepare data

bitcoin = data[,2]
ethereum = data[,3]
litecoin = data[,4]
neo = data[,5]
ripple = data[,6]

ts.plot(bitcoin)
ts.plot(ethereum)
ts.plot(litecoin)
ts.plot(neo)
ts.plot(ripple)

# 305 --- 2017 - 11 - 01
# bitcoin

bitcoin.lr = diff(log(bitcoin))
n1 = length(bitcoin.lr)
bitcoin.window = bitcoin.lr[151:n1]
# n1 = 415
ts.plot(bitcoin.window)
n = length(bitcoin.window)
# n = 111
n_test = 265-15
bitcoin.train = bitcoin.window[-c(n_test:n)]
bitcoin.test = bitcoin.window[n_test:n]

Box.test(bitcoin.train,type="Ljung-Box") # test for white noise process

auto.arima(bitcoin.train) # 0 0 0

aic_optimal_order(bitcoin.train) # 0 0 0

my_series = bitcoin.train

arima_model = arima(my_series,order=c(0,0,0))

resids = resid(arima_model)

par(mfrow=c(2,1))
acf(resid)
pacf(resid)
par(mfrow=c(2,1))
acf(resid, main = "Residual")
acf(resid^2,main="Residual Square")

Box.test(resid,lag=1,type="Ljung-Box",fitdf=0)
Box.test(resid^2,lag=1,type="Ljung-Box",fitdf=0)

refine_garch_order(my_series,0,0) # 1 1

refine_arma_order(my_series,1,1) # 0 0 0 , 0 0

refine_garch_order_forcing(my_series,0,0) # 0 0, 1 1

refine_arma_order(my_series,1,1) # 2 4, 1 1

refine_garch_order_forcing(my_series,2,4) # 2 4, 2 1

refine_arma_order(my_series,2,1) # 4 5, 2 1

refine_garch_order_forcing(my_series,4,5) # 4 5, 2 1

spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0, 0), 
                                  include.mean=T), distribution.model="std")    

nfore = length(bitcoin.test)
fore.series = NULL
fore.sigma = NULL
for(f in 1: nfore){
  ## Fit models
  data = bitcoin.train
  if(f>2)
    data = c(data,bitcoin.test[1:(f-1)])  
  final.model.1 = ugarchfit(spec, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.1, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)
  fore.sigma = c(fore.sigma, fore@forecast$sigmaFor)
}

ymin = min(c(as.vector(bitcoin.test),fore.series))
ymax = max(c(as.vector(bitcoin.test),fore.series))
data.plot = bitcoin.test
names(data.plot)="Fore"
par(mfrow=c(2,1))
plot(bitcoin.test,type="l", ylim=c(ymin,ymax),main="Mean Prediction of ARMA-Garch Model")
points(fore.series,lwd= 2, col="blue")

ymin = min(bitcoin.test^2,fore.sigma^2)
ymax = max(bitcoin.test^2,fore.sigma^2)

plot(bitcoin.test^2,type="l", ylim=c(ymin,ymax), main="Volatility Prediction of ARMA-GARCH model")
points(fore.sigma^2,lwd=2, col="blue")


library(mgcv)

zt.sq.log = log(resids^2)
n = length(resids)
time.pts = c(1:n)
time.pts = (time.pts-min(time.pts))/(max(time.pts)-min(time.pts))
gam.var = gam(zt.sq.log~s(time.pts))
plot(zt.sq.log)
lines(fitted(gam.var))

loc.fit = loess(zt.sq.log ~ time.pts)
par(mfrow=c(1,1))
plot(zt.sq.log)
lines(fitted(gam.var),col="red")
lines(fitted(loc.fit),col="blue")


# ethereum

ethereum.lr = diff(log(ethereum))
ethereum.window = ethereum.lr[305:n]
ts.plot(ethereum.window)
ethereum.train = ethereum.window[-c(n_test:n)]
ethereum.test = ethereum.window[n_test:n]

auto.arima(ethereum.train) # 2 1 1

aic_optimal_order(ethereum.train) # 2 2 0

my_series = ethereum.train

arima_model = arima(my_series,order=c(2,0,2))

resid = resid(arima_model)

par(mfrow=c(2,1))
acf(resid)
pacf(resid)
par(mfrow=c(2,1))
acf(resid)
acf(resid^2,main="Residual Square")

Box.test(resid,lag=1,type="Ljung-Box",fitdf=0)
Box.test(resid^2,lag=1,type="Ljung-Box",fitdf=0)

refine_garch_order(my_series,2,2) # 2 0 2 0 1

refine_arma_order(my_series,0,1) # 1 0 , 0 1

refine_garch_order(my_series,1,0) # 1 0 , 0 1

spec = ugarchspec(variance.model=list(garchOrder=c(0,1)),
                    mean.model=list(armaOrder=c(1, 0), 
                                    include.mean=T), distribution.model="std")    

nfore = length(ethereum.test)
fore.series = NULL
fore.sigma = NULL
for(f in 1: nfore){
  ## Fit models
  data = ethereum.train
  if(f>2)
    data = c(data,ethereum.test[1:(f-1)])  
  final.model.1 = ugarchfit(spec, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.1, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)
  fore.sigma = c(fore.sigma, fore@forecast$sigmaFor)
}

ymin = min(c(as.vector(ethereum.test),fore.series))
ymax = max(c(as.vector(ethereum.test),fore.series))
data.plot = ethereum.test
names(data.plot)="Fore"
par(mfrow=c(1,1))
plot(ethereum.test,type="l", ylim=c(ymin,ymax))
points(fore.series,lwd= 2, col="blue")

ymin = min(ethereum.test^2,fore.sigma^2)
ymax = max(ethereum.test^2,fore.sigma^2)

plot(ethereum.test^2,type="l", ylim=c(ymin,ymax))
points(fore.sigma^2,lwd=2, col="blue")

#litecoin

litecoin.lr = diff(log(litecoin))
litecoin.window = litecoin.lr[151:n1]
ts.plot(litecoin.window)
litecoin.train = litecoin.window[-c(n_test:n)]
litecoin.test = litecoin.window[n_test:n]

auto.arima(litecoin.train) # 0 0 0

aic_optimal_order(litecoin.train) # 2 2 0

my_series = litecoin.train

arima_model = arima(my_series,order=c(2,0,2))

resid = resid(arima_model)

par(mfrow=c(2,1))
acf(resid)
pacf(resid)
par(mfrow=c(2,1))
acf(resid)
acf(resid^2,main="Residual Square")

Box.test(resid,lag=1,type="Ljung-Box",fitdf=0)
Box.test(resid^2,lag=1,type="Ljung-Box",fitdf=0)

refine_garch_order(my_series,3,3) # 2 2, 0 0

refine_arma_order(my_series,0,0) # 0 0  , 0 0

refine_garch_order(my_series,0,0) #0 0 , 0 1

refine_arma_order(my_series,0,1) # 0 0 , 0 1

spec = ugarchspec(variance.model=list(garchOrder=c(0,1)),
                  mean.model=list(armaOrder=c(0, 0), 
                                  include.mean=T), distribution.model="std")    

nfore = length(litecoin.test)
fore.series = NULL
fore.sigma = NULL
for(f in 1: nfore){
  ## Fit models
  data = litecoin.train
  if(f>2)
    data = c(data,litecoin.test[1:(f-1)])  
  final.model.1 = ugarchfit(spec, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.1, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)
  fore.sigma = c(fore.sigma, fore@forecast$sigmaFor)
}

ymin = min(c(as.vector(litecoin.test),fore.series))
ymax = max(c(as.vector(litecoin.test),fore.series))
data.plot = litecoin.test
names(data.plot)="Fore"
par(mfrow=c(1,1))
plot(litecoin.test,type="l", ylim=c(ymin,ymax))
points(fore.series,lwd= 2, col="blue")

ymin = min(litecoin.test^2,fore.sigma^2)
ymax = max(litecoin.test^2,fore.sigma^2)

plot(litecoin.test^2,type="l", ylim=c(ymin,ymax))
points(fore.sigma^2,lwd=2, col="blue")

#neo

neo.lr = diff(log(neo))
neo.window = neo.lr[305:n1]
ts.plot(neo.window)
neo.train = neo.window[-c(n_test:n)]
neo.test = neo.window[n_test:n]

auto.arima(neo.train) # 0 0 0

aic_optimal_order(neo.train) # 3 3 0

my_series = neo.train

arima_model = arima(my_series,order=c(3,0,3))

resid = resid(arima_model)

par(mfrow=c(2,1))
acf(resid)
pacf(resid)
par(mfrow=c(2,1))
acf(resid)
acf(resid^2,main="Residual Square")

Box.test(resid,lag=1,type="Ljung-Box",fitdf=0)
Box.test(resid^2,lag=1,type="Ljung-Box",fitdf=0)

refine_garch_order(my_series,3,3) # 3 3, 2 1

refine_arma_order(my_series,2,1) # 5 3  , 2 1

# 0 0 fails to converge

spec = ugarchspec(variance.model=list(garchOrder=c(2,1)),
                  mean.model=list(armaOrder=c(5, 3), 
                                  include.mean=T), distribution.model="std")    

nfore = length(neo.test)
fore.series = NULL
fore.sigma = NULL
for(f in 1: nfore){
  ## Fit models
  data = neo.train
  if(f>2)
    data = c(data,neo.test[1:(f-1)])  
  final.model.1 = ugarchfit(spec, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.1, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)
  fore.sigma = c(fore.sigma, fore@forecast$sigmaFor)
}

ymin = min(c(as.vector(neo.test),fore.series))
ymax = max(c(as.vector(neo.test),fore.series))
data.plot = neo.test
names(data.plot)="Fore"
par(mfrow=c(1,1))
plot(neo.test,type="l", ylim=c(ymin,ymax))
points(fore.series,lwd= 2, col="blue")

ymin = min(neo.test^2,fore.sigma^2)
ymax = max(neo.test^2,fore.sigma^2)

plot(neo.test^2,type="l", ylim=c(ymin,ymax))
points(fore.sigma^2,lwd=2, col="blue")

#ripple

ripple.lr = diff(log(ripple))
ripple.window = ripple.lr[305:n1]
ts.plot(ripple.window)
ripple.train = ripple.window[-c(n_test:n)]
ripple.test = ripple.window[n_test:n]

auto.arima(ripple.train) # 0 0 0

aic_optimal_order(ripple.train) # 0 0 0

my_series = ripple.train

arima_model = arima(my_series,order=c(0,0,0))

resid = resid(arima_model)

par(mfrow=c(2,1))
acf(resid)
pacf(resid)
par(mfrow=c(2,1))
acf(resid)
acf(resid^2,main="Residual Square")

Box.test(resid,lag=1,type="Ljung-Box",fitdf=0)
Box.test(resid^2,lag=1,type="Ljung-Box",fitdf=0)

refine_garch_order(my_series,0,0) # 0 0, 0 1

refine_arma_order(my_series,0,1) # 0 0 , 2 1

spec = ugarchspec(variance.model=list(garchOrder=c(0,1)),
                  mean.model=list(armaOrder=c(0, 0), 
                                  include.mean=T), distribution.model="std")    

nfore = length(ripple.test)
fore.series = NULL
fore.sigma = NULL
for(f in 1: nfore){
  ## Fit models
  data = ripple.train
  if(f>2)
    data = c(data,ripple.test[1:(f-1)])  
  final.model.1 = ugarchfit(spec, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.1, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)
  fore.sigma = c(fore.sigma, fore@forecast$sigmaFor)
}

ymin = min(c(as.vector(ripple.test),fore.series))
ymax = max(c(as.vector(ripple.test),fore.series))
data.plot = ripple.test
names(data.plot)="Fore"
par(mfrow=c(1,1))
plot(ripple.test,type="l", ylim=c(ymin,ymax))
points(fore.series,lwd= 2, col="blue")

ymin = min(ripple.test^2,fore.sigma^2)
ymax = max(ripple.test^2,fore.sigma^2)

plot(ripple.test^2,type="l", ylim=c(ymin,ymax))
points(fore.sigma^2,lwd=2, col="blue")

###################################################################################
#GAM for residual

library(mgcv)

final.model = auto.arima(bitcoin.window)

aic_optimal_order(bitcoin.window)

resids = resid(final.model)
zt.sq.log = log(resids^2)
n = length(resids)
time.pts = c(1:n)
time.pts = (time.pts-min(time.pts))/(max(time.pts)-min(time.pts))
gam.var = gam(zt.sq.log~s(time.pts))
plot(zt.sq.log)
lines(fitted(gam.var))

loc.fit = loess(zt.sq.log ~ time.pts)

plot(zt.sq.log)
lines(fitted(gam.var),col="red")
lines(fitted(loc.fit),col="blue")

refine_garch_order(zt.sq.log,0,0)
refine_arma_order(zt.sq.log,0,0)
refine_garch_order(zt.sq.log,4,5)

spec.1 = ugarchspec(variance.model=list(garchOrder=c(0,0)),
                    mean.model=list(armaOrder=c(4,5), 
                    include.mean=T), distribution.model="std")    
model.1 = ugarchfit(spec.1,zt.sq.log)

plot(bitcoin.window^2)
par(mfrow=c(3,3))
s = 0
for (i in 1:234){
  print(i)
  nstart = 151+i
  nend = 181+i
  bitcoin.window = bitcoin.lr[nstart:nend]
  fit = auto.arima(bitcoin.window)
  resids = resid(fit)
  p = arimaorder(fit)[1]
  d = arimaorder(fit)[2]
  q = arimaorder(fit)[3]
  bt = Box.test(resids^2,lag=p+q+1,fitdf=p+q,type="Ljung-Box")
  pv = bt$p.value
  if(pv<0.05){
    acf(resids^2, main="ACF Plot of Squared Residual")
    model.1 = arima(bitcoin.window,order=c(p,d,q))
    #print(arch.test(model.1))
    #print(bt)
    #print(i)
    s = s+1
  }
}
