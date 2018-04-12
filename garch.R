setwd("C:/Users/Tianyi/Desktop/project")

data = read.csv("garch_data.csv")

#prepare data

bitcoin = data[,2]

bitcoin.log = diff(log(bitcoin))

n = length(bitcoin.log)
# define training window.

# 305 --- 2017-11-01

bitcoin.window = bitcoin.log[305:n]

ts.plot(bitcoin.window)

n = length(bitcoin.window)

bitcoin.train = bitcoin.window[-c(104:n)]

bitcoin.test = bitcoin.window[104:n]

Box.test(bitcoin.train,type="Ljung-Box")

#arch test
#library(aTSA)



#arch.test(final.arima)

#################################################################################
#function to find optimal arima order

aic_optimal_order <- function(time_series) {
  n = length(time_series)
  p = c(1:5)
  q = c(1:5)
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

library(forecast)

auto.arima(bitcoin.train)

aic_optimal_order(bitcoin.train)

# 0 0 1

final.arima = arima(bitcoin.train,order=c(0,0,0))

resid = resid(final.arima)

acf(resid)
acf(resid^2)

Box.test(resid,type="Ljung",fitdf=0,lag=1)
Box.test(resid^2,type="Ljung",fitdf=0,lag=1)

library(quantmod)
library(tseries)
library(fGarch)
library(mgcv)
library(rugarch)

######## GARCH ###########

final.bic = Inf
final.order = c(0,0)
for (p in 1:6) for (q in 1:6){
  print(c(p,q))
  spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                    mean.model=list(armaOrder=c(0, 0), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, bitcoin.train,solver="hybrid")
  current.bic = infocriteria(fit)[2]
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(p, q)
  }
}

# 1 1

# refine arma order

final.bic = Inf
final.order = c(0,0)
for (p in 0:4) for (q in 0:4){
  print(c(p,q))
  spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(p, q), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, bitcoin.train,solver="hybrid")
  current.bic = infocriteria(fit)[2]
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(p, q)
  }
}

# 2 4

# refine garch order

final.bic = Inf
final.order = c(0,0)
for (p in 1:6) for (q in 1:6){
  print(c(p,q))
  spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                    mean.model=list(armaOrder=c(2, 4), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, bitcoin.train,solver="hybrid")
  current.bic = infocriteria(fit)[2]
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(p, q)
  }
}

###### specify model

spec.2 = ugarchspec(variance.model=list(garchOrder=c(2,1)),
                    mean.model=list(armaOrder=c(5, 5), 
                                    include.mean=T), distribution.model="std")    
final.model.2 = ugarchfit(spec.2, bitcoin.train, solver = 'hybrid')


nfore = length(bitcoin.test)
fore.series.1 = NULL
for(f in 1: nfore){
  ## Fit models
  data = bitcoin.train
  if(f>2)
    data = c(bitcoin.train,bitcoin.test[1:(f-1)])  
  final.model.1 = ugarchfit(spec.2, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.1, n.ahead=1)
  fore.series.1 = c(fore.series.1, fore@forecast$seriesFor)
}

ymin = min(c(as.vector(bitcoin.test),fore.series.1))
ymax = max(c(as.vector(bitcoin.test),fore.series.1))
data.plot = bitcoin.test
names(data.plot)="Fore"
par(mfrow=c(1,1))
plot(bitcoin.test,type="l", ylim=c(ymin,ymax))
points(fore.series.1,lwd= 2, col="blue")




aic_optimal_order(bitcoin.ld)


