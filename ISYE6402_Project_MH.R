##ISYE6402 Project VAR and ARIMAX 
library(data.table)
library(zoo)
library(vars)
library(aod)

# Load data
data <- read.csv("data/inputs/combined_crypto_daily_data_new.csv",header=TRUE, stringsAsFactors = FALSE)
data<- data[nrow(data):1,]
rownames(data) = 1:nrow(data)

# impute financial data for non-trading days
data[,2:106] <- sapply(data[,2:106], as.numeric)
data[,2:106] <- na.locf(data[,2:106])
data$date <- as.Date(data$date, format="%m/%d/%y")
data[,2:106] <- sapply(data[,2:106], as.numeric)
sapply(data,class)

# Subset data based on cryptocurrencies and external factors
bitcoin <- data[,c(1:7)]
ethereum <- data[,c(1,14:19)]
ripple <- data[,c(1,68:73)]
litecoin <- data[,c(1,26:31)]
neo <- data[,c(1,44:47)]
vix <- data[, c(1,86:89)]
gold <- data[, c(1,90:93)]
oil <- data[, c(1,94:95)]
dji <- data[, c(1,96:100)]
nasdaq <- data[, c(1,101:105)]
usepu <- data[, c(1,106)]


## Functions ##
# Split training and test set
train_test_split <- function(data, train_start_date, train_end_date, forecast_days, currency_index) {
  index_train_start = which(data$date == train_start_date)
  index_train_end = which(data$date == train_end_date)
  currency_train = data[index_train_start:index_train_end, -1]
  currency_test = data[(index_train_end + 1) : (index_train_end + forecast_days), currency_index]
  return(list(currency_train, currency_test))
}


# ARIMA find optimal orders
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
  # plot(df_aic$aic,ylab="AIC values")
  row_min = df_aic[which.min(df_aic$aic),]
  return(row_min)
}


# function for assignment 
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

# forecast function for var
forecast_fun_var <- function(data, model, nvals, test) {
 
  forecast_values = predict(model, n.ahead=nvals)
  forecast_diff_log = forecast_values[[1]]$bitcoin_close[,1]
  lower_bound_diff_log = forecast_values[[1]]$bitcoin_close[,2]
  upper_bound_diff_log = forecast_values[[1]]$bitcoin_close[,3]
  
  forecast =  as.numeric(exp(diffinv(forecast_diff_log, lag=1, xi=log(data[,1][30]))))
  forecast <- forecast[2:length((forecast))]
  lower =  as.numeric(exp(diffinv(lower_bound_diff_log, lag=1, xi=log(data[,1][30]))))
  lower <- lower[2:length((lower))]
  upper =  as.numeric(exp(diffinv(upper_bound_diff_log, lag=1, xi=log(data[,1][30]))))
  upper <- upper[2:length((upper))]
  
  npts = time(c(data, test))
  ymax = max(max(upper), max(data)) + 0.25
  ymin = min(min(lower), min(data)) - 0.25
  return(list(forecast, ymax, ymin, npts, upper, lower))
}

# forecast function for arima
forecast_fun_arima <- function(data, model, nvals, test, l) {
  l=l
  forecast_values = predict(model, n.ahead=nvals, newxreg = test[,l])
  upper_bound = forecast_values$pred + 1.96*abs(forecast_values$se)
  lower_bound = forecast_values$pred - 1.96*abs(forecast_values$se)
  npts = time(c(data, test))
  ymax = max(max(upper_bound), max(data)) + 0.25
  ymin = min(min(lower_bound), min(data)) - 0.25
  return(list(forecast_values$pred, ymax, ymin, npts, upper_bound, lower_bound))
}

# performance measurement
perf_measure <- function(forecast, test) {
  # MSPE 
  mspe = mean((forecast - test)^2)
  # MAE 
  mae = mean(abs(forecast - test))
  # MAPE 
  mape = mean(abs(forecast- test) / test)
  # Precision measure PM
  pm = sum((forecast - test)^2) / sum((test - mean(test))^2)
  return(c(mspe, mae, mape, pm))
}


## Main ##

# start date for all analyses, and moving training windows, window size = 30 days
start_date = as.Date("2017-06-01")
# start date vector for train
train_start_date_windows = seq(start_date, max(data$date) - 31, "days")
# end date vector for train
train_end_date_windows = train_start_date_windows + 29
# forecast window, 1-step prediction
forecast_windows = c(1)

# Subset input data to get close prices
close <- cbind(bitcoin[,c(1,5)],ethereum[,5],ripple[,5], litecoin[,5], neo[,5], vix[,5], 
               gold[,2], oil[,2], dji[,5], nasdaq[,5], usepu[,2]) 
colnames(close) <- c("date", "bitcoin_close", "ethereum_close", "ripple_close", "litecoin_close", 
                     "neo_close", "vix_close", "gold", "oil", "dji_close", "nasdaq_close", "usepu")

# Create dataframe "close"
#close <- cbind(bitcoin[,c(1,5)],ethereum[,5],ripple[,5], litecoin[,5], neo[,5]) 
#colnames(close) <- c("date", "bitcoin_close", "ethereum_close", "ripple_close", "litecoin_close", "neo_close")
close <- close[close$date >= start_date,]
sapply(close,class)


###---- VAR Model ----###
df_perf = data.frame(matrix(nrow = length(train_start_date_windows)*
                              length(forecast_windows), ncol = 12, 0))
colnames(df_perf) = c("currency", "train_start", "num_forecast_vals", "mspe", "mae", "mape", "pm", 
                      "optimal_p", "forecast", "actual", "upper_bound","lower_bound")

index = 1

for(i in 1:length(train_start_date_windows)) {
  for(j in 1:length(forecast_windows)) {
    k = 2
    c(train,test) := train_test_split(close, train_start_date_windows[i], 
                                      train_end_date_windows[i], forecast_windows[j], k)
    mul_fit <- VAR(diff(log(ts(train))), lag.max = 10, type="none")
    #mul_fit <- restrict(mul_fit)
    optimal_p <- mul_fit$p
    c(forecast, max, min, npts, ub, lb) := forecast_fun_var(train, mul_fit, forecast_windows[j], test)
    c(mspe, mae, mape, pm) := perf_measure(forecast, test)
    
    df_perf[index, 1] = "bitcoin_close"
    df_perf[index, 2] = train_start_date_windows[i]
    df_perf[index, 3] = forecast_windows[j]
    df_perf[index, 4:7] = c(mspe, mae, mape, pm)
    df_perf[index, 8] = optimal_p
    df_perf[index, 9:12] = c(forecast,test,ub,lb)
    
    index = index + 1
    print(c(i, j))
  }
}

par(mfrow=c(1,1))
ymin <- min(min(df_perf[,9]),min(df_perf[,10]))
ymax <- max(max(df_perf[,9]),max(df_perf[,10]))+20
timevol <- time(df_perf[,10])
plot(df_perf[,10], type="l", ylim = c(ymin,ymax), xlab="Time", ylab = "Price", main="Bitcoin Price in USD")
lines(timevol, df_perf[,9], col="red")

# Export result to csv
write.csv(file = "data/outputs/var_analysis.csv", df_perf, row.names = F)


###---- ARIMAX ----###
for (l in 6:11) {
  df_perf_arimax = data.frame(matrix(nrow = length(train_start_date_windows)*
                                       length(forecast_windows), ncol = 16, 0))
  
  colnames(df_perf_arimax) = c("currency", "train_start", "forecast_actual", "volatility", 
                               "actual_val", "num_forecast_vals", "mspe", "mae", "mape", "pm", 
                               "optimal_p", "optimal_q", "optimal_d", "optimal_aic", "up", "lb")
  
  index = 1  

  for(i in 1:length(train_start_date_windows)) {
    for(j in 1:length(forecast_windows)) {
      k = 2
      c(train,test) := train_test_split(close, train_start_date_windows[i], 
                                        train_end_date_windows[i], forecast_windows[j], c(k:12))
      optimal_pdq = aic_optimal_order(log(train$bitcoin_close))
      arimax_fit <- arima(log(train$bitcoin_close), order = c(optimal_pdq[1]$p,optimal_pdq[3]$d,optimal_pdq[2]$q), 
                                      method='ML', xreg = data.frame(log(train[,l])))
  
      c(forecast, max, min, npts, ub, lb) := forecast_fun_arima(log(train), arimax_fit, forecast_windows[j], log(test),l)
      
      c(mspe, mae, mape, pm) := perf_measure(exp(forecast), (test[,1]))
      
      df_perf_arimax[index, 1] = "bitcoin_close"
      df_perf_arimax[index, 2] = train_end_date_windows[i] + 1
      df_perf_arimax[index, 3] = exp(forecast[1])
      df_perf_arimax[index, 4] = (exp(ub) - exp(lb))[1]
      df_perf_arimax[index, 5] = (test[,1])
      df_perf_arimax[index, 6] = forecast_windows[j]
      df_perf_arimax[index, 7:10] = c(mspe, mae, mape, pm)
      df_perf_arimax[index, 11:14] = optimal_pdq
      df_perf_arimax[index, 15:16] = c(exp(ub),exp(lb))
  
      
      index = index + 1
      print(c(i, j))
    }
  }
  write.csv(df_perf_arimax, paste("data/outputs/arimax_analysis_", l, "_.csv"), row.names = F)
}

par(mfrow=c(1,1))
ymin <- min(min(df_perf_arimax[,3]),min(df_perf_arimax[,5]))
ymax <- max(max(df_perf_arimax[,3]),max(df_perf_arimax[,5]))+20
timevol <- time(df_perf_arimax[,3])
plot(df_perf_arimax[,5], type="l", ylim = c(ymin,ymax), xlab="Time", ylab = "Price", main="Bitcoin Price in USD")
lines(timevol, df_perf_arimax[,3], col="red")

# Export result to csv
write.csv(file = "data/outputs/arimax_analysis_new.csv", df_perf_arimax, row.names = F)


###---- VAR on the whole period starting from 06/01/17----###
close_ts <- ts(close[,-1])

# Plot time series
plot(close_ts, plot.type="single", col = 1:ncol(close_ts))
legend("topleft", colnames(close_ts), col=1:ncol(close_ts), lty=1, cex=1)

# ACF of log differenced time series
acf(close_ts)

close_ts_diff_log <- diff(log(close_ts))
plot(close_ts_diff_log, plot.type="single", col = 1:ncol(close_ts_diff_log))
legend("topleft", colnames(close_ts_diff_log), col=1:ncol(close_ts), lty=1, cex=0.7)

acf(close_ts_diff_log)

var_fit <- VAR(close_ts_diff_log, lag.max=20, type="both")
var_fit$p
restrict_var_fit <- restrict(var_fit)
sumvar <- summary(var_fit)
summary(restrict_var_fit)
arch.test(var_fit)
normality.test(var_fit)

## Granger Causality: Wald Test
coef.bitcoin <- coefficients(var_fit)$bitcoin_close
var.model = vcov(var_fit)[1:400,1:400]

Box.test(resid(var_fit)[,1], lag =(20+1), type="Ljung-Box", fitdf=20)
Box.test(resid(var_fit)[,2], lag =(20+1), type="Ljung-Box", fitdf=20)
Box.test(resid(var_fit)[,3], lag =(20+1), type="Ljung-Box", fitdf=20)
Box.test(resid(var_fit)[,4], lag =(20+1), type="Ljung-Box", fitdf=20)
Box.test(resid(var_fit)[,5], lag =(20+1), type="Ljung-Box", fitdf=20)

plot(resid(var_fit)[,1])
points(resid(var_fit)[,2],col="red")
points(resid(var_fit)[,3],col="green")
points(resid(var_fit)[,4],col="blue")
points(resid(var_fit)[,5],col="brown")
points(resid(var_fit)[,6],col="cyan")
