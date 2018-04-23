##ISYE6402 Project
library(data.table)
library(zoo)
library(vars)

# Load data
data <- read.csv("data/combined_crypto_daily_data new.csv",header=TRUE, stringsAsFactors = FALSE)
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


# ACF
close <- cbind(bitcoin[,c(1,5)],ethereum[,5],ripple[,5], litecoin[,5], neo[,5], vix[,5], 
               gold[,2], oil[,2], dji[,5], nasdaq[,5], usepu[,2]) 
colnames(close) <- c("date", "bitcoin_close", "ethereum_close", "ripple_close", "litecoin_close", 
                     "neo_close", "vix_close", "gold", "oil", "dji_close", "nasdaq_close", "usepu")

# Create dataframe "close"
#close <- cbind(bitcoin[,c(1,5)],ethereum[,5],ripple[,5], litecoin[,5], neo[,5]) 
#colnames(close) <- c("date", "bitcoin_close", "ethereum_close", "ripple_close", "litecoin_close", "neo_close")
close <- close[close$date >= start_date,]
sapply(close,class)

# Dynamically Split training and test set
train_test_split <- function(data, train_start_date, train_end_date, forecast_days, currency_index) {
  index_train_start = which(data$date == train_start_date)
  index_train_end = which(data$date == train_end_date)
  currency_train = data[index_train_start:index_train_end, -1]
  currency_test = data[(index_train_end + 1) : (index_train_end + forecast_days), currency_index]
  return(list(currency_train, currency_test))
}

# ARIMA model fitting
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

# forecast function
forecast_fun_var <- function(data, model, nvals, test) {
  l = length(data)
  
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

# start date for all analyses, and moving training windows
start_date = as.Date("2017-06-01")
# start date vector for train
train_start_date_windows = seq(start_date, max(data$date) - 31, "days")
# end date vector for train
train_end_date_windows = train_start_date_windows + 29
# forecast window, 1-step prediction
forecast_windows = c(1)


### VAR Model ###
df_perf = data.frame(matrix(nrow = length(train_start_date_windows)*
                              length(forecast_windows), ncol = 10, 0))
colnames(df_perf) = c("currency", "train_start", "num_forecast_vals", "mspe", "mae", "mape", "pm", 
                      "optimal_p", "forecast", "actual")

index = 1

for(i in 1:length(train_start_date_windows)) {
  for(j in 1:length(forecast_windows)) {
    k = 2
    c(train,test) := train_test_split(close, train_start_date_windows[i], 
                                      train_end_date_windows[i], forecast_windows[j], k)
    #print(diff(log(ts(train))))
    mul_fit <- VAR(diff(log(ts(train))), lag.max = 10, type="none")
    optimal_p <- mul_fit$p
    #print((mul_fit$p))
    c(forecast, max, min, npts, ub, lb) := forecast_fun_var(train, mul_fit, forecast_windows[j], test)
    
    c(mspe, mae, mape, pm) := perf_measure(forecast, test)
    
    df_perf[index, 1] = "bitcoin_close"
    df_perf[index, 2] = train_start_date_windows[i]
    df_perf[index, 3] = forecast_windows[j]
    df_perf[index, 4:7] = c(mspe, mae, mape, pm)
    df_perf[index, 8] = optimal_p
    df_perf[index, 9:10] = c(forecast,test)
    
    index = index + 1
    print(c(i, j))
  }
}

par(mfrow=c(1,1))
ymin <- min(min(df_perf[,9]),min(df_perf[,10]))
ymax <- max(max(df_perf[,9]),max(df_perf[,10]))+20
timevol <- time(df_perf[,10])
plot(df_perf[,10], type="l", ylim = c(ymin,ymax), xlab="Time", ylab = "Price", main="Bitcoin Price in USD")
#lines(timevol, df_perf[,9], lty=3, lwd=2, col="blue")
lines(timevol, df_perf[,9], col="red")





### ARIMAX ###

df_perf = data.frame(matrix(nrow = length(train_start_date_windows)*
                              length(forecast_windows), ncol = 10, 0))
colnames(df_perf) = c("currency", "train_start", "num_forecast_vals", "mspe", "mae", "mape", "pm", 
                      "optimal_p", "optimal_q", "optimal_d", "optimal_aic")

index = 1

for(i in 1:length(train_start_date_windows)) {
  for(j in 1:length(forecast_windows)) {
    k = 2
    c(train,test) := train_test_split(close, train_start_date_windows[i], 
                                      train_end_date_windows[i], forecast_windows[j], k)
    optimal_pdq = aic_optimal_order(train)
    arimax_fit <- model_fin = arima(train, order = c(optimal_pdq[1]$p,optimal_pdq[3]$d,optimal_pdq[2]$q), 
                                    method='ML',xreg = data.frame(train[,7:10]))
    optimal_p <- mul_fit$p
    #print((mul_fit$p))
    c(forecast, max, min, npts, ub, lb) := forecast_fun_arima(train, mul_fit, forecast_windows[j], test)
    
    c(mspe, mae, mape, pm) := perf_measure(forecast, test)
    
    df_perf[index, 1] = "bitcoin_close"
    df_perf[index, 2] = train_start_date_windows[i]
    df_perf[index, 3] = forecast_windows[j]
    df_perf[index, 4:7] = c(mspe, mae, mape, pm)
    df_perf[index, 8] = optimal_p
    df_perf[index, 9:10] = c(forecast,test)
    
    index = index + 1
    print(c(i, j))
  }
}

par(mfrow=c(1,1))
ymin <- min(min(df_perf[,9]),min(df_perf[,10]))
ymax <- max(max(df_perf[,9]),max(df_perf[,10]))+20
timevol <- time(df_perf[,10])
plot(df_perf[,10], type="l", ylim = c(ymin,ymax), xlab="Time", ylab = "Price", main="Bitcoin Price in USD")
#lines(timevol, df_perf[,9], lty=3, lwd=2, col="blue")
lines(timevol, df_perf[,9], col="red")








#----------------------------------------#
close_train = close[1:(nrow(close)-7),]
close_test = close[(nrow(close)-6):nrow(close),]
close_train_ts <- ts(close_train[,-1])

# Plot time series
plot(close_train_ts, plot.type="single", col = 1:ncol(close_train_ts))
legend("topleft", colnames(close_train_ts), col=1:ncol(close_train), lty=1, cex=1)

# ACF of log differenced time series
acf(close_train_ts)

close_ts_diff_log <- diff(log(close_train_ts))
plot(close_ts_diff_log, plot.type="single", col = 1:ncol(close_ts_diff_log))
legend("topleft", colnames(close_ts_diff_log), col=1:ncol(close), lty=1, cex=0.7)

acf(close_ts_diff_log)

library(vars)
mul_var_1 <- VAR(close_ts_diff_log, lag.max=20, type="none")
mul_var_2 <- VAR(close_ts_diff_log, lag.max=20, type="const")
mul_var_3 <- VAR(close_ts_diff_log, lag.max=20, type="trend")
mul_var_4 <- VAR(close_ts_diff_log, lag.max=20, type="both")
mul_var_1$p
mul_var_2$p
mul_var_3$p
mul_var_4$p

mul_fit <- VAR(close_ts_diff_log, lag.max=10, type="both")
mul_fit$p
restrict_mul_fit <- restrict(mul_fit)
arch.test(mul_fit)
normality.test(mul_fit)

Box.test(resid(mul_fit)[,1], lag =(6+1), type="Ljung-Box", fitdf=6)
Box.test(resid(mul_fit)[,2], lag =(6+1), type="Ljung-Box", fitdf=6)
Box.test(resid(mul_fit)[,3], lag =(6+1), type="Ljung-Box", fitdf=6)
Box.test(resid(mul_fit)[,4], lag =(6+1), type="Ljung-Box", fitdf=6)
Box.test(resid(mul_fit)[,5], lag =(6+1), type="Ljung-Box", fitdf=6)


# Prediction on Bitcoin
mul_pred_diff_log <- predict(mul_fit, n.ahead=7)
mul_pred_diff_log_1 <- mul_pred_diff_log[[1]]$bitcoin_close[,1]
mul_ubound_diff_log_1 <- mul_pred_diff_log[[1]]$bitcoin_close[,3]
mul_lbound_diff_log_1 <- mul_pred_diff_log[[1]]$bitcoin_close[,2]

# inverse of lag log difference
mul_pred_inv_1 <-  as.numeric(exp(diffinv(mul_pred_diff_log_1, lag=1, xi=log(close_train_ts[,1][409]))))
mul_pred_inv_1 <- mul_pred_inv_1[2:length((mul_pred_inv_1))]

mul_ubound_inv_1 <-  as.numeric(exp(diffinv(mul_ubound_diff_log_1, lag=1, xi=log(close_train_ts[,1][409]))))
mul_ubound_inv_1 <- mul_ubound_inv_1[2:length((mul_ubound_inv_1))]

mul_lbound_inv_1 <-  as.numeric(exp(diffinv(mul_lbound_diff_log_1, lag=1, xi=log(close_train_ts[,1][409]))))
mul_lbound_inv_1 <- mul_lbound_inv_1[2:length((mul_lbound_inv_1))]

mul_MSPE_1 <- mean((close_test[,2]-mul_pred_inv_1)^2)

## MAE (Mean Absolute Prediction Error)
mul_MAE_1 <- mean(abs(close_test[,2]-mul_pred_inv_1))

## MAPE (Mean Absolute Percentage Prediction Error)
mul_MAPE_1 <- mean(abs(close_test[,2]-mul_pred_inv_1)/close_test[,2])

## PM
mul_PM_1 <- sum((close_test[,2]-mul_pred_inv_1)^2)/sum((close_test[,2]-mean(close_test[,2]))^2)

# Plot original + prediction for product 1 using VAR
par(mfrow=c(1,1))

ymin <- min(close[,2])
ymax <- max(close[,2])+20
timevol <- time(close[,1])
plot(close[,2], type="l", ylim = c(ymin,ymax), xlab="Time", ylab = "Price", main="Bitcoin Price in USD")
points(timevol[(416-6):416], mul_pred_inv_1, col="red")
lines(timevol[(416-6):416], mul_ubound_inv_1, lty=3, lwd=2, col="blue")
lines(timevol[(416-6):416], mul_lbound_inv_1, lty=3, lwd=2, col="green")
legend("topleft", c("upper bound", "prediction", "lower bound"), col=c("blue","red","green"), lty=c(3,NA,3),pch=c(NA,1,NA), cex=.65)


