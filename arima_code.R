setwd("E:/isye_6402/project/cryptocurrencypricehistory")
library("lubridate")
library("date")
library("anytime")
library("dplyr")

######### defining constants for entire analysis ##############


# working with top n currencies
n = 5
# fraction of data for training 
ftrain = 0.997
# number of testing points - to determine how many days in advance we have to make predictions
ndays = 4
# if forecasting for last 7 days
take_ndays = 1

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


# function to get close price of top-5 currencies 
get_close_price <- function(data, currencies) {
  l = length(currencies)
  combined_data = data.frame(data$date)
  colnames(combined_data) = "date"
  for(i in 1:l) {
    temp = data[,grepl(currencies[i], names(data))]
    temp1 = data.frame(temp[,grepl("*Close", names(temp))])
    colnames(temp1) = currencies[i]
    combined_data = cbind.data.frame(combined_data, temp1)
  } 
  return(combined_data)
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
  plot(df_aic$aic,ylab="AIC values")
  row_min = df_aic[which.min(df_aic$aic),]
  return(row_min)
}


train_test_split <- function(data, train_start_date, train_end_date, forecast_days, currency_index) {
  index_train_start = which(data$date == train_start_date)
  index_train_end = which(data$date == train_end_date)
  currency_train = data[index_train_start:index_train_end, currency_index]
  currency_test = data[(index_train_end + 1) : (index_train_end + forecast_days), currency_index]
  currency_train = log(currency_train)
  currency_test = log(currency_test)
  return(list(currency_train, currency_test))
}

forecast_fun <- function(data, model, nvals, test) {
  l = length(data)
  forecast_values = as.vector(predict(model, n.ahead=nvals))
  upper_bound = forecast_values$pred + 1.96*abs(forecast_values$se)
  lower_bound = forecast_values$pred - 1.96*abs(forecast_values$se)
  npts = time(c(data, test))
  ymax = max(max(upper_bound), max(data)) + 0.25
  ymin = min(min(lower_bound), min(data)) - 0.25
  return(list(forecast_values$pred, ymax, ymin, npts, upper_bound, lower_bound))
}

plot_forecast <- function(data, forecast, test, max, min, npts, ub, lb) {
  plot(npts, c(data, test), type="l", ylim=c(min,max), xlab="Time", ylab="difference of log prices")
  points(npts[(length(data)+1):length(c(data,test))],forecast,col="blue")
  lines(npts[(length(data)+1):length(c(data,test))],ub,lty=3,lwd= 2, col="red")
  lines(npts[(length(data)+1):length(c(data,test))],lb,lty=3,lwd= 2, col="red")
}

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


all_data = read.csv(file = "combined_crypto_daily_data.csv", header = T)
market_cap = data.frame(t(all_data[1, ][,grepl("*Market.Cap", names(all_data))]))
market_cap$currency <- rownames(market_cap)
colnames(market_cap) = c("capital", "currency")
market_cap = market_cap[order(-market_cap$capital), ]
rownames(market_cap) = c()

top_currencies = unlist(strsplit(market_cap$currency[1:5], "_"))[ c(TRUE,FALSE, FALSE) ]
close_data = get_close_price(all_data, top_currencies)
close_data$date = as.Date(close_data$date)
close_data = close_data[order(close_data$date), ]


# start date for all analyses, and moving training windows
start_date = as.Date("2017-06-01")
train_start_date_windows = seq(start_date, max(close_data$date) - 31, "days")
train_end_date_windows = train_start_date_windows + 29


# filter data from the specified date
close_data = subset(close_data, date >= start_date)
rownames(close_data) <- seq(length=nrow(close_data)) 

################################## main code for arima experiments ##################################
train_date_windows = seq(train_start_date, end_date, "weeks")
forecast_windows = c(1)

df_perf = data.frame(matrix(nrow = length(train_date_windows)*
                              length(forecast_windows)*(ncol(close_data) - 1), ncol = 11, 0))
colnames(df_perf) = c("currency", "train_start", "num_forecast_vals", "mspe", "mae", "mape", "pm", 
                      "optimal_p", "optimal_q", "optimal_d", "optimal_aic")
index = 1
for(i in 1:length(train_start_date_windows)) {
  for(j in 1:length(forecast_windows)) {
    for(k in 2:ncol(close_data)) {
      c(train, test) := train_test_split(close_data, train_start_date_windows[i], 
                                         train_end_date_windows[i], forecast_windows[j], k)
      
      optimal_pdq = aic_optimal_order(train)
      model_fin = arima(train, order = c(optimal_pdq[1]$p,optimal_pdq[3]$d,optimal_pdq[2]$q), method='ML')
      c(forecast, max, min, npts, ub, lb) := forecast_fun(train, model_fin, forecast_windows[j], test)
      plot_forecast(train, forecast, test, max, min, npts, ub, lb)
      c(mspe, mae, mape, pm) := perf_measure(forecast, test)
      df_perf[index, 1] = colnames(close_data)[k]
      df_perf[index, 2] = train_start_date_windows[i]
      df_perf[index, 3] = forecast_windows[j]
      df_perf[index, 4:7] = c(mspe, mae, mape, pm)
      df_perf[index, 8:11] = optimal_pdq
      index = index + 1
      print(c(i, j, k))
    }
  }
}


