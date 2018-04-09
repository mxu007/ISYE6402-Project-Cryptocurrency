##ISYE6402 HW3
library(data.table)

# Load data
data <- read.csv("combined_crypto_daily_data.csv",header=TRUE)
data<- data[nrow(data):1,]
rownames(data) = 1:nrow(data)

#data <- data[1:1660,]

# Subset data based on cryptocurrencies
bitcoin <- data[c(1345:1760),c(1:7)]
ethereum <- data[c(1345:1760),c(1,14:19)]
ripple <- data[c(1345:1760),c(1,68:73)]
litecoin <- data[c(1345:1760),c(1,26:31)]
neo <- data[c(1345:1760),c(1,44:47)]

# Subset data based on cryptocurrencies
#bitcoin <- data[c(1345:1660),c(1:7)]
#ethereum <- data[c(1345:1660),c(1,14:19)]
#ripple <- data[c(1345:1660),c(1,68:73)]
#litecoin <- data[c(1345:1660),c(1,26:31)]
#neo <- data[c(1345:1660),c(1,44:47)]

# ACF
close <- cbind(bitcoin[,c(1,5)],ethereum[,5],ripple[,5], litecoin[,5], neo[,5]) 
colnames(close) <- c("data", "bitcoin_close", "ethereum_close", "ripple_close", "litecoin_close", "neo_close")

close_train = close[1:(nrow(close)-7),]
close_test = close[(nrow(close)-6):nrow(close),]

close_train_ts <- ts(close_train[,-1])

# Plot time series
plot(close_train_ts, plot.type="single", col = 1:ncol(close_train_ts))
legend("topleft", colnames(close_train_ts), col=1:ncol(close_train), lty=1, cex=1)

# ACF of log differenced time series
acf(close_train_ts)

clsoe_ts_diff_log <- diff(log(close_train_ts))
plot(clsoe_ts_diff_log, plot.type="single", col = 1:ncol(clsoe_ts_diff_log))
legend("topleft", colnames(clsoe_ts_diff_log), col=1:ncol(close), lty=1, cex=1)

acf(clsoe_ts_diff_log)

library(vars)
mul_var_1 <- VAR(clsoe_ts_diff_log, lag.max=20, type="none")
mul_var_2 <- VAR(clsoe_ts_diff_log, lag.max=20, type="const")
mul_var_3 <- VAR(clsoe_ts_diff_log, lag.max=20, type="trend")
mul_var_4 <- VAR(clsoe_ts_diff_log, lag.max=20, type="both")
mul_var_1$p
mul_var_2$p
mul_var_3$p
mul_var_4$p

mul_fit <- VAR(clsoe_ts_diff_log, lag.max=10, type="both")
arch.test(mul_fit)
normality.test(mul_fit)

Box.test(resid(mul_fit)[,1], lag =(10+1), type="Ljung-Box", fitdf=10)
Box.test(resid(mul_fit)[,2], lag =(10+1), type="Ljung-Box", fitdf=10)
Box.test(resid(mul_fit)[,3], lag =(10+1), type="Ljung-Box", fitdf=10)
Box.test(resid(mul_fit)[,4], lag =(10+1), type="Ljung-Box", fitdf=10)
Box.test(resid(mul_fit)[,5], lag =(10+1), type="Ljung-Box", fitdf=10)


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


