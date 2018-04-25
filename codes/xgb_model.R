setwd("E:/isye_6402/project/codes_isye6402/data/")
library("lubridate")
library("date")
library("anytime")
library("dplyr")
library("zoo")
library("sqldf")

library(quantmod)
library(TTR)
library(xgboost)

df = read.csv(file = "btc_xgb_data.csv", header = T)
colnames(df) = c("Date", "Open", "High", "Low", "Close", "Volume")
# Define the technical indicators to build the model 
rsi = RSI(df$Close, n=14, maType="WMA")
adx = data.frame(ADX(df[,c("High","Low","Close")]))
sar = SAR(df[,c("High","Low")], accel = c(0.02, 0.2))
trend = df$Close - sar
# create a lag in the technical indicators to avoid look-ahead bias 
rsi = c(NA,head(rsi,-1)) 
adx$ADX = c(NA,head(adx$ADX,-1))
trend = c(NA,head(trend,-1))

# Create the target variable
price = df$Close-df$Open
class = ifelse(price > 0,1,0)

# Create a Matrix for XGB Inputs
model_df = data.frame(class,rsi,adx$ADX,trend)
model = matrix(c(class,rsi,adx$ADX,trend), nrow=length(class))
model = na.omit(model)
colnames(model) = c("class","rsi","adx","trend")

# Split data into train and test sets 
breakpoint = nrow(model) - 235
training_data = model[1:breakpoint,]
test_data = model[(breakpoint+1):nrow(model),]
# Split data training and test data into X and Y
X_train = training_data[,2:4] ; Y_train = training_data[,1]
class(X_train)[1]
class(Y_train)
X_test = test_data[,2:4] 
Y_test = test_data[,1]
class(X_test)[1] 
class(Y_test)


# Train the xgboost model using the "xgboost" function
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgModel = xgboost(data = dtrain, nround = 5, objective = "binary:logistic")

# [1]	train-error:0.352645 
# [2]	train-error:0.320739 
# [3]	train-error:0.285474 
# [4]	train-error:0.260285 
# [5]	train-error:0.259446 

# Using cross validation
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "binary:logistic")
print(cv)

##### xgb.cv 5-folds
# iter train_error_mean train_error_std test_error_mean test_error_std
# 1        0.3528512      0.01363336       0.5054568     0.01042790
# 2        0.3257738      0.01647729       0.4970464     0.02212760
# 3        0.2963822      0.02478594       0.4936784     0.02314204
# 4        0.2768620      0.02491389       0.4928446     0.02374211
# 5        0.2686736      0.03070259       0.4953590     0.02456430
# 6        0.2619562      0.03762289       0.4953730     0.02400243
# 7        0.2369764      0.03486017       0.4995570     0.03204541
# 8        0.2256450      0.02412850       0.4961992     0.02656745
# 9        0.2172496      0.02219724       0.4961958     0.02753925
# 10        0.2048672      0.01509351       0.4987062     0.02812426


preds = predict(xgModel, X_test)
test_df = data.frame(test_data)
test_df = cbind(test_df, data.frame(preds))
test_df_fin = cbind(test_df, df[(nrow(df) - 234): nrow(df), ])
write.csv(file = "xgb_output.csv", test_df_fin, row.names = F)
