# ISYE6402-Project-Cryptocurrency
ISYE6402 Time Series Analysis Project -- Cryptocurrencies: Pricing, Volatility and Trading Strategy Using Time Series Analysis

Team Member : Nirmit Chetwani, Tianyi Liu, Minghan Xu

Codes:
1. data_prep:
    input: all the raw csv files that we extract using Kaggle data source and other sources
    output: a standard, flat table that contains metrics related to all cryptocurrencies and external indices, csv called "combined_crypto_daily_data new.csv" inside "\data\inputs"

2. arima:
    input: takes the data prepared above
    what it does: performs ARIMA forecasts, and executes the trading strategy
    output: three csv files names btc_analysis, dji_analysis and ethereum analysis containing details like forecasts,
            s.e, optimal arima orders related to these currencies. The data from these files is merged into a csv "trend_analysis". All these files are in the folder "\data\outputs".

3. arimax_var:
    input: takes the data prepared in the first step
    output: two csv files named: arima_analysis and var_analysis located in folder "\data\outputs"

4. garch:
    input: takes input "garch_data.csv" present in "\data\inputs"
    output: graphs and analysis in the R-code itself.

5. xgb_model:
    input: btc_data, containing only the bitcoin data, but all fields like open, close, volume, etc.
    output: a file containing predictions from xg-boost, in the file called "btc_xgb_output.csv" inside the folder "\data\outputs". The trading strategy for this is implemented in the file "trend_analysis" in the folder "\data\outputs".
