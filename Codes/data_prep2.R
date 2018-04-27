setwd("E:/mgt_6203/project/data")

# reading the data
causal_data = read.csv(file = "causal_data.csv", header = T)
txn_data = read.csv(file = "transaction_data.csv", header = T)
hh_data = read.csv(file = "hh_demographic.csv", header = T)
income_map = read.csv(file = "income_level.csv", header = T)

colnames(txn_data) = tolower(colnames(txn_data))
# threshold value for churn
thresh_churn = 360
frac_threshold = 0.8
min_cutoff = 0.3

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



# function for last n weeks data, for a particular column

last_nweeks = function(current_week, lastk, data) {
  temp = subset(data, week_no %in% seq(current_week - lastk, current_week))
  if(nrow(temp) > 0) {
    sales = sum(temp$total_sales)
    disc = sum(temp$total_disc)
    orders = sum(temp$num_orders)
    freq = orders/lastk
    return(list(sales, disc, freq))
  } else {
    return(list(sales, disc, freq))
  }
  
}

txn_data_rollup = sqldf("select household_key, store_id, week_no, 
                        max(day) as last_purchase_day_week,
                        sum(sales_value) as total_sales, 
                        sum(retail_disc) as total_disc, 
                        count(distinct basket_id) as num_orders, 
                        sum(quantity) as total_qty
                        from txn_data
                        group by household_key, store_id, week_no
                        order by household_key, store_id, week_no asc")
txn_data_rollup = subset(txn_data_rollup, total_sales > 0)

hh_weekly_purchase = sqldf("select household_key, week_no, sum(sales_value) as total_sales
                           from txn_data group by household_key, week_no 
                           order by household_key, week_no")


txn_data_new = sqldf("select a.*, b.total_sales as overall_weekly_purchase, 
                      round(a.total_sales/b.total_sales, 2) as frac_share from txn_data_rollup as a 
                      left join hh_weekly_purchase as b on a.household_key = b.household_key 
                      and a.week_no = b.week_no")

fav_store = sqldf("select household_key, store_id as store_id, sum(total_sales) as total_sales_hhs from txn_data_rollup
                  group by household_key, store_id order by household_key, store_id, total_sales_hhs desc")

fav_store = fav_store[order(fav_store$household_key, -fav_store$total_sales_hhs), ]
store_sales = sqldf("select a.household_key, a.store_id, 
                    round(a.total_sales_hhs / b.total_sales_hh, 2) as market_share
                    from fav_store as a left join
                    (
                      select household_key, sum(total_sales_hhs) as total_sales_hh from fav_store 
                      group by household_key
                    ) as b on a.household_key = b.household_key")
                

txn_data_fin_raw = sqldf("select a.*, b.market_share as overall_market_share 
                         from txn_data_new as a left join store_sales as b on a.household_key = b.household_key
                         and a.store_id = b.store_id")


#txn_data_fin_raw$if_churn = ifelse(txn_data_fin_raw$frac_share < txn_data_fin_raw$overall_market_share*frac_threshold
#                                   & txn_data_fin_raw$overall_market_share > min_cutoff, 1, 0)


txn_data_fin_raw = txn_data_fin_raw[order(txn_data_fin_raw$household_key, 
                                          txn_data_fin_raw$store_id, 
                                          txn_data_fin_raw$week_no), ]


txn_data_fin_raw = txn_data_fin_raw %>% group_by(household_key, store_id) %>% mutate(row_id = row_number())
txn_data_fin_raw = data.frame(txn_data_fin_raw)


key_stores = sqldf("select store_id, count(distinct household_key) as distinct_hh from txn_data_fin_raw where 
                   overall_market_share > 0.3 group by store_id order by distinct_hh desc limit 20")

filter_data_model = subset(txn_data_fin_raw, store_id %in% key_stores$store_id & overall_market_share > 0.3)
filter_data_model$if_churn = 0
for(i in 1:(nrow(filter_data_model) - 1)) {
  if((filter_data_model[i+1, 1] == filter_data_model[i, 1]) & 
     (filter_data_model[i+1, 3] - filter_data_model[i, 3] > 2)) {
    filter_data_model$if_churn[i] = 1
  } else {
    filter_data_model$if_churn[i] = 0
  }
}

filter_data_model$three_weeks_sales = 0
filter_data_model$three_weeks_disc = 0
filter_data_model$three_weeks_freq = 0

filter_data_model$five_weeks_sales = 0
filter_data_model$five_weeks_disc = 0
filter_data_model$five_weeks_freq = 0


for(i in 1:nrow(filter_data_model)) {
  if(filter_data_model$row_id[i] > 1) {
    c(filter_data_model$three_weeks_sales[i], 
      filter_data_model$three_weeks_disc[i], 
      filter_data_model$three_weeks_freq[i]) := last_nweeks(filter_data_model$week_no[i], 3, 
                                                            subset(filter_data_model, 
                                                                   household_key == filter_data_model$household_key[i]))
    c(filter_data_model$five_weeks_sales[i], 
      filter_data_model$five_weeks_disc[i], 
      filter_data_model$five_weeks_freq[i]) := last_nweeks(filter_data_model$week_no[i], 5, 
                                                           subset(filter_data_model, 
                                                                  household_key == filter_data_model$household_key[i]))
  }
}

filter_data_model = filter_data_model %>% 
                    group_by(household_key, store_id) %>% 
                    mutate(weeklag = lag(week_no, n = 1))
filter_data_model = data.frame(filter_data_model)
filter_data_model$weeklag = filter_data_model$week_no - filter_data_model$weeklag
filter_data_model[is.na(filter_data_model)] <- 0

write.csv(file = "final_data_use.csv", filter_data_model, row.names = F)
