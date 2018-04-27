
library(dplyr)
library(tools)
library(reshape2)

dunnhumby_path = "/Users/johnabraham/Desktop/dunnhumby_The-Complete-Journey/dunnhumby - The Complete Journey CSV"
processed_files_path = "/Users/johnabraham/Dropbox/MSARelated/Spring2018/MGT6203/ProjectRelated"

## Given a list of <household_key, store_id, churn_week> return {household_key, [product_id_list]}

trans_data <-read.csv(paste(dunnhumby_path,"/transaction_data.csv",sep=""))
product_list <- read.csv(paste(dunnhumby_path,"/product.csv",sep=""))


## Getting a sample of a dummy output from the model
sample_model_output <-read.csv(paste(processed_files_path,"/final_data_use.csv",sep=""))
sample_model_output <- sample_model_output[sample_model_output$if_churn==1,]
sample_model_output <- sample_model_output[sample(1:dim(sample_model_output)[1],100),]
# Ensuring that household_key, store_id comes only for a unique week_no - for ease with evaluating performance
sample_model_output <- as.data.frame(sample_model_output %>% group_by(household_key,store_id) %>% summarise(week_no = max(week_no)))
# Now we have rows with the expected output schema

get_product_lists <- function(model_output, trans=trans_data, products=product_list, purchase_history_lookback=10, top_n_products = 5){
  names(trans) <- tolower(names(trans))
  model_output["churn_week_no"] <- model_output["week_no"]
  model_output["week_no"] <- NULL
  joined <- merge(model_output[c("household_key", "store_id", "churn_week_no")], trans, by=c("household_key", "store_id"))
  # Want to look at data prior to the predicted week of churn
  joined <- joined[joined$week_no<=joined$churn_week_no,]
  # Want to summarize based on recent purchase history, default value looks back 10 weeks
  joined <- joined[joined$week_no>=(joined$churn_week_no-purchase_history_lookback),]
  # ties.method for rank by qty is "max" is because many products might just be bought once by customer
  # there might not be a clear favourite in that case and we do not want to enforce one.
  # Value based ranking is more likely to be unique but chosen max because it better than default  - "average" which gives non-integral ranks
  # Only retaining cases where a household_key purchases at least 3 quantity - to avoid products like say, a costly Television or Vacuum cleaner
  household_top_products <- as.data.frame(joined %>% 
                                            group_by(store_id, household_key, product_id) %>% 
                                            summarize(tot_sales_value = sum(sales_value),tot_product_qty = sum(quantity)) %>% filter(tot_product_qty>2) %>%
                                            mutate(rank_by_value=rank(-tot_sales_value, ties.method="max"), rank_by_qty=rank(-tot_product_qty, ties.method="max")))
  # return top n products by value or by quantity
  #household_top_products <- household_top_products[which((household_top_products$rank_by_value<=top_n_products) | (household_top_products$rank_by_qty<=top_n_products)),  ]
  # For now, only ranking by value
  household_top_products <- household_top_products[which(household_top_products$rank_by_value<=top_n_products),]
  # household_top_products_recommended <- as.data.frame(household_top_products %>% 
  #                                             group_by(store_id, household_key) %>% 
  #                                               summarise(reco_prod_by_value = paste(product_id, collapse="|"), reco_prod_by_qty= paste(product_id, collapse="|")))
  
  # Allowing two kinds of output - one at household_key, store_id, product_id vs household_key, store_id, product_id list, product_name list
  # Make T to F to toggle between output types
    if(T){
    household_top_products['recommendation_rank'] <- household_top_products['rank_by_value']
    return(household_top_products[c('store_id','household_key','recommendation_rank','product_id')])    
  }
  else {
    names(products) <- tolower(names(products))
    household_reco_products <- merge(household_top_products[c("store_id","household_key","product_id")], products[c("product_id", "sub_commodity_desc")], by="product_id")
    household_reco_products$prod_name <- toTitleCase(tolower(trimws(household_reco_products$sub_commodity_desc)))
    household_reco_products <- as.data.frame(household_reco_products %>%
                                               group_by(store_id, household_key) %>%
                                               summarise(product_ids = paste(product_id, collapse="|"),
                                                         product_names = paste(prod_name, collapse="|")))
    return(household_reco_products)
  }
}
  

# Try calling that function
data_output <- get_product_lists(sample_model_output, trans_data, product_list)

# Output the recommendation to be read by the Python code that will add collaborative filtering based recommendations
write.csv(data_output,file=paste(processed_files_path,"/recommendation_output.csv",sep=""),sep = ",",row.names=F)



### Function to get the top n purchases by value if the customer returned in the next 10 weeks after being predicted to be a churn.
# churn_noshop_duration is the no. of weeks the customer does not shop
get_future_products <- function(model_output, trans=trans_data, purchase_lookahead=10, top_n_products = 5, churn_noshop_duration=2){
  names(trans) <- tolower(names(trans))
  model_output["churn_week_no"] <- model_output["week_no"]
  model_output["week_no"] <- NULL
  joined <- merge(model_output[c("household_key", "store_id", "churn_week_no")], trans, by=c("household_key", "store_id"))
  
  # Retaining data between churn week and (churn week+purchase_lookahead) week
  joined <- joined[joined$week_no<=joined$churn_week_no+purchase_lookahead+churn_noshop_duration,]
  joined <- joined[joined$week_no>=(joined$churn_week_no+churn_noshop_duration),]
  # ties.method for rank by qty is "max" is because many products might just be bought once by customer
  # there might not be a clear favourite in that case and we do not want to enforce one.
  # Value based ranking is more likely to be unique but chosen max because it better than default  - "average" which gives non-integral ranks
  # Only retaining cases where a household_key purchases at least 3 quantity - to avoid products like say, a costly Television or Vacuum cleaner
  household_top_products <- as.data.frame(joined %>% 
                                            group_by(store_id, household_key, product_id) %>% 
                                            summarize(tot_sales_value = sum(sales_value),tot_product_qty = sum(quantity)) %>% filter(tot_product_qty>2) %>%
                                            mutate(rank_by_value=rank(-tot_sales_value, ties.method="max"), rank_by_qty=rank(-tot_product_qty, ties.method="max")))
  # return top n products by value or by quantity
  #household_top_products <- household_top_products[which((household_top_products$rank_by_value<=top_n_products) | (household_top_products$rank_by_qty<=top_n_products)),  ]
  # For now, only ranking by value
  household_top_products <- household_top_products[which(household_top_products$rank_by_value<=top_n_products),]
  # household_top_products_recommended <- as.data.frame(household_top_products %>% 
  #                                             group_by(store_id, household_key) %>% 
  #                                               summarise(reco_prod_by_value = paste(product_id, collapse="|"), reco_prod_by_qty= paste(product_id, collapse="|")))
  
  # Allowing two kinds of output - one at household_key, store_id, product_id vs household_key, store_id, product_id list, product_name list
  # Make T to F to toggle between output types
    household_top_products['actual_rank'] <- household_top_products['rank_by_value']
    return(household_top_products[c('store_id','household_key','actual_rank','product_id')])    
 
}

future_data_output <- get_future_products(sample_model_output, trans_data)

#####################################


## Evaluating the efficacy of recommendations

evaluate_recommendations <- function(recommendations, future_purchase, match_level=1){
  ### Function returns - a list of 3 numbers
  ### 1st is the number of store_id,household_key pairs where the recommended and the future product_ids match - meaning our recommendations were spot on 
  ### 2nd is the number of store_id,household_key pairs that came back to shop - unique pairs in the future_purchase file
  ### 3rd is the number of store_id, household_key we started with  - unique pairs in the recommendations file
  
  future_purchase['purchase'] <- 1
  recommendation_match <- merge(recommendations,future_purchase, by=c("store_id","household_key","product_id"),all.x=T)
  recommendation_match_summary <- as.data.frame(recommendation_match %>% group_by(store_id,household_key) %>% summarise(tot_count=n(),match_recos = sum(purchase,na.rm=T)))
  recommendation_match_summary['reco_success'] <- as.numeric(recommendation_match_summary$match_recos>=match_level)
  # Returns Customers whose purchase were were able to predict at at least match level, Customers who returned at all, Total No. of customers in churn set
  return(c(sum(recommendation_match_summary$reco_success), dim(unique(future_purchase[c("store_id","household_key")]))[1], dim(recommendation_match_summary)[1]))
  
}

# Read the file with the collaborative filtering based recommendations added from the Jupyter notebook code.
recommendation_augmented_output <- read.csv(paste(processed_files_path,"/recommendation_augmented_output.csv",sep=""))


#evaluate_recommendations(data_output,future_data_output)
#evaluate_recommendations(recommendation_augmented_output,future_data_output)


#evaluate_recommendations(data_output, get_future_products(sample_model_output, trans_data, purchase_lookahead = 20),match_level = 1)
#evaluate_recommendations(recommendation_augmented_output, get_future_products(sample_model_output, trans_data, purchase_lookahead = 20), match_level = 1)


#evaluate_recommendations(data_output,future_data_output,match_level = 2)
#evaluate_recommendations(recommendation_augmented_output, future_data_output, match_level = 2)


## Evaluate reco perf
evaluation <- function(data_output,recommendation_augmented_output, future_data_output, match_range=c(1,2,3)){
  matches = c()
  return_customers = c()
  churn_customers = c()
  type = c()
  match = c()  
  for(m in match_range){
    output = evaluate_recommendations(data_output, future_data_output, match_level = m)
    match = c(match,m)
    type =c(type,"History")
    matches = c(matches,output[1])
    return_customers = c(return_customers,output[2])
    churn_customers = c(churn_customers, output[3])
    output = evaluate_recommendations(recommendation_augmented_output, future_data_output, match_level = m)
    match = c(match,m)
    type =c(type,"History+CF")
    matches = c(matches,output[1])
    return_customers = c(return_customers,output[2])
    churn_customers = c(churn_customers, output[3])
  }
  return(data.frame(Match_Level = match, Recommendation_Type=type, Customer_Matches=matches,Return_Customers = return_customers, Overall_Customers=churn_customers))
}

evaluation(data_output, recommendation_augmented_output, future_data_output, match_range = c(1,2,3,4))
