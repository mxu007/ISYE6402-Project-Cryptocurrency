There are 4 codes overall:

1. data_prep2.R:
    input: transaction_data.csv
    output: data to be fed for churn-prediction
2. churn_model.py:
    input: data prepared in the first code
    output: prediction of customers that are likely to churn
3. Recommendations.R:
    input: takes the data prepared from churn-prediction
    output: recommendation list of past-purchased products that the customer is likely to buy
4. RecommenderCollaborative.ipynb:
    input: Takes the history based product recommendations made by Recommendations.R
    output: Appends collaborative filtering based products (n CF based products that meet a threshold t, n,t - being parameters) to each original product in the list
