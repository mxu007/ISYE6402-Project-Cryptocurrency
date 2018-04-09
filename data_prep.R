setwd("E:/isye_6402/project/cryptocurrencypricehistory")
library("lubridate")
library("date")

filenames = c("bitcoin_price", "dash_price", "ethereum_price", "iota_price", "litecoin_price", 
              "monero_price", "nem_price", "neo_price", "numeraire_price", "omisego_price", "qtum_price", 
              "ripple_price", "stratis_price", "waves_price")
combined_data = c()

# function to transform the data
txy = function(data) {
  colnames(data) = paste(filenames[i], colnames(data), sep = "_")
  colnames(data)[1] = "date"
  data$date = as.date(data$date)
  data$date = as.Date(data$date, origin = "1960-01-01")
  data[, 6] = as.numeric(gsub(",", "", data[, 6]))
  data[, 7] = as.numeric(gsub(",", "", data[, 7]))
  return(data)  
}


for(i in 1:length(filenames)) {
  data = read.csv(file = paste0(filenames[i], ".csv"), header = T, stringsAsFactors = F)
  assign(paste0(strsplit(filenames[i], "_")[[1]][1]), txy(data))
}

final_data = sqldf("select a.*, b.*, c.*, d.*, e.*, f.*, g.*, h.*, i.*, j.*, k.*, l.*, m.*, n.*
                   from bitcoin as a 
                   left join dash as b on b.date = a.date
                   left join ethereum as c on c.date = a.date
                   left join iota as d on d.date = a.date
                   left join litecoin as e on e.date = a.date
                   left join monero as f on f.date = a.date
                   left join nem as g on g.date = a.date
                   left join neo as h on h.date = a.date 
                   left join numeraire as i on i.date = a.date 
                   left join omisego as j on j.date = a.date 
                   left join qtum as k on k.date = a.date
                   left join ripple as l on l.date = a.date 
                   left join stratis as m on m.date = a.date 
                   left join waves as n on n.date = a.date")
head(final_data)
date = as.Date(final_data[, 1], origin = "1960-01-01")
final_data = final_data[ , -which(names(final_data) %in% c("date"))]
final_data = cbind(data.frame(date), final_data)
write.csv(file = "combined_crypto_daily_data.csv", final_data, row.names = F)
