library(tidyverse)
histPricePath <- 'historicalPriceData/'


# TASK 1
# try file.names
histPriceName <- 'historicalPriceData/ERCOT_DA_Prices_'
years <- c(2016, 2017, 2018, 2019)
prices <- NULL
for (year in years){
  df <- read_csv(paste0(histPriceName,year,'.csv'))
  if (is.null(prices)){
    prices <- df
  } else {
    prices <- full_join(prices, df)
  }
} 

# TASK 2
# Split date into date and time
prices <- prices %>% separate(Date, c("date", "time"), " ")
# Split date into year months day
prices <- prices %>% separate(date, into = c("year", "month", "day"), by="-") 
# group dataframe by year and month, average over both hubs and loading zones
aveMonthly <- prices %>% group_by(year, month) %>% summarise(avePrice = mean(Price))

# TASK 3
# Write to csv