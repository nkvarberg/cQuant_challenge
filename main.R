library(tidyverse)
workingDir <- "/Users/nickvarberg/Desktop/cQuant_challenge"
histPricePath <- 'historicalPriceData/'
outputPath <- 'output/'

# First set working directory
setwd(workingDir)

# TASK 1
# try file.names
histPriceName <- 'ERCOT_DA_Prices_'
years <- c(2016, 2017, 2018, 2019)
prices <- NULL
for (year in years){
  df <- read_csv(paste0(histPricePath,histPriceName,year,'.csv'))
  if (is.null(prices)){
    prices <- df
  } else {
    prices <- full_join(prices, df)
  }
} 

# TASK 2
# Split date into date and time
prices <- prices %>% separate(Date, c("Date", "Time"), " ")
# Split date into year months day
prices <- prices %>% separate(Date, into = c("Year", "Month", "Day"), by="-") 
# Group dataframe by year and month, 
#  and I'm interpreting the question to mean group by settlement pont!
aveMonthly <- prices %>% group_by(SettlementPoint, Year, Month)
# I'm interpreting the question to mean
aveMonthly <- aveMonthly %>% summarise(AveragePrice = mean(Price))
# TASK 3
# Write to csv
aveMonthly %>% write.csv(paste0(outputPath,'AveragePriceByMonth.csv'), row.names = F)
