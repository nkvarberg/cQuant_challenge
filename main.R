#install.packages("devtools")
# To get pivot_wider
#install.packages("broom")
#devtools::install_github("hadley/tidyverse")
library(tidyverse)
workingDir <- "/Users/nickvarberg/Desktop/cQuant_challenge"
histPricePath <- 'historicalPriceData/'
outputPath <- 'output/'
outputFormattedPath <- 'output/formattedSpotHistory/'

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
# Split date into year month day
# Group dataframe by year and month, 
#  and I'm interpreting the question to mean group by settlement point!
aveMonthly <- prices %>% separate(Date, c("Date", "Time"), " ") %>% 
  separate(Date, into = c("Year", "Month", "Day"), by="-") %>%
  group_by(SettlementPoint, Year, Month) %>% 
  summarise(AveragePrice = mean(Price))

# TASK 3
# Write to csv
aveMonthly %>% write.csv(paste0(outputPath,'AveragePriceByMonth.csv'), row.names = F)

# TASK 4
# First filter to just hubs, then filter to > zero
hubs <- prices %>% filter(str_detect(SettlementPoint, "^HB")) %>% filter(Price>0)
# Compute the hourly price volatility for each year and each settlement hub
hourlyVol <- hubs %>% group_by(SettlementPoint, Year) %>% 
  summarise(HourlyVolatility = sd(log(Price)))

# TASK 5
# Write hourly vol to a csv
hourlyVol %>% write.csv(paste0(outputPath,'HourlyVolatilityByYear.csv'), row.names = F)

# TASK 6
# Summarise highest vol hub for each year
maxHourlyVol <- hourlyVol %>% group_by(Year) %>% 
  filter(HourlyVolatility == max(HourlyVolatility)) %>% arrange(Year)
# Write to a csv
maxHourlyVol %>% write.csv(paste0(outputPath,'MaxVolatilityByYear.csv'), row.names = F)


# TASK 7
# Take prices and split date into Date and Time
# Change Hour to just first two digits
formattedPrices <- prices %>% separate(Date, c("Date", "Time"), " ") %>%
  separate(Time, c('Hour', 'Minute', 'Second'), ':') %>%
  mutate(Hour = as.integer(Hour)+1) %>%
  select(-c(Minute, Second))
# Group by each settlement point
for (setPt in unique(formattedPrices$SettlementPoint)){
  a <- formattedPrices %>% filter(SettlementPoint == setPt) %>%
    pivot_wider(Date, Hour, names_prefix = "X")
  print(head(a))
}






