# Nick Varberg
# April 17, 2020
# cQuant Energy Analyst coding challenge

# Load libraries and set filepaths
library(tidyverse)
workingDir <- "/Users/nickvarberg/Desktop/cQuant_challenge"
histPricePath <- 'historicalPriceData/'
outputPath <- 'output/'
outputFormattedPath <- 'output/formattedSpotHistory/'
hourlyShapePath <- 'output/hourlyShapeProfiles/'

# First set working directory
setwd(workingDir)


## TASK 1
prices <- NULL
for (file in list.files(histPricePath)){
  df <- read_csv(paste0(histPricePath,file))
  if (is.null(prices)){
    prices <- df
  } else {
    prices <- full_join(prices, df)
  }
} 

##TASK 2
# Split date into date and time
# Split date into year month day
# Group dataframe by year and month, 
#  and I'm interpreting the question to mean group by settlement point!
aveMonthly <- prices %>% separate(Date, c("Date", "Time"), " ") %>% 
  separate(Date, into = c("Year", "Month", "Day"), by="-") %>%
  group_by(SettlementPoint, Year, Month) %>% 
  summarise(AveragePrice = mean(Price))

## TASK 3
# Write to csv
aveMonthly %>% write.csv(paste0(outputPath,'AveragePriceByMonth.csv'), row.names = F)


## TASK 4
# First filter to just hubs, then filter to > zero
# Compute the hourly price volatility for each year and each settlement hub
hourlyVol <- prices %>% 
  filter(str_detect(SettlementPoint, "^HB")) %>% 
  filter(Price>0) %>%
  separate(Date, into = c("Year", "Month", "Day"), by="-") %>%
  group_by(SettlementPoint, Year) %>% 
  summarise(HourlyVolatility = sd(log(Price)))

## TASK 5
# Write hourly vol to a csv
hourlyVol %>% write.csv(paste0(outputPath,'HourlyVolatilityByYear.csv'), row.names = F)


## TASK 6
# Summarise highest vol hub for each year
maxHourlyVol <- hourlyVol %>% group_by(Year) %>% 
  filter(HourlyVolatility == max(HourlyVolatility)) %>% arrange(Year)
# Write to a csv
maxHourlyVol %>% write.csv(paste0(outputPath,'MaxVolatilityByYear.csv'), row.names = F)


## TASK 7
# Take prices and split date into Date and Time
# Change Hour to just first two digits
formattedPrices <- prices %>% separate(Date, c("Date", "Time"), " ") %>%
  separate(Time, c('Hour', 'Minute', 'Second'), ':') %>%
  mutate(Hourend = as.integer(Hour)+1) %>%
  select(-c(Hour, Minute, Second))
# Group by each settlement point
# If I had pivot_wider() I would use it here but I couldn't get it to load,
#  so I used spread()
for (setPt in unique(formattedPrices$SettlementPoint)){
  a <- formattedPrices %>% filter(SettlementPoint == setPt) %>%
    spread(Hourend, Price)
  # Change colnames to Date, Variable, X1, X2, ...
  newcols <- colnames(a)
  newcols[2] <- 'Variable'
  for (i in 3:length(newcols)){
    newcols[i] = paste0('X', newcols[i])
  }
  colnames(a) <- newcols
  # reorder columns
  a <- a[c(2,1,3:ncol(a))]
  # write to csv
  a %>% write.csv(
    paste0(outputFormattedPath,'spot_',setPt, '.csv'), 
    row.names = F)
}


## BONUS MEAN PLOTS
# First create Date column
plotMonthly <- aveMonthly %>% 
  mutate(Day = '01') %>%
  unite(Date, Year, Month, Day,sep = "-") %>%
  mutate(Date = as.Date(Date))

# The first plot should show the monthly average prices for all settlement hubs
hubsPlot <- ggplot(filter(plotMonthly, str_detect(SettlementPoint, '^HB')), 
          aes(x=Date, y=AveragePrice, color=SettlementPoint)) +
  geom_line() + 
  scale_x_date(date_breaks = "months" , date_labels = "%Y-%m-%d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# Save to png
ggsave(paste0(outputPath, 'SettlementHubAveragePriceByMonth.png'),
       plot = hubsPlot)

# The second plot should show the monthly average prices for all load zones
loadzonesPlot <- ggplot(filter(plotMonthly, str_detect(SettlementPoint, '^LZ')), 
                   aes(x=Date, y=AveragePrice, color=SettlementPoint)) +
  geom_line() + 
  scale_x_date(date_breaks = "months" , date_labels = "%Y-%m-%d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# Save to png
ggsave(paste0(outputPath, 'LoadZoneAveragePriceByMonth.png'),
       plot = loadzonesPlot)


## BONUS VOLATILITY PLOTS
hourlyVol$SettlementPoint <- as.factor(hourlyVol$SettlementPoint)
hourlyVolPlot <- ggplot(filter(hourlyVol, SettlementPoint != 'HB_PAN'), 
       aes(x=Year, y=HourlyVolatility, group=SettlementPoint, color=SettlementPoint)) + 
  geom_line() +
  ggtitle('Yearlong Power Price Hourly Vol is increasing recently')
ggsave(paste0(outputPath, 'HourlyVolatilityByYear.png'),
       plot = hourlyVolPlot)


## BONUS HOURLY SHAPE PROFILE CALCULATION
for (SetPt in unique(formattedPrices$SettlementPoint)){
  hourlyShape <- formattedPrices %>% 
    mutate(Weekday = weekdays(as.Date(Date))) %>%
    filter(SettlementPoint == SetPt) %>%
    separate(Date, into = c("Year", "Month", "Day"), by="-") %>%
    group_by(SettlementPoint, Month, Weekday, Hourend) %>%
    summarise(AveragePrice = mean(Price)) %>% # average price per hour
    group_by(SettlementPoint, Month, Weekday) %>%
    mutate(NormAveragePrice = AveragePrice/mean(AveragePrice)) %>% # hour average over day average
    select(SettlementPoint, Month, Weekday, Hourend, NormAveragePrice)
  hourlyShape %>% write.csv(paste0(hourlyShapePath,'profile_',SetPt,'.csv'), row.names = F)
}

# I enjoyed this challenge.
# Thank you for taking the time to look over my work!
# I look forward to hearing back.
# Nick Varberg
# nicholas.varberg@colorado.edu