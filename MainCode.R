### Chuqi(Aria) Cai
### Energy Analyst Programming Exercise

# Setup
install.packages("ggplot2")
library(ggplot2)
#library(plyr) #Tools for Splitting, Applying and Combining Data
library(tidyverse) #Getting data frames to tidy
library(lubridate) #For dates and date-times
library(dplyr)
library(forecast)

###############  Task 1  #################

Price_2016 <- read.csv(file="./historicalPriceData/ERCOT_DA_Prices_2016.csv",header=TRUE)
Price_2017 <- read.csv(file="./historicalPriceData/ERCOT_DA_Prices_2017.csv",header=TRUE)
Price_2018 <- read.csv(file="./historicalPriceData/ERCOT_DA_Prices_2018.csv",header=TRUE)
Price_2019 <- read.csv(file="./historicalPriceData/ERCOT_DA_Prices_2019.csv",header=TRUE)
names(Price_2016)

# combine into single dataframe
histo_price <- rbind(Price_2016,Price_2017,Price_2018,Price_2019)
str(histo_price)
###############  Task 2  #################

histo_price$timestamp <- as.POSIXct(histo_price$Date, format='%Y-%m-%d %H:%M:%OS')
str(histo_price)

# Aggregate data into monthly: Compute the average price for each settlement point and year-month in the historical dataset
histo_monthly_price <-  histo_price %>%
  na.omit() %>%
  group_by(SettlementPoint,Time = floor_date(timestamp,unit="month")) %>%
  summarize(AveragePrice = mean(Price, na.rm = TRUE))

###############  Task 3  #################
# Write the computed monthly average prices to file as a CSV
#add Year and date column
histo_monthly_price$Year = year(histo_monthly_price$Time)
histo_monthly_price$Month = month(histo_monthly_price$Time)
histo_monthly_price <- subset(histo_monthly_price, select = -Time)
# Write the data frame as csv
write_csv(histo_monthly_price, file = "./output/AveragePriceByMonth.csv")

###############  Task 4  #################
# Filter out the data for Settlement hubs
histo_price_hub <-  histo_price %>% 
  filter(substr(histo_price$SettlementPoint, 1, 2) == "HB", Price > 0)
# Add year column and compute log price
histo_price_hub$Year = year(histo_price_hub$timestamp)
histo_price_hub$log_price = log(histo_price_hub$Price)

# Compute the hourly price1 volatility for each year and each settlement hub in the historical power price data
histo_price_volatility <-  histo_price_hub %>%
  na.omit() %>%
  group_by(SettlementPoint,Year) %>%
  summarize(HourlyVolatility = sd(Price, na.rm = TRUE))

###############  Task 5  #################
# Write the data frame as csv
write_csv(histo_price_volatility, file = "./output/HourlyVolatilityByYear.csv")

###############  Task 6  #################
# Determine which settlement hub showed the highest overall hourly volatility for each historical year.
max_volatility <-  histo_price_volatility %>%
  group_by(Year) %>% 
  top_n(1, HourlyVolatility)

# Write the data frame as csv
write_csv(max_volatility, file = "./output/MaxVolatilityByYear.csv")


###############  Task 7  #################








