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
# Data Translation and Formatting

for(i in unique(histo_price$SettlementPoint)) {
  SP_df_test <- subset(histo_price, SettlementPoint == i)
  SP_df_test %>% rename(Variable = SettlementPoint)
  str(SP_df_test)
  SP_df_matrix <- SP_df_test %>%
    mutate(Date = as.character(Date)) %>%
    mutate(Date = substr(Date,1,10)) %>%
    mutate(myHour = hour(timestamp))
  hourstats <- SP_df_matrix%>%
    na.omit() %>%
    group_by(Date)%>%
    summarise(X1 = Price[myHour == 0],
              X2 = Price[myHour == 1],
              X3 = Price[myHour == 2],
              X4 = Price[myHour == 3],
              X5 = Price[myHour == 4],
              X6 = Price[myHour == 5],
              X7 = Price[myHour == 6],
              X8 = Price[myHour == 7],
              X9 = Price[myHour == 8],
              X10 = Price[myHour == 9],
              X11 = Price[myHour == 10],
              X12 = Price[myHour == 11],
              X13 = Price[myHour == 12],
              X14 = Price[myHour == 13],
              X15 = Price[myHour == 14],
              X16 = Price[myHour == 15],
              X17 = Price[myHour == 16],
              X18 = Price[myHour == 17],
              X19 = Price[myHour == 18],
              X20 = Price[myHour == 19],
              X21 = Price[myHour == 20],
              X22 = Price[myHour == 21],
              X23 = Price[myHour == 22],
              X24 = Price[myHour == 23])
  hourstats$Variable = i
  write.csv(hourstats,file = paste0("./formattedSpotHistory/spot_",i,".csv"))
}


#library(xts)
#library(zoo)


###############  Bonus - Mean Plot  #################

# Generate two line plots that display the monthly average prices you computed in Task 2 in chronological order
