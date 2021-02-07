# Main script to run for automatic evaluator
library(data.table)
library(lubridate)
library(ggplot2)


## ----Reading in Data: #TASK 1-------------------------------------------------------
historical_price_files <- list.files('historicalPriceData',full.names = T) #All files here are ERCOT_DA but if files from multiple markets exist in dir, can do regex matching with pattern argument to read them separately

pull_data <- function(file) {
  if (!file.exists(file)){
    data <- NULL
  } else {
    data <- fread(file)
  }
  data
}
price_data <- rbindlist(lapply(historical_price_files, pull_data))



## ----Aggregating Prices: #TASK 2--------------------------------------------------
avg_price_data <- price_data[,.(AveragePrice = mean(Price)), by = .(SettlementPoint, Year = year(Date), Month = month(Date))]


## ----Saving monthly averages to csv: #TASK 3--------------------------------------
dir.create('all_outputs')
output_dir <- "all_outputs/"
fwrite(avg_price_data, paste0(output_dir,'AveragePriceByMonth.csv'))


## ----Price Volatility: #TASK 4----------------------------------------------------
hourly_volatility <- price_data[Price > 0  & grepl("HB_",SettlementPoint), 
                                .(HourlyVolatility = sd(log(Price))), 
                                by = .(SettlementPoint, Year = year(Date))]



## ----Saving Hourly Volatility to csv: #TASK 5-------------------------------------
# if dir already exists, no adverse effect, only warning. By having this here we can run this chunk independently of the monthly avg price task
dir.create('all_outputs')
fwrite(hourly_volatility, paste0(output_dir,'HourlyVolatilityByYear.csv'))


## ----Max. Hourly Volatility: #TSK 6----------------------------------------------

max_volatility <- hourly_volatility[ ,.SD[which.max(HourlyVolatility)], by = Year]
setcolorder(max_volatility, c("SettlementPoint","Year","HourlyVolatility")) # Just making sure col order is same as task5 in case required by evaluator
fwrite(max_volatility, paste0(output_dir,'MaxVolatilityByYear.csv'))



## ----Data Translation for Model Input: #TASK 7----
price_data <- price_data[ ,hr:= hour(Date) + 1]  # Hour ending values as required for translated column names
spot_prices <- dcast(price_data, SettlementPoint + date(Date) ~ reorder(paste0("X", hr),hr), value.var = 'Price') # Pivot of factors in rows into columns
setnames(spot_prices,"SettlementPoint","Variable", skip_absent = T) 
spot_price_by_setlmnt_point <- split(spot_prices, by = "Variable")  #Splitting data table into list of smaller ones by SettlementPoint

# Saving the split spot price data as csvs
dir.create(paste0(output_dir,"formattedSpotHistory"))
lapply(seq_along(spot_price_by_setlmnt_point), function(i){
  fwrite(spot_price_by_setlmnt_point[[i]], paste0(output_dir,"formattedSpotHistory/","spot_",
                                                  names(spot_price_by_setlmnt_point)[i],
                                                  ".csv"))
}
)


## ----Plots for Monthly Average Prices: #BNS - Mean Plots------------------------
# Creating a date Col for Avg Monthly Prices
avg_price_data <- avg_price_data[ ,Date:= ymd(paste0(Year, "-", Month, "-" ,"01"))]  # A little inelegant, maybe there is a better lubridate function for this

# SettlementHub Average Prices
hubprices <- avg_price_data[grepl("HB_",SettlementPoint), ]

hub_plot <- ggplot(hubprices, aes(x = date(Date), y = AveragePrice, color = SettlementPoint)) + 
  geom_line() +
  ggtitle('Monthly Average for ERCOT Settlement Hubs') + 
  xlab('Date') + 
  ylab('Price') +
  scale_x_date(date_breaks = "months", date_labels = "%b%y") + 
  theme(axis.text.x = element_text(angle=90, hjust = 1))

ggsave(paste0(output_dir,"SettlementHubAveragePriceByMonth.png"), hub_plot)

# LoadZone Average Prices
lzprices <- avg_price_data[grepl("LZ_",SettlementPoint), ]

lz_plot <- ggplot(lzprices, aes(x = date(Date), y = AveragePrice, color = SettlementPoint)) + 
  geom_line() + 
  ggtitle('Monthly Average for ERCOT Load Zones') +
  xlab('Date') +
  ylab('Price') +
  scale_x_date(date_breaks = "months", date_labels = "%b%y") + 
  theme(axis.text.x = element_text(angle=90, hjust = 1))
ggsave(paste0(output_dir,"LoadZoneAveragePriceByMonth.png"), lz_plot)


## ----Plots for Volatility: #BNS - Volatility Plots------------------------------------

volatility_trend <- ggplot(hourly_volatility, aes(x = Year, y = HourlyVolatility,  color = SettlementPoint)) + 
  geom_smooth(method = lm, alpha = 0.1) + 
  geom_point() +
  #geom_bar(stat = 'identity', position = position_dodge(width = 0.5)) + 
  ggtitle('Volatility trend for ERCOT Hubs') +
  xlab('Year') +
  ylab('Volatility')
ggsave(paste0(output_dir,"Volatility trend for ERCOT Hubs.png"), volatility_trend)

avg_volatility <- ggplot(hourly_volatility, aes(x = SettlementPoint, y = HourlyVolatility)) + 
  geom_bar(stat = 'summary',  fun.y = "mean") + 
  ggtitle('Average Volatility from 2016 to 2019') +
  xlab('Hub') +
  ylab('Averege Volatility')
ggsave(paste0(output_dir,"Average Volatility over years.png"), avg_volatility)

## ----Hourly Shape Profile------------------------------------------------
price_data <- price_data[,`:=`(day_of_week = wday(Date), month = month(Date))]
avg_price_month_day_hr <- price_data[ ,.(Hour_Average =mean(Price)), by = .(SettlementPoint, month, day_of_week, hr)]
normalized_by_hours <- avg_price_month_day_hr[ ,Normalized_Hour_Average := Hour_Average/mean(Hour_Average), by = .(SettlementPoint, month, day_of_week)]
normalized_by_hours <- normalized_by_hours[,.SD, .SDcols = !c('Hour_Average')]

normalized_by_hours_listed <- split(normalized_by_hours, by = "SettlementPoint")

dir.create("all_outputs/hourlyShapeProfiles")
lapply(seq_along(normalized_by_hours_listed), function(i){
  fwrite(normalized_by_hours_listed[[i]], paste0("all_outputs/hourlyShapeProfiles/","profile_",
                                                 names(normalized_by_hours_listed)[i],
                                                 ".csv"))
}
)



