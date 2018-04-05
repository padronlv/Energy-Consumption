
#------------------------libraries, wd and seed-------------------------------------------
#libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
#set wd and seed
setwd("C:/Users/VPL/Desktop/Data Science/Ubiqum/Module 3/Task 1")
set.seed(123)

#-----------------import data----------------------------------
data_hec <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

#--------------------------------Data exploration--------------------
#data exploration
summary(data_hec)
head(data_hec)
str(data_hec)

#-------------------------------NA-----------------------
length(which(is.na(data_hec)))
length(which(data_hec == "?"))
length(which(data_hec == ""))
length(which(data_hec == " "))
length(which(data_hec == "none"))
head(data_hec)

#Drop rows with NA
data_hec_na <- data_hec[complete.cases(data_hec), ]
length(which(is.na(data_hec_na)))

#-------------------------------------Work with DateTime------------------------------------
data_hec_na_datatime <- mutate(data_hec_na, DateTime = paste(Date, Time))
data_hec_na_datatime <- data_hec_na_datatime[,c(ncol(data_hec_na_datatime), 1:(ncol(data_hec_na_datatime)-1))]
head(data_hec_na_datatime)
data_hec_na_datatime$DateTime <- dmy_hms(data_hec_na_datatime$DateTime, tz = "GMT")
data_hec_na_datatime <- mutate(data_hec_na_datatime, weekday = wday(DateTime))
data_hec_na_datatime <- data_hec_na_datatime[, c(1, ncol(data_hec_na_datatime), 2:(ncol(data_hec_na_datatime)-1))]
str(data_hec_na_datatime)

#-------------------------Convert variables to the right type and right unit--------------------------------
data_hec_pre <- data_hec_na_datatime
data_hec_pre$Global_active_power <- as.numeric(data_hec_na_datatime$Global_active_power)
data_hec_pre$Global_reactive_power <- as.numeric(data_hec_na_datatime$Global_reactive_power)
data_hec_pre$Voltage <- as.numeric(data_hec_na_datatime$Voltage)
data_hec_pre$Global_intensity <- as.numeric(data_hec_na_datatime$Global_intensity)
data_hec_pre$Sub_metering_1 <- as.numeric(data_hec_na_datatime$Sub_metering_1)
data_hec_pre$Sub_metering_2 <- as.numeric(data_hec_na_datatime$Sub_metering_2)

data_hec_pre$Global_active_power <- data_hec_pre$Global_active_power * 100 / 6
data_hec_pre$Global_reactive_power <- data_hec_pre$Global_reactive_power * 100 / 6
data_hec_pre$Sub_remaining <- data_hec_pre$Global_active_power - data_hec_pre$Sub_metering_1 - data_hec_pre$Sub_metering_2 - data_hec_pre$Sub_metering_3

str(data_hec_pre)
summary(data_hec_pre)

#----------------------------Filter by year and month--------------------------------------

data2006 <- filter(data_hec_pre, year(DateTime) == "2006")
data2007 <- filter(data_hec_pre, year(DateTime) == "2007")
data2007March <- filter(data_hec_pre, year(DateTime) == "2007", month(DateTime) == "3")
data2007March1 <- filter(data_hec_pre, year(DateTime) == "2007", month(DateTime) == "3", day(DateTime) == "1")

#-----------------------------Change granularity (15 min)--------------------------
data_hec_pre2 <- mutate(data_hec_pre, DateTime15 = floor_date(DateTime, "15 minutes"))
data_15min <- group_by(data_hec_pre2, DateTime15)
data_15min <- summarize_all(data_15min,funs(mean))
#--------------------------------------Plots-------------------------------------------
#boxplots
ggplot(data=data_15min) + geom_boxplot(aes(x= "Global_active_power", y = Global_active_power))+ 
  geom_boxplot(aes(x= "Global_reactive_power", y = Global_reactive_power))+
  theme_economist() +
  scale_fill_economist()
ggplot(data=data_15min) + geom_line(aes(x= DateTime15, y = Global_active_power))


  
#boxplot(data_15min$Global_active_power)
#boxplot(data_15min$Global_reactive_power)
#boxplot((data_15min$Voltage))
#boxplot((data_15min$Sub_metering_1))
#boxplot((data_15min$Sub_metering_2))
#boxplot((data_15min$Sub_metering_3))
warning
#consumption in a day
minutly_consumption_plot <- function(day1, month1, year1) {
  #plot the consumption for every minute in a day
  data_for_plot <- filter(data_15min, year(DateTime) == year1)
  data_for_plot <- filter(data_for_plot, month(DateTime) == month1)
  data_for_plot <- filter(data_for_plot, day(DateTime) == day1)

ggplot(data_for_plot, aes(x=DateTime)) + geom_point(aes(y = Global_active_power), color = "blue")+
        geom_point(aes(y=Global_reactive_power), color = "red")+
        geom_line(aes(y=Sub_metering_1), color= "pink") +
        geom_line(aes(y=Sub_metering_2), color = "yellow" )+
        geom_line(aes(y=Sub_metering_3), color = "green")+
        ggtitle("Consumption per minute")+
        xlab("Time") + ylab("Watt hour")
}

minutly_consumption_plot(24, 04, 2007)

#data_for_plot <- aggregate(data_hec_pre, by = list (date(data_hec_pre$DateTime)), FUN = mean)
#length(which(is.na(data_for_plot)))
#data_for_plot <- aggregate(data_hec_pre, by = list (date(data_hec_pre$DateTime)), FUN = mean)
#data_for_plot <- aggregate(data_for_plot, by = list (month(data_for_plot$Group.1)), FUN = mean)

#consumption in a month
daily_consumption_plot <- function(month1, year1) {
  #plot the average consumption for every day in a month
  data_for_plot <- aggregate(data_15min, by = list (date(data_hec_pre$DateTime)), FUN = mean)
  #print(data_for_plot)
  data_for_plot <- filter(data_for_plot, year(DateTime) == year1)
  data_for_plot <- filter(data_for_plot, month(DateTime) == month1)
  
  
  ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power), color = "blue")+
    geom_line(aes(y=Global_reactive_power), color = "red")+
    geom_line(aes(y=Sub_metering_1), color = "pink")+
    geom_line(aes(y=Sub_metering_2), color = "yellow" )+
    geom_line(aes(y=Sub_metering_3), color = "green")+
    ggtitle("Daily Consumption")+
    xlab("Time") + ylab("Watt hour")
}

daily_consumption_plot(03, 2007)

#consumption in a year
Monthly_consumption_plot <- function(year1) {
  # plot the average consumption for every month in a year
  # mode should be : global, submeters, or all
  data_for_plot <- aggregate(data_15min, by = list (paste(month(data_hec_pre$DateTime)), year(data_hec_pre$DateTime)), FUN = mean)
  data_for_plot <- filter(data_for_plot, year(DateTime) == year1)
  
  
      ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power), color = "blue")+
        geom_line(aes(y=Global_reactive_power), color = "red")+
        geom_line(aes(y=Sub_metering_1), color = "pink")+
        geom_line(aes(y=Sub_metering_2), color = "yellow" )+
        geom_line(aes(y=Sub_metering_3), color = "green")+
        ggtitle("Consumption 2008")+
        xlab("Time") + ylab("Watt hour")
  }




Monthly_consumption_plot(2008)

data_for_plot <- aggregate(data_15min, by = list (week(data_hec_pre$DateTime), year(data_hec_pre$DateTime)), FUN = mean)
weekly_consumption_plot <- function(mode="all") {
  # plot the average consumption for every month in a year
  # mode should be : global, submeters, or all
  data_for_plot <- aggregate(data_hec_pre, by = list (week(data_hec_pre$DateTime), year(data_hec_pre$DateTime)), FUN = mean)
 
  
 
    ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power), color = "blue")+
      geom_point(aes(y=Global_reactive_power), color = "red")+
      geom_point(aes(y=Sub_metering_1), color = "pink")+
      geom_point(aes(y=Sub_metering_2), color = "yellow" )+
      geom_point(aes(y=Sub_metering_3), color = "green")+
      ggtitle("Daily Consumption")+
      xlab("Time") + ylab("Watt hour")
}

weekly_consumption_plot()    
    
data_for_plot <- filter(data_15min, year(DateTime) == 2007)
data_for_plot <- aggregate(data_for_plot, by = list(month(data_for_plot$DateTime)), FUN = mean)
data_for_plot <- gather(data_for_plot, electricity_mode, watt_hour, Global_active_power, Global_reactive_power, Sub_metering_1, Sub_metering_2, Sub_metering_3, Sub_remaining)


Monthly_consumption_bar <- function(year1) {
  
  data_for_plot <- filter(data_hec_pre, year(DateTime) == year1)
  data_for_plot <- aggregate(data_for_plot, by = list(month(data_for_plot$DateTime)), FUN = mean)
  data_for_plot <- gather(data_for_plot, electricity_mode, watt_hour, Global_active_power, Global_reactive_power, Sub_metering_1, Sub_metering_2, Sub_metering_3)
  ggplot() +
    geom_col(data = data_for_plot, aes(x= electricity_mode, y = watt_hour, fill= factor(month(DateTime))))  
  
}

Monthly_consumption_bar(2008)

data_for_plot <- data_15min
data_for_plot <- aggregate(data_for_plot, by = list(month(data_for_plot$DateTime), year(data_for_plot$DateTime)), FUN = mean)
data_for_plot <- gather(data_for_plot, electricity_mode, watt_hour, Global_active_power, Global_reactive_power, 
                        a = Sub_metering_1, Sub_metering_2, Sub_metering_3, Sub_remaining)
ggplot() + geom_col(data=data_for_plot, aes(x=factor(month(DateTime)), y = watt_hour, fill = factor(year(DateTime))))+ theme_economist() + 
  scale_fill_economist()+
  facet_wrap(~electricity_mode, scales = "free_y")+ ggtitle("Monthly Consumption") + theme(legend.position="right")




ggplot(data_month)+ geom_point(aes(x = Group.1, y = Global_active_power))
ggplot(data2006)+ geom_point(aes(x = DateTime, y = Global_active_power)) + scale_color_brewer(palette="Dark2")
ggplot(data2007March)+ geom_point(aes(x = DateTime, y = Global_active_power))
ggplot(data2007March1)+ geom_point(aes(x = DateTime, y = Sub_metering_3))

#---------------------------Time Series--------------------------------------------

#Period Year
energy_pyear <- function(feature) {
  x = as.character(feature)
  crazystuff = paste0("energy_month$", x)
  energy_month <- group_by(data_15min, year(DateTime15), month(DateTime15), day(DateTime15))
  energy_month <- summarize_all(energy_month,funs(mean))
  
  energytimeseries_year <- ts(crazystuff, frequency=12, start=c(2006, 12))
  
}



energytimeseries_year <- energy_pyear("Global_active_power")

plot.ts(energytimeseries_year)
energytimeseries_year

energytimeseriescomponents <- decompose(energytimeseries)
plot(energytimeseriescomponents)

#Period week
energy2008 <- filter(data_15min, year(DateTime15) == 2009)
energy_week2008 <- group_by(energy2008, year(DateTime15), month(DateTime15), day(DateTime15))
energy_week2008 <- summarize_all(energy_week2008,funs(mean))
energytimeseries_week <- ts(energy_week2008$Global_active_power, frequency=7, start=1, end=12)
plot.ts(energytimeseries_week)
energytimeseries_week

energytimeseries_week_components <- decompose(energytimeseries_week)
plot(energytimeseries_week_components)
energytimeseries_week_components$seasonal
