
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
data_hec_na_datatime <- mutate(data_hec_na_datatime, weekday = format(wday(DateTime)))
data_hec_na_datatime <- data_hec_na_datatime[, c(1, ncol(data_hec_na_datatime), 2:(ncol(data_hec_na_datatime)-1))]
summary(data_hec_na_datatime)
unique(data_hec_na_datatime$weekday)
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

#---------------------------------------weekday/weekend day----------------------------
energywd <- group_by(data_15min, daysea= quarter(DateTime15), daywe= wday(DateTime15))
energywd <- summarize_all(energywd, funs(mean))

energywd <- mutate(energywd, wd = which())


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


data_for_plot_wd <- gather(energywd, electricity_mode, watt_hour, Global_active_power, Global_reactive_power, 
                        Sub_metering_1, Sub_metering_2, Sub_metering_3, Sub_remaining)
ggplot() + geom_col(data=data_for_plot_wd, aes(x=factor(daywe), y = watt_hour, fill = factor(daysea)), position="dodge")+
  theme_economist() +  scale_fill_economist()+
  facet_wrap(~electricity_mode, scales = "free_y")+ ggtitle("Monthly Consumption") + theme(legend.position="right")

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
data_for_plot <- gather(data_for_plot, electricity_mode, watt_hour, Global_active_power,
                        Global_reactive_power, Sub_metering_1, Sub_metering_2, Sub_metering_3, Sub_remaining)


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

#function period Year
energy_pyear <- function(feature) {
  energy_month <- group_by(data_15min, year(DateTime15), month(DateTime15))
  energy_month <- summarize_all(energy_month,funs(mean))
  return(ts(energy_month[feature], frequency=12, start=c(2006, 12)))
}

#apply function period year
energyts_GAP_year <- energy_pyear("Global_active_power")
energyts_GAP_year
plot.ts(energyts_GAP_year)
energyts_GRP_year <- energy_pyear("Global_reactive_power")
plot.ts(energyts_GRP_year)
energyts_S1_year <- energy_pyear("Sub_metering_1")
plot.ts(energyts_S1_year)
energyts_S2_year <- energy_pyear("Sub_metering_2")
plot.ts(energyts_S2_year)
energyts_S3_year <- energy_pyear("Sub_metering_3")
plot.ts(energyts_S3_year)
energyts_SR_year <- energy_pyear("Sub_remaining")
plot.ts(energyts_SR_year)

#energytimeseriescomponents <- decompose(energytimeseries)
#plot(energytimeseriescomponents)

#function period week
energy_pweek <- function(feature, yearf) {
  energy_year <- filter(data_15min, year(DateTime15) == yearf)
  energy_weekyear <- group_by(energy_year, year(DateTime15), month(DateTime15), day(DateTime15))
  energy_weekyear <- summarize_all(energy_weekyear,funs(mean))
  return(ts(energy_weekyear[feature], frequency=7, start=c(1, 1)))
}

#apply function period week
energyts_GAP_week2008 <- energy_pweek("Global_active_power", 2008)
energyts_GAP_week2008
plot.ts(energyts_GAP_week2008)
energyts_GRP_week2008 <- energy_pweek("Global_reactive_power", 2008)
plot.ts(energyts_GRP_week2008)
energyts_S1_week2008 <- energy_pweek("Sub_metering_1", 2008)
plot.ts(energyts_S1_week2008)
energyts_S2_week2008 <- energy_pweek("Sub_metering_2", 2008)
plot.ts(energyts_S2_week2008)
energyts_S3_week2008 <- energy_pweek("Sub_metering_3", 2008)
plot.ts(energyts_S3_week2008)
energyts_SR_week2008 <- energy_pweek("Sub_remaining", 2008)
plot.ts(energyts_SR_week2008)




#energytimeseries_week_components <- decompose(energytimeseries_week)
#plot(energytimeseries_week_components)
#energytimeseries_week_components$seasonal
