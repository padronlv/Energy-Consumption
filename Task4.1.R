
#------------------------libraries and wd-------------------------------------------
#libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
#set wd
setwd("C:/Users/VPL/Desktop/Data Science/Ubiqum/Module 3/Task 1")

#-----------------import data----------------------------------
data_hec <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

#--------------------------------data exploration and rename variables--------------------
summary(data_hec)
head(data_hec)
str(data_hec)
length(which(data_hec == "?"))
#comment


#-------------------------------NA-----------------------
data_hec_na <- data_hec[complete.cases(data_hec), ]

length(which(is.na(data_hec_na)))
length(which(data_hec_na == "?"))
length(which(data_hec_na == ""))
length(which(data_hec_na == " "))
length(which(data_hec_na == "none"))
head(data_hec)

#-------------------------------------datatime------------------------------------
data_hec_na_datatime <- mutate(data_hec_na, DateTime = paste(Date, Time))
data_hec_na_datatime <- data_hec_na_datatime[,c(ncol(data_hec_na_datatime), 1:(ncol(data_hec_na_datatime)-1))]
head(data_hec_na_datatime)
data_hec_na_datatime$DateTime <- dmy_hms(data_hec_na_datatime$DateTime, tz = "GMT")
data_hec_na_datatime <- mutate(data_hec_na_datatime, weekday = wday(DateTime))
data_hec_na_datatime <- data_hec_na_datatime[, c(1, ncol(data_hec_na_datatime), 2:(ncol(data_hec_na_datatime)-1))]


str(data_hec_na_datatime)

#-------------------------convert variables--------------------------------
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
#------------Rename variable-----------------------

#----------------------------filter--------------------------------------

data2006 <- filter(data_hec_pre, year(DateTime) == "2006")
data2007 <- filter(data_hec_pre, year(DateTime) == "2007")
data2007March <- filter(data_hec_pre, year(DateTime) == "2007", month(DateTime) == "3")
data2007March1 <- filter(data_hec_pre, year(DateTime) == "2007", month(DateTime) == "3", day(DateTime) == "1")


#----------------------------------Plots----------------------------------------
#boxplots
boxplot(data_hec_pre$Global_active_power)
boxplot(data_hec_pre$Global_reactive_power)
boxplot((data_hec_pre$Voltage))
boxplot((data_hec_pre$Sub_metering_1))
boxplot((data_hec_pre$Sub_metering_2))
boxplot((data_hec_pre$Sub_metering_3))
warning
#consumption in a day
minutly_consumption_plot <- function(day1, month1, year1) {
  #plot the consumption for every minute in a day
  data_for_plot <- filter(data_hec_pre, year(DateTime) == year1)
  data_for_plot <- filter(data_for_plot, month(DateTime) == month1)
  data_for_plot <- filter(data_for_plot, day(DateTime) == day1)

ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power, color = "blue"))+
        geom_line(aes(y=Global_reactive_power, color = "red"))+
        geom_line(aes(y=Sub_metering_1, color= "pink")) +
        geom_line(aes(y=Sub_metering_2, color = "yellow" ))+
        geom_line(aes(y=Sub_metering_3, color = "green"))+
        ggtitle("Consumption per minute")+
        xlab("Time") + ylab("Watt hour")
}

minutly_consumption_plot(18, 04, 2007)

#data_for_plot <- aggregate(data_hec_pre, by = list (date(data_hec_pre$DateTime)), FUN = mean)
#length(which(is.na(data_for_plot)))
#data_for_plot <- aggregate(data_hec_pre, by = list (date(data_hec_pre$DateTime)), FUN = mean)
#data_for_plot <- aggregate(data_for_plot, by = list (month(data_for_plot$Group.1)), FUN = mean)

#consumption in a month
daily_consumption_plot <- function(month1, year1) {
  #plot the average consumption for every day in a month
  data_for_plot <- aggregate(data_hec_pre, by = list (date(data_hec_pre$DateTime)), FUN = mean)
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
  data_for_plot <- aggregate(data_hec_pre, by = list (paste(month(data_hec_pre$DateTime)), year(data_hec_pre$DateTime)), FUN = mean)
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

data_for_plot <- aggregate(data_hec_pre, by = list (week(data_hec_pre$DateTime), year(data_hec_pre$DateTime)), FUN = mean)
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
    
data_for_plot <- filter(data_hec_pre, year(DateTime) == 2007)
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

data_for_plot <- data_hec_pre
data_for_plot <- aggregate(data_for_plot, by = list(month(data_for_plot$DateTime), year(data_for_plot$DateTime)), FUN = mean)
data_for_plot <- gather(data_for_plot, electricity_mode, watt_hour, Global_active_power, Global_reactive_power, 
                        a = Sub_metering_1, Sub_metering_2, Sub_metering_3, Sub_remaining)
ggplot() + geom_col(data=data_for_plot, aes(x=factor(month(DateTime)), y = watt_hour, fill = factor(year(DateTime)))) +
  facet_wrap(~electricity_mode, scales = "free_y")+ ggtitle("Monthly Consumption")




  

wardata_month <- aggregate(data_hec_pre, by = list (paste(month(data_hec_pre$DateTime), year(data_hec_pre$DateTime))), FUN = mean)


ggplot(data_month)+ geom_point(aes(x = Group.1, y = Global_active_power))
ggplot(data2006)+ geom_point(aes(x = DateTime, y = Global_active_power))
ggplot(data2007March)+ geom_point(aes(x = DateTime, y = Global_active_power))
ggplot(data2007March1)+ geom_point(aes(x = DateTime, y = Sub_metering_3))




