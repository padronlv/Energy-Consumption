
#------------------------libraries, wd and seed-------------------------------------------
#libraries
require(randomForest)
library(rpart.plot)
library(caret)
library(forecast)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
#set wd and seed
setwd("C:/Users/VPL/Desktop/Data Science/Ubiqum/Module 4/Task 1")
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

#data2006 <- filter(data_hec_pre, year(DateTime) == "2006")
#data2007 <- filter(data_hec_pre, year(DateTime) == "2007")
#data2007March <- filter(data_hec_pre, year(DateTime) == "2007", month(DateTime) == "3")
#data2007March1 <- filter(data_hec_pre, year(DateTime) == "2007", month(DateTime) == "3", day(DateTime) == "1")

#-----------------------------Change granularity (15 min)--------------------------
data_hec_pre2 <- mutate(data_hec_pre, DateTime15 = floor_date(DateTime, "15 minutes"))
data_15min <- group_by(data_hec_pre2, DateTime15)
data_15min <- summarize_all(data_15min,funs(mean))
data_15min <- data_15min[,-c(2,3,4,5,8,9)]
data_15min <- mutate(data_15min, WeekdayTime = wday(DateTime15))

#----------------------------Change granularity(1h)----------------
data_1h <- mutate(data_15min, DateTime1h = floor_date(DateTime15, "hour"))
data_1h <- group_by(data_1h, DateTime1h = (DateTime1h))
data_1h <- summarize_all(data_1h,funs(mean))

#-----------------------------Mean weekday-----------------------------------

data_1hW <- filter(data_1h, WeekdayTime %in% 2:6)
data_1hW <- group_by(data_1hW, DateTime1h = hour(DateTime1h))
data_1hW <- summarise_all(data_1hW, funs(mean))

#----------------------------Mean Weekend-day----------------
data_1hWd <- filter(data_1h, WeekdayTime %in% c(1,7))
data_1hWd <- group_by(data_1hWd, DateTime1h = hour(DateTime1h))
data_1hWd <- summarise_all(data_1hWd, funs(mean))


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

#boxplot(data_15min$Global_active_power)
#boxplot(data_15min$Global_reactive_power)
#boxplot((data_15min$Voltage))
#boxplot((data_15min$Sub_metering_1))
#boxplot((data_15min$Sub_metering_2))
#boxplot((data_15min$Sub_metering_3))

#pie chart consumption
dataglobal <- gather(data_15min, electricity_mode, watt_hour, Sub_metering_1, Sub_metering_2, Sub_metering_3, Sub_remaining)
dataglobal <- group_by(dataglobal, electricity_mode)
dataglobal <- summarize_all(dataglobal, funs(mean))
ggplot(data=dataglobal) + geom_count(aes(x = electricity_mode, y = 0, size = watt_hour, color = electricity_mode)) +
  theme_economist() +  scale_colour_economist() + scale_size_area(max_size = 200) +
  theme(legend.position = "none", panel.grid = element_blank(), axis.title= element_blank(),axis.text = element_blank(), axis.ticks = element_blank())

#plot comsumption mean week day
ggplot(data_1hW, aes(x=DateTime1h)) + geom_line(aes(y = Global_active_power))+
  geom_line(aes(y=Sub_metering_1)) +
  geom_line(aes(y=Sub_metering_2))+
  geom_line(aes(y=Sub_metering_3))+
  ggtitle("Consumption in a Mo-Fr Day")+ theme_economist() +  scale_colour_economist()+
  xlab("Time") + ylab("Watt-hour")

#plot consumption mean weekend day
ggplot(data_1hWd, aes(x=DateTime1h)) + geom_line(aes(y = Global_active_power, color = "black"))+
  geom_line(aes(y=Sub_metering_1, color = "red")) +
  geom_line(aes(y=Sub_metering_2, color = "green"))+
  geom_line(aes(y=Sub_metering_3, color = "blue"))+
  ggtitle("Consumption in the Weekend")+ theme_economist() +  scale_colour_economist()+
  xlab("Time") + ylab("Watt-hour")


#plot consupmtion per months for all submeterings
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

ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power))+
        geom_line(aes(y=Sub_metering_1)) +
        geom_line(aes(y=Sub_metering_2))+
        geom_line(aes(y=Sub_metering_3))+
        ggtitle("Consumption per minute")+ theme_economist() +  scale_colour_economist()+
        xlab("Time") + ylab("Watt hour")
}

minutly_consumption_plot(24, 04, 2007)





#consumption in a month
daily_consumption_plot <- function(month1, year1) {
  #plot the average consumption for every day in a month
  data_for_plot <- aggregate(data_15min, by = list (date(data_15min$DateTime)), FUN = mean)
  #print(data_for_plot)
  data_for_plot <- filter(data_for_plot, year(DateTime) == year1)
  data_for_plot <- filter(data_for_plot, month(DateTime) == month1)
  
  
  ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power), color = "blue")+
    geom_line(aes(y=Sub_metering_1), color = "pink")+
    geom_line(aes(y=Sub_metering_2), color = "yellow" )+
    geom_line(aes(y=Sub_metering_3), color = "green")+
    ggtitle("Daily Consumption")+
    xlab("Time") + ylab("Watt hour")
}

daily_consumption_plot(11, 2007)

#consumption in a year
Monthly_consumption_plot <- function(year1) {
  # plot the average consumption for every month in a year
  # mode should be : global, submeters, or all
  data_for_plot <- aggregate(data_15min, by = list (paste(month(data_15min$DateTime)), year(data_15min$DateTime)), FUN = mean)
  data_for_plot <- filter(data_for_plot, year(DateTime) == year1)
  
  
      ggplot(data_for_plot, aes(x=DateTime)) + geom_line(aes(y = Global_active_power), color = "blue")+
        geom_line(aes(y=Sub_metering_1), color = "pink")+
        geom_line(aes(y=Sub_metering_2), color = "yellow" )+
        geom_line(aes(y=Sub_metering_3), color = "green")+
        ggtitle("Consumption 2008")+
        xlab("Time") + ylab("Watt hour")
  }




Monthly_consumption_plot(2009)

data_for_plot <- aggregate(data_15min, by = list (week(data_15min$DateTime), year(data_15min$DateTime)), FUN = mean)
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
  
  data_for_plot <- filter(data_15min, year(DateTime) == year1)
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

#GAP
energytsforecastsGAP <- HoltWinters(energyts_GAP_year)
energytsforecastsGAP
plot(energytsforecastsGAP)
energytsforecastsfutureGAP <- forecast(energytsforecastsGAP, h=12)
plot(energytsforecastsfutureGAP)
autoplot(energytsforecastsfutureGAP)

#GRP
energytsforecastsGRP <- HoltWinters(energyts_GRP_year)
energytsforecastsGRP
plot(energytsforecastsGRP)
energytsforecastsfutureGRP <- forecast(energytsforecastsGRP, h=12)
plot(energytsforecastsfutureGRP)
autoplot(energytsforecastsfutureGRP)

#S1
energytsforecastsS1 <- HoltWinters(energyts_S1_year)
energytsforecastsS1
plot(energytsforecastsS1)
energytsforecastsfutureS1 <- forecast(energytsforecastsS1, h=12)
plot(energytsforecastsfutureS1)
autoplot(energytsforecastsfutureS1)

#s2
energytsforecastsS2 <- HoltWinters(energyts_S2_year)
energytsforecastsS2
plot(energytsforecastsS2)
energytsforecastsfutureS2 <- forecast(energytsforecastsS2, h=12)
plot(energytsforecastsfutureS2)
autoplot(energytsforecastsfutureS2)

#s3
energytsforecastsS3 <- HoltWinters(energyts_S3_year)
energytsforecastsS3
plot(energytsforecastsS3)
energytsforecastsfutureS3 <- forecast(energytsforecastsS3, h=12)
plot(energytsforecastsfutureS3)
autoplot(energytsforecastsfutureS3)






#----------------------------------Machine Learning---------------------------------
data_1year <- filter(data_15min, year(DateTime15) == 2010)
data_1h <- mutate(data_1year, DateTime1h = floor_date(DateTime, "hour"))
data_1h <- group_by(data_1h, DateTime1h)
data_1h <- summarize_all(data_1h,funs(mean))



dataML <- mutate(data_1h, YearTime = as.integer(year(DateTime1h)))
#dataML <- mutate(dataML, MonthTime = as.integer(month(DateTime1h)))
dataML <- mutate(dataML, Quartertime = as.integer(quarter(DateTime1h)))
dataML <- mutate(dataML, WeekdayTime = as.integer(wday(DateTime1h)))
dataML <- mutate(dataML, Week_Weekend = if_else(WeekdayTime %in% c(2,3,4,5,6), "workday", "weekend"))
dataML <- mutate(dataML, HourTime = as.integer(hour(DateTime1h)))
dataML <- dataML[,-c(2, 3, 4, 5, 6)]
dataMLGAP <- dataML[,-c(3:9)]
summary(dataMLGAP)
str(dataMLGAP)


#---------PreTrain-----------
#Datapartition
Data_Partition <- createDataPartition(dataMLGAP$Global_active_power, p = .75, list = FALSE)
training <- dataMLGAP[Data_Partition,]
testing <- dataMLGAP[-Data_Partition,]

#10 fold cross validation
Control_RepeatedCV <- trainControl(method = "cv", number = 5)
#fitControl <- trainControl(method = "cv", number = 10)


#------------------------------------DT----------------------------------------
#train DT
DT <- train(Global_active_power~.-DateTime1h, data = training, method = "rpart", trControl=Control_RepeatedCV, tuneLength = 25)
DT
varImp(DT)
#plot DT

#predictor variables
predictors(DT)

#make predictions
testPredDT <- predict(DT, testing)

#performace measurment
postResample(testPredDT, testing$Global_active_power)

#plot predicted verses actual
plot(testPredDT,testing$Global_active_power)

#plot DT
rpart.plot(DT$finalModel)
#------------------------------------DT----------------------------------------
#train DT
RF <- train(Global_active_power~.-DateTime1h, data = training, method = "rf", importance = TRUE, trControl=Control_RepeatedCV, tuneLength =10)
RF
varImp(RF)

#predictor variables
predictors(RF)

#make predictions
testPredRF <- predict(RF, testing)

#performace measurment
postResample(testPredRF, testing$Global_active_power)

#plot predicted verses actual
plot(testPredRF,testing$Global_active_power)

#------------------------------------XGBM----------------------------------------
#train DT
XGBM <- train(Volume~ PositiveServiceReview + x4StarReviews + x1StarReviews, data = training, method = "xgbTree",
              preprocess = c("center","scale"), trControl=Control_RepeatedCV, tuneLength = 20)
XGBM <- train(Global_active_power~.-DateTime1h, data = training, method = "svmLinear", importance = TRUE, trControl=Control_RepeatedCV, tuneLength =10)
XGBM
varImp(XGBM)

#predictor variables
predictors(XGBM)

#make predictions
testPredXGBM <- predict(XGBM, testing)

#performace measurment
postResample(testPredXGBM, testing$Global_active_power)

#plot predicted verses actual
plot(testPredXGBM,testing$Global_active_power)
