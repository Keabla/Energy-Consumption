#install and load libraries necessary to analyze the dataset
pacman::p_load(lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,edeaR,doParallel,urca,forecast)

#set global path
setwd("C:/Users/Martin Albaek/Documents/coding academy/Ubiqum/Course/Module 3/Task 1")
getwd()

#load energy consumtpion dataset
#EnCon2 <- read.csv("household_power_consumption.txt",header = TRUE,sep = ";")
EnCon <- read.table("household_power_consumption.txt",header = TRUE,sep = ";",na.strings = "?")

#Why does the number of missing values change from EnCon to Encon1, is it because in Enco1 there are blank spaces?

#check the dimension of the dataset to be sure that the importing was a success
dim(EnCon)
str(EnCon)

#change name for submeters 1,2 and 3 respectively for kitchen, laundry room and 
#for heating (electric water-heater and an air-conditioner), and replacing the other names with shorter ones
colnames(EnCon) <- c("Date","Time","Active_Power","Reactive_Power","Voltage","Intensity","Kitchen","Laundry_Room","Heating")

#change scale of kitchen, laundry room and for heating
EnCon$Kitchen <- EnCon$Kitchen/1000*60
EnCon$Laundry_Room <- EnCon$Laundry_Room/1000*60 
EnCon$Heating <- EnCon$Heating/1000*60 

#creating new variable for all heating except Kitchen, Laundry and Heating
EnCon <- EnCon %>% mutate(Others = Active_Power - Kitchen - Laundry_Room - Heating)

#combine the time clomns and convert the new one to the date format
EnCon1 <- EnCon %>% unite(DateTime,Date:Time,sep = " ",remove = TRUE)
EnCon1$DateTime <- strptime(EnCon1$DateTime, "%d/%m/%Y %H:%M:%S")
EnCon2 <- EnCon1

#create new column to check missing dates and check how many of them are there (function returns "TRUE" if 
#between dates there are more than 1 minute missing)
EnCon1$missingDates <- c(1, diff(as.POSIXct(EnCon1$DateTime, format="%d/%m/%Y %H:%M:%S", tz="UTC"))) > 1
sum(EnCon1$missingDates == TRUE)

#statistics of NAs

stat <- skim_to_wide(EnCon)
stat1 <- skim_to_wide(EnCon1)
StatNAs <- statsNA(EnCon$Active_Power)

#Visualize missing values
#ma.kalman to substitute it with interpolation #pad package

#check any missing values 
sum(is.na(EnCon1$DateTime))
summary(EnCon)
colSums(is.na(EnCon1))

EnCon1$DateTime <- as.POSIXct(EnCon1$DateTime) 

#Plot Active Power to get an idea of which are the dates in which data is missing
EnCon2 <- EnCon1

EnCon2 <- EnCon2[,2:ncol(EnCon2)]
EnCon2[is.na(EnCon2)] <- 0
EnCon2$DateTime <- EnCon1$DateTime


ggplot(EnCon2, aes_string(x = "DateTime", y = "Active_Power")) +
  geom_bar(stat="identity") + xlab("Energy Consumption per Minute") + ylab("Active_Power") + 
  ggtitle(paste("Weekly En Con by hour","for","Active_Power",sep = " ")) +
  scale_x_datetime(labels = date_format("%A %H"))

#replace the missing values with the average 

#which rows have missing values
Row_NA <- which(is.na(EnCon1$Active_Power))

#Check current Time zone 
#Sys.timezone()

#Create new dataframe with a new column without the years, only month, day, hour, minute and second
EnCon1_NA <- EnCon1 %>% mutate(Date_WYear = format(EnCon1$DateTime, format("%m-%d %H:%M:%S")))

#Drop DateTime and missing Dates so to have a dataframe of same width of the one in which we will retrieve the data
#For missing values
EnCon1_NA$DateTime <- NULL
EnCon1_NA$missingDates <- NULL

#reorder columns with Date_WYear being first
EnCon1_NA <- EnCon1_NA %>% select(Date_WYear, everything()) 

#Creation of the dataframe in which we will retrieve the data for the missing values, by avering the values
#for each category of electricity for each moment of the year for all the duration that we have available for 
#the dataset

EnCon_NA <- na.omit(EnCon1_NA)

EnCon_NA <- EnCon_NA %>% group_by(Date_WYear) %>%
  summarize(Active_Power = mean(Active_Power),
            Reactive_Power = mean(Reactive_Power),
            Voltage = mean(Voltage),
            Intensity = mean(Intensity),
            Kitchen = mean(Kitchen),
            Laundry_Room = mean(Laundry_Room),
            Heating = mean(Heating),
            Others = mean(Others)) 

#check for the rows that interest us
EnCon_NAR <- EnCon1_NA %>% group_by(Date_WYear) %>%
  summarize(Active_Power = mean(Active_Power))

EnCon_NAR <- EnCon_NAR[2:nrow(EnCon_NAR),]

Row_NA1 <- which(is.na(EnCon_NAR$Active_Power))

#keep only the selected rows (useful ones) from the dataframe in which we retrieve information
EnCon_NA <- EnCon_NA[Row_NA1,] 

#substitute each row in which there are missing values with the the average of the corresponding  
#%m%d %H%M%S
for (i in Row_NA) {
  EnCon1_NA[i,] <- EnCon_NA %>% filter(Date_WYear == EnCon1_NA[i,]$Date_WYear)
}

EnCon1_NA$DateTime <- EnCon1$DateTime
EnCon1_NA$Date_WYear <- NULL

#Plot missing data prediction 


ggplot(EnCon1_NA[Row_NA,], aes_string(x = "DateTime", y = "Active_Power")) +
  geom_point(stat = "identity") + xlab("Energy Consumption per Minute") + ylab("Active_Power") + 
  ggtitle(paste("Missing Data","for","Active_Power",sep = " ")) +
  scale_x_datetime(labels = date_format("%d/%m/%y"))


EnCon_1 <- EnCon1_NA 

sum(is.na(EnCon_1$Active_Power))

#create new columns with date having last unit hour,day,week,month,year as unit and create a dataframe for each of these unit 
#and collapse the data for each dataframe corresping to its unit

TimeClass <- c("hour","day","week","month","year","season")
NewDataFrame <- c()
EnCon1Full <- EnCon_1
DFName <- c()

#maybe do mean for Voltage and Intensity

for (i in TimeClass) {
  EnCon1Full <- EnCon1Full %>% mutate(date_by_TimeClass = floor_date(DateTime,unit = i))
  NewDataFrame <- EnCon1Full %>% group_by(date_by_TimeClass) %>% summarize(Obs = n(),
                                                                           Active_Power = sum(Active_Power),
                                                                           Reactive_Power = sum(Reactive_Power),
                                                                           Voltage = mean(Voltage),
                                                                           Intensity = mean(Intensity),
                                                                           Kitchen = sum(Kitchen),
                                                                           Laundry_Room = sum(Laundry_Room),
                                                                           Heating = sum(Heating),
                                                                           Others = sum(Others))
  DTName <- paste("EnCon","by",i,sep = "_")
  colnames(NewDataFrame) <- gsub("TimeClass",i,colnames(NewDataFrame))
  assign(DTName[1],NewDataFrame)
  colnames(EnCon1Full) <- gsub("TimeClass",i,colnames(EnCon1Full))
}

colNames <- colnames(EnCon1)[2:9]

#Further Data Exploration
EnCon_by_day$date_by_day <- as.POSIXct(EnCon_by_day$date_by_day)

Test <- subset(EnCon_by_day, year(EnCon_by_day$date_by_day)== 2008 & month(EnCon_by_day$date_by_day)== c(8))

scale_x_datetime(labels = date_format("%d/%m/%y %A"))

#check for holidays
Holidays <- EnCon_by_day %>% group_by(year(date_by_day),month(date_by_day)) %>% filter(Kitchen == 0) %>% 
  summarise(Active_Power=sum(Active_Power),
            H_Days = n())  

#Holidays <- cbind(Holidays,paste(Holidays$Year,Holidays$Month),stringsAsFactors = FALSE)
#rename columns and 
colnames(Holidays) <- c("Year","Month","Active_P","HDays")
Holidays_TS <- ts(Holidays,frequency = 12, start = c(2006,12))

plot.ts(Holidays_TS[,"Active_P"])
plot.ts(Holidays_TS[,"HDays"])
plot(Holidays$HDays, type="l")


### Time Series ###

#transform weekly, monthly and seasonal dataframes in time series
EnCon_by_week1 <- na.omit(EnCon_by_week)
EnCon_by_week1$date_by_week <- NULL
EnCon_by_week1 <- EnCon_by_week1[2:(nrow(EnCon_by_week1)-1),]
EnCon_by_week1$Obs <- NULL
EnCon_TSW <- ts(EnCon_by_week1, frequency = 52, start = decimal_date(ymd("2006,12,17")))

EnCon_by_month1 <- na.omit(EnCon_by_month)
EnCon_by_month1$date_by_month <- NULL
EnCon_by_month1 <- EnCon_by_month1[2:(nrow(EnCon_by_month1)-1),]
EnCon_by_month1$Obs <- NULL
EnCon_TSM <- ts(EnCon_by_month1, frequency = 12, start = c(2007,01))

EnCon_by_season1 <- na.omit(EnCon_by_season)
EnCon_by_season1$date_by_season <- NULL
EnCon_by_season1 <- EnCon_by_season1[2:(nrow(EnCon_by_season1)-1),]
EnCon_by_season1$Obs <- NULL
EnCon_TSS <- ts(EnCon_by_season1, frequency = 4, start = c(2007,01))


##forecasting trend ##
F_TSW <- forecast(EnCon_TSW[,"Active_Power"], h = 52, level = 0)
F_TSM <- forecast(EnCon_TSM[,"Active_Power"], h = 12, level = 0)
F_TSS <- forecast(EnCon_TSS[,"Active_Power"], h = 4, level = 0)

TSLM_W <- tslm(Active_Power~season+trend,Train_TSW) 
F_TSLM_W <- forecast(TSLM_W,h=52, level = 0)
TSLM_M <- tslm(Active_Power~season+trend,Train_TSM)
F_TSLM_M <- forecast(TSLM_M,h=12, level = 0)
TSLM_S <- tslm(Active_Power~season+trend,EnCon_TSS)
F_TSLM_S <- forecast(TSLM_S, h = 4, level = 0)

#plot the forecastings
autoplot(F_TSW)
autoplot(F_TSM)
autoplot(F_TSS)

autoplot(F_TSLM_W)
autoplot(F_TSLM_M)
autoplot(F_TSLM_S)

#Prediction tables
Prediction_Table_W <- cbind(summary(F_TSW)[,1],summary(F_TSLM_W)[,1])
colnames(Prediction_Table_W) <- c("Forecast Model","tslm Model")

Prediction_Table_M <- cbind(summary(F_TSM)[,1],summary(F_TSLM_M)[,1])
colnames(Prediction_Table_M) <- c("Forecast Model","tslm Model")

Prediction_Table_S <- cbind(summary(F_TSS)[,1],summary(F_TSLM_S)[,1])
colnames(Prediction_Table_S) <- c("Forecast Model","tslm Model")

#Compare models for different time series
autoplot(EnCon_TSW[,"Active_Power"]) +
  autolayer(F_TSW,series = "Forecast Model") +
  autolayer(F_TSLM_W, series = "tslm Model")

autoplot(EnCon_TSM[,"Active_Power"]) +
  autolayer(F_TSM,series = "Forecast Model") +
  autolayer(F_TSLM_M, series = "tslm Model")

autoplot(EnCon_TSS[,"Active_Power"]) +
  autolayer(F_TSS,series = "Forecast Model") +
  autolayer(F_TSLM_S, series = "tslm Model")

## Decompose ##
autoplot(stl(EnCon_TSW[,"Active_Power"], "periodic"))
autoplot(stl(EnCon_TSM[,"Active_Power"], "periodic"))
autoplot(stl(EnCon_TSS[,"Active_Power"], "periodic"))

summary(stl(EnCon_TSW[,"Active_Power"], "periodic"))
summary(stl(EnCon_TSM[,"Active_Power"], "periodic"))
summary(stl(EnCon_TSS[,"Active_Power"], "periodic"))

## Train Seasonal Models ##

#create train and test set  for each time series
Train_TSW <- window(EnCon_TSW, start = decimal_date(ymd("2006,12,17")), end = decimal_date(ymd("2009,12,27")))
Test_TSW <- window(EnCon_TSW, start = decimal_date(ymd("2010,01,03")), end = decimal_date(ymd("2010,10,31")))

Train_TSM <- window(EnCon_TSM, start = c(2007, 01), end = c(2009,12))
Test_TSM <- window(EnCon_TSM, start = c(2010, 01), end = c(2010,10))

Train_TSS <- window(EnCon_TSS, start = c(2007, 01), end = c(2009,04))
Test_TSS <- window(EnCon_TSS, start = c(2010, 01), end = c(2010,02))


# Holt Models #
H_TSW <- HoltWinters(Train_TSW[,"Active_Power"]) 
H_TSW_FOR <- predict(H_TSW, n.ahead = 43, prediction.interval = TRUE)

autoplot(H_TSW_FOR[,1], series = "Forecast") +
  autolayer(Train_TSW[,"Active_Power"],series = "Train") +
  autolayer(Test_TSW[,"Active_Power"], series = "Test")

H_TSM <- HoltWinters(Train_TSM[,"Active_Power"]) 
H_TSM_FOR <- predict(H_TSM, n.ahead = 10, prediction.interval = TRUE)

autoplot(H_TSM_FOR[,1], series = "Forecast") +
  autolayer(Train_TSM[,"Active_Power"],series = "Train") +
  autolayer(Test_TSM[,"Active_Power"], series = "Test")

H_TSS <- HoltWinters(Train_TSS[,"Active_Power"]) 
H_TSS_FOR <- predict(H_TSS, n.ahead = 2, prediction.interval = TRUE)

autoplot(H_TSS_FOR[,1], series = "Forecast") +
  autolayer(Train_TSS[,"Active_Power"],series = "Train") +
  autolayer(Test_TSS[,"Active_Power"], series = "Test")

#check accuracy
accuracy(H_TSW_FOR,Test_TSW[,1])
accuracy(H_TSM_FOR,Test_TSM[,1])
accuracy(H_TSS_FOR,Test_TSS[,1])


# Arima Model #
AR_TSW <- auto.arima(Train_TSW[,"Active_Power"], seasonal = TRUE) 
AR_TSM <- auto.arima(Train_TSM[,"Active_Power"], seasonal = TRUE) 
AR_TSS <- auto.arima(Train_TSS[,"Active_Power"], seasonal = TRUE) 

#AR_TSW1 <- Arima(Train_TSW[,"Active_Power"],c(1.5,1.5,0),c(1,1,0))

AR_TSW_FOR <- forecast(AR_TSW,43)
AR_TSM_FOR <- forecast(AR_TSM,10)
AR_TSS_FOR <- forecast(AR_TSS,2)

#Plot the results

autoplot(AR_TSW_FOR[,1], series = "Forecast") +
  autolayer(Train_TSW[,"Active_Power"],series = "Train") +
  autolayer(Test_TSW[,"Active_Power"], series = "Test")


#AR_TSW_FOR1 <- forecast(AR_TSW1,43)

accuracy(AR_TSW_FOR, Test_TSW[,1])
accuracy(AR_TSM_FOR, Test_TSM[,1])
accuracy(AR_TSS_FOR, Test_TSS[,1])

#accuracy(AR_TSW_FOR1, Test_TSW[,1])

# SNaive #
S_TSW_FOR <- snaive(Train_TSW[,"Active_Power"], h = 43, level = 0.005)
S_TSM_FOR <- snaive(Train_TSM[,"Active_Power"], h = 10, level = 0.005)
S_TSS_FOR <- snaive(Train_TSS[,"Active_Power"], h = 2)

autoplot(S_TSW_FOR) +
  autolayer(Test_TSW[,1], series = "Test")

accuracy(S_TSM_FOR, Test_TSM[,1])

#Plot for the presentation

autoplot(H_TSW_FOR[,1], series = "Holt-Winter") +
  autolayer(Train_TSW[,"Active_Power"], series = "Past Prediction") +
  autolayer(Test_TSW[,"Active_Power"], series = "Actual Prediction") +
  #autolayer(S_TSW_FOR, series = "Seasonal Naive") +
  #autolayer(F_TSLM_W, series = "Linear Model") + 
  ggtitle("Model Selection") +
  labs(y = "Monthly Energy Consumption")

autoplot(S_TSM_FOR, series = "Seasonal Naive") +
  autolayer(Train_TSM[,"Active_Power"], series = "Past Prediction") +
  autolayer(Test_TSM[,"Active_Power"], series = "Actual Prediction") +
  ggtitle("Seasonal Naive") +
  labs(y = "Monthly Energy Consumption")


#check whether stationaty or not
EnCon_TSW %>% ur.kpss() %>% summary()

EnCon_TSW %>% diff(lag= 52) %>% diff() %>% ur.kpss() %>% summary()
