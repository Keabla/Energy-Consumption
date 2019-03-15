#install and load libraries necessary to analyze the dataset
pacman::p_load(lubridate,dplyr,tidyr,skimr,ggplot2,scales,fracdiff,imputeTS,edeaR,doParallel)

#set global path
setwd("C:/Users/Martin Albaek/Documents/coding academy/Ubiqum/Course/Module 3/Task 1")
getwd()

#load energy consumtpion dataset
#EnCon2 <- read.csv("household_power_consumption.txt",header = TRUE,sep = ";")
EnCon <- read.table("household_power_consumption.txt",header = TRUE,sep = ";",na.strings = "?")

#Why does the number of missing values change from EnCon to Encon1, is it because in Enco1 there are blank spaces?

#check the dimension of the dataset to be sure that the importing was a success
dim(EnCon)
str(EnCon2)

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

#statistics of NAs

stat <- skim_to_wide(EnCon)
stat1 <- skim_to_wide(EnCon1)
StatNAs <- statsNA(EnCon$Active_Power)

#Visualize missing values
#ma.kalman to substitute it with interpolation #pad package


#check any missing values 
sum(is.na(EnCon1))
summary(EnCon)
colSums(is.na(EnCon))

#replace the missing values with the average 
vloop <- c(2:ncol(EnCon1))

for (i in vloop) {
  EnCon1[is.na(EnCon1[,i]), i] <- mean(EnCon1[,i], na.rm = TRUE)
}

#create new columns with date having last unit hour,day,week,month,year as unit and create a dataframe for each of these unit 
#and collapse the data for each dataframe corresping to its unit

EnCon1$DateTime <- as.POSIXct(EnCon1$DateTime) 

TimeClass <- c("hour","day","week","month","year","season")
NewDataFrame <- c()
EnCon1Full <- EnCon1
DFName <- c()

#maybe do mean for Voltage and Intensity

for (i in TimeClass) {
  EnCon1Full <- EnCon1Full %>% mutate(date_by_TimeClass = floor_date(DateTime,unit = i))
  NewDataFrame <- EnCon1Full %>% group_by(date_by_TimeClass) %>% summarize(Obs = n(),
                                                                           Active_Power = sum(Active_Power),
                                                                      Reactive_Power = sum(Reactive_Power),
                                                                      Voltage = sum(Voltage),
                                                                      Intensity = sum(Intensity),
                                                                      Kitchen = sum(Kitchen),
                                                                      Laundry_Room = sum(Laundry_Room),
                                                                      Heating = sum(Heating),
                                                                      Others = sum(Others))
  DTName <- paste("EnCon","by",i,sep = "_")
  TimeClass <- as.POSIXct(TimeClass)
  colnames(NewDataFrame) <- gsub("TimeClass",i,colnames(NewDataFrame))
  assign(DTName[1],NewDataFrame)
  colnames(EnCon1Full) <- gsub("TimeClass",i,colnames(EnCon1Full))
  }

colNames <- colnames(EnCon1)[2:9]


#plotting per hour for a day
for (i in colNames) {
  plot <- ggplot(EnCon_by_hour, aes_string(x = "date_by_hour", y = i)) +
    geom_line() + xlab("Energy Consumption per Hour") + ylab(i) + 
    ggtitle(paste("Daily En Con by hour","for",i,sep = " ")) +
    scale_x_datetime(labels = date_format("%d/%m %H"),
                     limit=c(as.POSIXct("2009-04-01 02:00:00"),as.POSIXct("2009-04-02 01:00:00")))
  ggsave(plot, file=paste("Daily_Energy_Consumption_by_hour_","for_",i,".pdf",sep = ""))
}

#plotting per hour for a week
for (i in colNames) {
  plot <- ggplot(EnCon_by_hour, aes_string(x = "date_by_hour", y = i)) +
    geom_line() + xlab("Energy Consumption per Hour") + ylab(i) +
    ggtitle(paste("Weekly En Con by Hour","for",i,sep = " "))
    scale_x_datetime(labels = date_format("%A %H"),
                     limit=c(as.POSIXct("2009-03-31 02:00:00"),as.POSIXct("2009-04-06 01:00:00")))
  ggsave(plot, file=paste("Weekly_Energy_Consumption_by_hour_","for_",i,".pdf",sep = ""))
}

#plotting per day for a week
for (i in colNames) {
  plot <- ggplot(EnCon_by_day, aes_string(x = "date_by_day", y = i)) +
    geom_line() + xlab("Energy Consumption per Day") + ylab(i) +
    ggtitle(paste("Weekly En Con by Day","for",i,sep = " "))
    scale_x_datetime(labels = date_format("%A"),
                     limits = (as.POSIXct("2009-03-31"),as.POSIXct("2009-04-06")))
  ggsave(plot, file=paste("Weekly_Energy_Consumption_by_day_","for_",i,".pdf",sep = ""))
}

#plotting per week for year
for (i in colNames) {
  plot <- ggplot(EnCon_by_week, aes_string(x = "date_by_week", y = i)) +
    geom_line() + xlab("Yearly Energy Consumption per Week") + ylab(i) +
    ggtitle(paste("Yearly En Con by Week","for",i,sep = " "))
    scale_x_datetime(labels = date_format("%d/%m"),
                     limits = c(as.POSIXct(strptime("2008-12-28","2010-01-03"),format = "%Y-%m-%d")))
  ggsave(plot, file=paste("Yearly_Energy_Consumption_by_week_","for_",i,".pdf",sep = ""))
}


#test

#equisse

#problem with laundry room