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
colSums(is.na(EnCon))

EnCon1$DateTime <- as.POSIXct(EnCon1$DateTime) 

#Plot Active Power to get an idea of which are the dates in which data is missing
ggplot(EnCon1, aes_string(x = "DateTime", y = "Active_Power")) +
  geom_bar() + xlab("Energy Consumption per Minute") + ylab("Active_Power") + 
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
                                                                      Voltage = sum(Voltage),
                                                                      Intensity = sum(Intensity),
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

#plotting per hour for a day
for (i in colNames) {
  plot <- ggplot(EnCon_by_hour, aes_string(x = "date_by_hour", y = i)) +
    geom_line() + xlab("Energy Consumption per Hour") + ylab(i) + 
    ggtitle(paste("Daily En Con by hour","for",i,sep = " ")) +
    scale_x_datetime(labels = date_format("%H"),
                     limit=c(as.POSIXct("2009-04-01 02:00:00"),as.POSIXct("2009-04-02 01:00:00")))
  ggsave(plot, file=paste("Daily_Energy_Consumption_by_hour_","for_",i,".pdf",sep = ""))
}

#plotting per hour for a week
for (i in colNames) {
  plot <- ggplot(EnCon_by_hour, aes_string(x = "date_by_hour", y = i)) +
    geom_line() + xlab("Energy Consumption per Hour") + ylab(i) + 
    ggtitle(paste("Weekly En Con by hour","for",i,sep = " ")) +
    scale_x_datetime(labels = date_format("%A %H"),
                     limit=c(as.POSIXct("2009-03-31 02:00:00"),as.POSIXct("2009-04-06 01:00:00")))
  ggsave(plot, file=paste("Weekly_Energy_Consumption_by_hour_","for_",i,".pdf",sep = ""))
}

#plotting per day for a week
for (i in colNames) {
  plot <- ggplot(EnCon_by_day, aes_string(x = "date_by_day", y = i)) +
    geom_line() + xlab("Energy Consumption per Day") + ylab(i) + 
    ggtitle(paste("Weekly En Con by Day","for",i,sep = " ")) +
    scale_x_datetime(labels = date_format("%A %H"),
                     limit=c(as.POSIXct("2009-03-31"),as.POSIXct("2009-04-06")))
  ggsave(plot, file=paste("Weekly_Energy_Consumption_by_day_","for_",i,".pdf",sep = ""))
}

#plotting per week for year
for (i in colNames) {
  plot <- ggplot(EnCon_by_week, aes_string(x = "date_by_week", y = i)) +
    geom_line() + xlab("Energy Consumption per Week") + ylab(i) + 
    ggtitle(paste("Yearly En Con by Day","for",i,sep = " ")) +
    scale_x_datetime(labels = date_format("%d/%m"),
                     limit=c(as.POSIXct("2008-12-28"),as.POSIXct("2010-01-03")))
  ggsave(plot, file=paste("Yearly_Energy_Consumption_by_week_","for_",i,".pdf",sep = ""))
}



plot(EnCon1Full$DateTime,EnCon1Full$Active_Power)

#
ggplot(EnCon_by_hour, aes_string(x = "date_by_hour", y = "Active_Power")) +
    geom_line() + xlab("Energy Consumption per Hour") + ylab(i) + 
    ggtitle(paste("Weekly En Con by hour","for",i,sep = " ")) +
    scale_x_datetime(labels = date_format("%A %H") )



#equisse

#problem with laundry room