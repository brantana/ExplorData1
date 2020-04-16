library(curl)
library(tidyverse)
library(lubridate)
library(tibble)
library(dplyr)
library(rafalib)
library(datasets)
#Download data file to local drive.
url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
destfile = "dat.zip"
download.file(url,destfile,method = "curl")
#Extract (unzip) data file.
unzip("dat.zip")
tmp1<-read.table("household_power_consumption.txt",sep = ";",header=TRUE)
head(tmp1)
dim(tmp1)
str(tmp1,vec.len=2)
#Re-class and prepare selected data file
#       Extract the required dates
tmp2<- tmp1[ which(tmp1$Date=="1/2/2007" | tmp1$Date=="2/2/2007"), ]
#       Revise variable class assignments
tmp2$Date<-dmy(tmp2$Date)
tmp2$Time<-hms(tmp2$Time)
tmp2$Date_Time<-ymd_hms(tmp2$Date+tmp2$Time)
tmp2$Global_active_power<-as.numeric(tmp2$Global_active_power)
tmp2$Global_reactive_power<-as.numeric(tmp2$Global_reactive_power)
tmp2$Voltage<-as.numeric(tmp2$Voltage)
tmp2$Global_intensity<-as.numeric(tmp2$Global_intensity)
tmp2$Sub_metering_1<-as.numeric(tmp2$Sub_metering_1)
tmp2$Sub_metering_2<-as.numeric(tmp2$Sub_metering_2)
tmp2$Sub_metering_3<-as.numeric(tmp2$Sub_metering_3)
str(tmp2,vec.len=2)
#Create charts
mypar(1,1)
#       Plot1.png
with(faithful,hist(tmp2$Global_active_power/500,
                xlab="Global Active Power (kilowatts)",ylab ="Frequency",
                col="red",main="Global Active Power",ylim = c(0,1200)))
dev.copy(png,file="plot1.png")
dev.off()
#       Plot2.png
with(faithful,plot(tmp2$Date_Time,tmp2$Global_active_power/500,
                xlab="",ylab="Global Active Power (kilowatts)",
                type="l"))
dev.copy(png,file="plot2.png")
dev.off()
#       Plot3.png (The combined plot)
with(faithful,plot(tmp2$Date_Time,tmp2$Sub_metering_1-2,
                xlab="",ylab="Energy sub metering",
                type="l",ylim = c(0,40)))
par(new=TRUE)
with(faithful,plot(tmp2$Date_Time,tmp2$Sub_metering_2-2,
                xlab="",ylab="Energy sub metering",
                type="l", col="red",ylim = c(0,40)))
par(new=TRUE)
with(faithful,plot(tmp2$Date_Time,tmp2$Sub_metering_3,
                xlab="",ylab="Energy sub metering",
                type="l",col="blue",ylim = c(0,40)))
legend(c("topright"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"),
       lty=c(1,1,1), ncol=1)
dev.copy(png,file="plot3.png")
dev.off()
#       Plot4 (The 2x2 set of plots)
mypar(2,2)
#       Plot4-1
with(faithful,plot(tmp2$Date_Time,tmp2$Global_active_power/500,
                xlab="",ylab="Global Active Power (kilowatts)",
                type="l"))
#       Plot4-2
with(faithful,plot(tmp2$Date_Time,tmp2$Voltage,
                xlab="datetime",ylab="Voltage",
                type="l"))
#       Plot4-3
with(faithful,plot(tmp2$Date_Time,tmp2$Sub_metering_1-2,
                 xlab="",ylab="Energy sub metering",
                 type="l",ylim = c(0,40)))
par(new=TRUE)
with(faithful,plot(tmp2$Date_Time,tmp2$Sub_metering_2-2,
                 xlab="",ylab="Energy sub metering",
                 type="l", col="red",ylim = c(0,40)))
par(new=TRUE)
with(faithful,plot(tmp2$Date_Time,tmp2$Sub_metering_3,
                 xlab="",ylab="Energy sub metering",
                 type="l",col="blue",ylim = c(0,40)))
legend(c("topright"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"),
       lty=c(1,1,1), ncol=1,bty="n")
#       Plot4-4
with(faithful,plot(tmp2$Date_Time,tmp2$Global_reactive_power,
     xlab="datetime",ylab="Global_reactance _power",
     type="l"))
dev.copy(png,file="plot4.png")
dev.off()






