library(tidyverse)

#Organize Data for StreamChemistry - 2013 water year

StreamChemistry <- read.csv('C:/Users/eacor/OneDrive/Documents/SUNY_ESF/Graduate School/Fall 2023/Uncertainty/HubbardBrook_weekly_stream_chemistry.csv')

StreamChemistry<-StreamChemistry%>%filter(site == 'W3') 

StreamChemistry$DATETIME <- paste(StreamChemistry$date,StreamChemistry$timeEST)

StreamChemistry$DATETIME<-as.POSIXct(StreamChemistry$DATETIME, format = '%m/%d/%Y %H:%M')

class(StreamChemistry$DATETIME)

StreamChemistry <- StreamChemistry%>%filter(between(DATETIME, as.Date('2013/06/01'), as.Date('2017/05/31')))

StreamChemistry = subset(StreamChemistry, select = c(PO4,DATETIME))

#Time Series
library(ggplot2)

ggplot(StreamChemistry, aes(x=DATETIME, y=PO4)) +
  geom_line() + 
  xlab("")

#Organize sensor data - 2013 water year
StreamSensor <- read.csv("C:/Users/eacor/OneDrive/Documents/SUNY_ESF/Graduate School/Fall 2023/Uncertainty/HBF_WQual_Level4.csv")

StreamSensor$Date<-as.POSIXct(StreamSensor$Date, format = '%m/%d/%Y %H:%M')

class(StreamSensor$Date)

StreamSensor <- StreamSensor%>%filter(between(Date, as.Date('2013/06/01'), as.Date('2017/05/31')))

StreamSensor = subset(StreamSensor, select = c(Date, Nitrate_mg, TempC, pH, ODOPerCent, TurbidityFNU, FDOMRFU, PO4..ug.P.L.))

colnames(StreamSensor) =c("DATETIME", "PO4", "Nitrate_mg", "TempC", "pH", "ODOPerCent", "TurbidityFNU", "FDOMRFU")

ggplot(StreamSensor, aes(x=DATETIME, y=PO4)) +
  geom_point() + 
  xlab("")

#More data wrangling for random forest
##Combine Dataframes
library(data.table)

setDT(StreamChemistry)
setDT(StreamSensor)

setkey(StreamSensor, PO4, DATETIME)[, SensorSample:=PO4]
StreamSensor[StreamChemistry, roll='nearest']


#Random Forest
library(randomForest)
StreamSensor <- na.omit(StreamSensor)

mod = randomForest(PO4~SensorSample, data=StreamSensor)

mod
mod$rsq
plot(mod)
importance(mod)
varImpPlot(mod)

mod$predicted
