
library(lubridate)
library(openair)
source('data_conversion.r')
source('dataclean.R')


calendar_plot<-function(data,Station,Year,...){
  d<-dataclean(data)
  d<-data_conversion(d)
  d<-d[d$year==Year,]
  PM_Day<-aggregate(PM2.5 ~ date+station, data = d, max)
  PM_station<-PM_Day[PM_Day$station==Station,]
  p<-calendarPlot(PM_station, pollutant = "PM2.5", year = Year)
  p
}

