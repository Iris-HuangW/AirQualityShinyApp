
library(lubridate)
library(openair)
source('data_conversion.r')
source('dataclean.R')

time_variation<-function(data,Station,Year){
  d<-dataclean(data)
  d<-data_conversion(d)
  d<-d[d$year==Year,]
  PM_h_station<-d[d$station==Station,]
  ph<-timeVariation(PM_h_station, pollutant = "PM2.5")
  ph
}

