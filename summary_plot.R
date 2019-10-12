
library(lubridate)
library(openair)
source('data_conversion.r')
source('dataclean.R')



summary_plot<-function(data,Year,...){
  d<-dataclean(data)
  d<-data_conversion(d)
  d<-d[d$year==Year,]
  s<-summaryPlot(d, clip = FALSE)
  s
}