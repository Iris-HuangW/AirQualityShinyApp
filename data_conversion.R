data_conversion<-function(data){
  data$date<-with(data, ymd_h(paste(year, month, day, hour, sep= ' ')))
  data$month<-NULL
  data$day<-NULL
  data$hour<-NULL
  data
}
