dataclean<-function(data){
  data$X<-NULL
  data$No<-NULL
  data$PM10<-NULL
  data$PRES<-data$PRES/1000
  data<-data[,c(5,1:4,6:12)]
}
