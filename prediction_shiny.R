library(shiny)
library(fastDummies)
#library(xgboost)

#data(agaricus.train, package='xgboost')
#data(agaricus.test, package='xgboost')

ui=fluidPage(
  sliderInput(inputId="Year", label="Select Year", 
               min=2018, max=2020,value=2018),
  sliderInput(inputId="Month", label="Select Month", 
              min=1, max=12,value=1),
  numericInput(inputId="Day", label="Enter Day", 
               value=0,min=1, max=31),
  numericInput(inputId="Hour", label="Enter Hour", 
               value=0,min=0, max=24),
  numericInput(inputId="TEMP", label="Enter Temperature", 
               value=0,min=-50, max=40),
  numericInput(inputId="PRES", label="Enter Pressure", 
               value=0,min=0, max=2),
  numericInput(inputId="DEWP", label="Enter Dew Point Temperature", 
               value=0,min=-50, max=50),
  numericInput(inputId="Rain", label="Enter Rain Precipitation ", 
               value=0,min=0, max=200),
  selectInput(inputId = "wd", label="Choose Wind Direction",
              choices = list("E"="E","ENE"="ENE","ESE"="ESE",'N'='N','NE'='NE','NNE'='NNE','NNW'='NNW','NW'='NW','S'='S','SE'='SE',
                             'SSE'='SSE','SSW'='SSW','SW'='SW','W'='W','WNW'='WNW','WSW'='WSW')),
  numericInput(inputId="WSPM", label="Enter Wind Speed", 
               value=0,min=0, max=20),
  selectInput(inputId = "station", label="Choose Station",
              choices = list("Aotizhongxing"="Aotizhongxing","Changping"="Changping","Dingling"="Dingling",'Dongsi'='Dongsi',
                             'Guanyuan'='Guanyuan','Gucheng'='Gucheng','Huairou'='Huairou','Nongzhanguan'='Nongzhanguan',
                             'Shunyi'='Shunyi','Tiantan'='Tiantan','Wanliu'='Wanliu','Wanshouxigong'='Wanshouxigong')),
  actionButton("Enter", "Enter Values"),
  verbatimTextOutput("modelSummary")
)

server = function(input,output, session){
  observeEvent( input$Enter, {
    year = input$Year
    month = input$Month
    day = input$Day
    hour = input$Hour
    TEMP = input$TEMP
    PRES = input$PRES
    DEWP = input$DEWP
    Rain = input$Rain
    wd = as.factor(input$wd)
    WSPM = input$WSPM
    station = as.factor(input$station)
    t = data.frame(year, month, day,hour,TEMP,PRES,DEWP,Rain,wd,WSPM,station)
    t = dummy_cols(t, select_columns = c("wd", "station"))
    t$wd = NULL
    t$station = NULL
    t_h2o<-as.h2o(t)
    
    output$modelSummary <- renderPrint({
      p<-h2o.predict(rf,t_h2o)
      myTestPreds=as.data.frame(p)
      myTestPreds
    })
  })
}
shinyApp(ui=ui, server=server)
