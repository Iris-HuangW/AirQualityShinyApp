library(shiny)
library(fastDummies)
library(lubridate)
library(openair)
source('data_conversion.r')
source('dataclean.R')
data<-read.table('fullhazedatanomissing.csv',header=TRUE, sep=",")

ui=fluidPage(
  sliderInput(inputId="Year", label="Select Year", 
              min=2013, max=2017,value=2013),
  selectInput(inputId = "station", label="Choose Station",
              choices = list("Aotizhongxing"="Aotizhongxing","Changping"="Changping","Dingling"="Dingling",'Dongsi'='Dongsi',
                             'Guanyuan'='Guanyuan','Gucheng'='Gucheng','Huairou'='Huairou','Nongzhanguan'='Nongzhanguan',
                             'Shunyi'='Shunyi','Tiantan'='Tiantan','Wanliu'='Wanliu','Wanshouxigong'='Wanshouxigong')),
  plotOutput(outputId='timevariation')
  #actionButton("Enter", "Enter Values")
)

server = function(input,output, session){
  #observeEvent( input$Enter, {
    #year = input$Year
    #station = as.factor(input$station)
    
    
    output$timevariation <- renderPlot({
      time_variation(data,input$station,input$Year)
    })
  #})
}
shinyApp(ui=ui, server=server)