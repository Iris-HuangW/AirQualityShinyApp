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
  plotOutput(outputId='summaryplot')
)

server = function(input,output, session){
  output$summaryplot <- renderPlot({
    summary_plot(data,input$Year)
  })
}
shinyApp(ui=ui, server=server)