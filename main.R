getwd()
data<-read.table('fullhazedatanomissing.csv',header=TRUE, sep=",")
spot =  read.csv(file="l_a.csv", header=TRUE, sep=",")
names(spot)[1] = 'desc'

source('data_conversion.R')
source('dataclean.R')
source('calender_plot.R')
source('time_variation.R')
source('summary_plot.R')

library(shiny)
library(caret)
library(dplyr)
library(readr)
library(fastDummies)
library(randomForest)
library(shinythemes)
library(leaflet)
library(DT)
library(openair)
library(maps)
library(ggmap)
library(ggplot2)
library(shinydashboard)

#install.packages('')
################################################################################################
#mapStates = map("state", fill = TRUE, plot = FALSE)
#leaflet(data = mapStates) %>% addTiles() %>%
#addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


#ggplot(china_map1, aes(x = long, y = lat, group = group)) +
#  geom_path(color = "grey40") +
#  geom_polygon(fill = 'beige')

#install.packages('rgdal')
#library(rgdal)
#china_region <- readOGR(dsn='C:/Users/Iris Huang/Downloads', layer = "BOUNT_poly") ## read the province level data
#china_region <- transform(china_region, NAME99 = iconv(NAME99, from = "GBK")) ## Transform from GBK to UTF-8
#china_region$ADCODE99[grep("北京", china_region$NAME99)]



#library(ggmap)
#google_key()
#register_google(key= "AIzaSyBKIA9TOPEX0nf85YNEVi91PMBMu3vez8o")
#Aotizhongxin = get_map(geocode("Beijing Olympic Sports Center"),zoom=12)
#ggmap(Aotizhongxin)

################################################################################################
d<-dataclean(data)

#change wind speed to positive value
d$DEWP = abs(d$DEWP)


#create dummy variables
library(fastDummies)
d = dummy_cols(d, select_columns = c("wd", "station"))
d$wd = NULL
d$station = NULL


#correlation (No Result)
library(caret)
d=d%>%
  select(PM2.5, everything())

descrCor <-  cor(d[,2:ncol(d)])                          

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.8)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)

summary(descrCor2[upper.tri(descrCor2)])
d <- cbind(d$PM2.5, filteredDescr)
names(d)[1] <- "PM2.5"

rm(filteredDescr, descrCor, descrCor2,  highlyCorDescr)


#pre-processing
preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)

set.seed(1234) # set a seed so you can replicate your results
inTrain <- createDataPartition(y = d$PM2.5,   # outcome variable
                               p = .90,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set
rm(inTrain)

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

m1 <- train(PM2.5 ~ .,               # model specification
            data = train,        # train set used to build model
            method = "lm",      # type of model you want to build
            trControl = ctrl,    # how you want to learn
            metric = "RMSE"       # performance measure
)
m1
################################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Beijing Air-Quality Application"),
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    
    sidebarMenu(
      
      menuItem("Descriptive", tabName = "desc", icon = icon("dashboard")),
      menuItem("Maps & Predictive", tabName = "map", icon = icon("map-marker",lib = "glyphicon")),
      menuItem("Data Table", tabName = "table", icon = icon("th")),
      menuItem("About Us", tabName = "info", icon = icon("header",lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "desc",
                
                mainPanel(
                  sliderInput(inputId="Year", label="Select Year", 
                              min=2013, max=2017,value=2013),
                  selectInput(inputId = "station", label="Choose Station",
                              choices = list("Aotizhongxin"="Aotizhongxin","Changping"="Changping","Dingling"="Dingling",'Dongsi'='Dongsi',
                                             'Guanyuan'='Guanyuan','Gucheng'='Gucheng','Huairou'='Huairou','Nongzhanguan'='Nongzhanguan',
                                             'Shunyi'='Shunyi','Tiantan'='Tiantan','Wanliu'='Wanliu','Wanshouxigong'='Wanshouxigong')),
                  plotOutput(outputId='calendarplot'),
                  
                  plotOutput(outputId='timevariation'),
                  
                  plotOutput(outputId='summaryplot')
                  
                )
        ),
        tabItem(tabName = "map",
                absolutePanel(
                  h2("PM2.5 Prediction"),
                  id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = 1000,
                  checkboxInput('input_draw_point', 'Draw point', FALSE ),
                  verbatimTextOutput('summary'),
                  style = "opacity: 0.85; z-index: 10;",
                  
                  sliderInput(inputId="Year", label="Select Year", 
                              min=2018, max=2025,value=2018),
                  sliderInput(inputId="Month", label="Select Month", 
                              min=1, max=12,value=1),
                  numericInput(inputId="Day", label="Enter Day", 
                               value=1,min=1, max=31),
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
                              choices = list("Aotizhongxin"="Aotizhongxin","Changping"="Changping","Dingling"="Dingling",'Dongsi'='Dongsi',
                                             'Guanyuan'='Guanyuan','Gucheng'='Gucheng','Huairou'='Huairou','Nongzhanguan'='Nongzhanguan',
                                             'Shunyi'='Shunyi','Tiantan'='Tiantan','Wanliu'='Wanliu','Wanshouxigong'='Wanshouxigong')),
                  actionButton("Enter", "Enter Values"),
                  verbatimTextOutput("modelSummary")
                ),
                
                leafletOutput("map", width=1500, height=1000)
        ),
        tabItem(tabName = "table",
                sidebarPanel(
                  checkboxGroupInput("show_vars", "Columns to show:",
                                     names(data), selected = names(data))
                ),
                mainPanel(
                  dataTableOutput("table")
                )
        ),
        tabItem(tabName = "info",
                sidebarPanel("Information",
                             h5("Maintainer: Mario, Tom, Vera, Iris"),
                             h5("Version: 1.0"),
                             h5("Contact us:"),
                             h6("lanhamm@purdue.edu"),
                             h6("chen@purdue.edu"),
                             h6("wei@purdue.edu"),
                             h6("guo@purdue.edu"),
                             h6("huang@purdue.edu")
                             
                ),
                mainPanel(
                  
                  img(src="https://krannert.purdue.edu/includes/img/KRN-BG-V-RGB.png",width=200,height=80),
                  br(),
                  h3("Beijing Multi-Site Air Quality Application",aign="center"),
                  h5("Air polution in Beijing, China has been a serious environment problem endangering health of 
                     Beijing residents and facing local industries and governments for decades. This application 
                     explores the hidden data patterns and correlations between variables of multi-site air 
                     quality stations in Beijing, China. Using predictive modeling prediction, users are able to 
                     foresee the PM 2.5 and PM 10 concentrations and plan for their daily activities."),
                  br(),
                  h5("DataSource:"),
                  h5("Beijing Multi-Site Air-Quality Data Data Set: This data set includes hourly air pollutants 
                     data from 12 nationally-controlled air-quality monitoring sites. The air-quality data are 
                     from the Beijing Municipal Environmental Monitoring Center. The meteorological data in each 
                     air-quality site are matched with the nearest weather station from the China Meteorological 
                     Administration. The time period is from March 1st, 2013 to February 28th, 2017. Missing data 
                     are denoted as NA."),
                  h5(a("http://archive.ics.uci.edu/ml/datasets/Beijing+Multi-Site+Air-Quality+Data#",
                       href="http://archive.ics.uci.edu/ml/datasets/Beijing+Multi-Site+Air-Quality+Data#")),
                  br(),
                  h5("Packages:"),
                  h5("The application use the following packages : H2O, Caret, shiny, shinyWidgets, shinythemes, 
                     leaflet, ggmap. To see which versions of these packages are used (and dependancies), look at 
                     the file package_info.txt in app directory. If packages are not installed, they will be on 
                     application launch."),
                  br(),
                  br(),
                  h5("Features and Functionalities:"),
                  h5("Descriptive Data Visualizations generated from configured variable selections, 
                     Prediction result and Recommendation generated from predictive models and user input, 
                     Data Views in data tables"),
                  br(),
                  h5("technology used: ",",",a("R",href="https://www.r-project.org/"),",", 
                     a("Shiny",href="http://shiny.rstudio.com/") ,",", 
                     a("H2O",href="https://www.h2o.ai/") ,",", 
                     a("Leaflet",href="https://cran.r-project.org/web/packages/leaflet/index.html"), ",",
                     a("OpenAir",href="https://cran.r-project.org/web/packages/openair/index.html"))
                  
                ) )
      )
    )
  )
)
################################################################################################

################################################################################################
server = function(input, output) {
  
  output$calendarplot <- renderPlot({
    calendar_plot(data,input$station,input$Year)
  })
  
  output$timevariation <- renderPlot({
    time_variation(data,input$station,input$Year)
  })
  
  output$summaryplot <- renderPlot({
    summary_plot(data,input$Year)
  })
  
  output$map <- renderLeaflet({
    leaflet()%>%addTiles()%>%addMarkers(lng=-116.40, lat=31.5, popup="Welcome to Beijing")
    leaflet(spot) %>% addTiles() %>%
      addCircles(lng = ~lat, lat = ~long, weight = 1,radius = 1000, popup = ~desc
      )
  })
  
  observeEvent( input$Enter, {
    year = input$Year
    month = input$Month
    day = input$Day
    hour = input$Hour
    TEMP = input$TEMP
    PRES = input$PRES
    DEWP = input$DEWP
    RAIN = input$Rain
    WSPM = input$WSPM
    
    
    wd = as.factor(input$wd)
    wd_E = ifelse(wd == 'E', 1, 0)
    wd_ENE = ifelse(wd == 'ENE', 1, 0)
    wd_ESE = ifelse(wd == 'ESE', 1, 0)
    wd_N = ifelse(wd == 'N', 1, 0)
    wd_NE = ifelse(wd == 'NE', 1, 0)
    wd_NNE = ifelse(wd == 'NNE', 1, 0)
    wd_NNW = ifelse(wd == 'NNW', 1, 0)
    wd_NW = ifelse(wd == 'NW', 1, 0)
    wd_S = ifelse(wd == 'S', 1, 0)
    wd_SE = ifelse(wd == 'SE', 1, 0)
    wd_SSE = ifelse(wd == 'SSE', 1, 0)
    wd_SSW = ifelse(wd == 'SW', 1, 0)
    wd_SW = ifelse(wd == 'SW', 1, 0)
    wd_W = ifelse(wd == 'W', 1, 0)
    wd_WNW = ifelse(wd == 'WNW', 1, 0)
    wd_WSW = ifelse(wd == 'WSW', 1, 0)
    
    station = as.factor(input$station)
    station_Aotizhongxin = ifelse(station == 'Aotizhongxin', 1, 0)
    station_Changping = ifelse(station == 'Changping', 1, 0)
    station_Dingling = ifelse(station == 'Dingling', 1, 0)
    station_Dongsi = ifelse(station == 'Dongsi', 1, 0)
    station_Guanyuan = ifelse(station == 'Guanyuan', 1, 0)
    station_Gucheng = ifelse(station == 'Gucheng', 1, 0)
    station_Huairou = ifelse(station == 'Huairou', 1, 0)
    station_Nongzhanguan = ifelse(station == 'Nongzhanguan', 1, 0)
    station_Shunyi = ifelse(station == 'Shunyi', 1, 0)
    station_Tiantan = ifelse(station == 'Tiantan', 1, 0)
    station_Wanliu = ifelse(station == 'Wanliu', 1, 0)
    station_Wanshouxigong = ifelse(station == 'Wanshouxigong', 1, 0)
    
    
    t = data.frame(year, month, day,hour,TEMP,PRES,DEWP,RAIN,wd,WSPM,station, 
                   wd_E, wd_ENE, wd_ESE, wd_N, wd_NE, wd_NNE, wd_NNW, wd_NW, wd_S, wd_SE, wd_SSE, wd_SSW, wd_SW, wd_W, wd_WNW, wd_WSW,
                   station_Aotizhongxin, station_Changping, station_Dingling, station_Dongsi, station_Guanyuan, station_Gucheng, station_Huairou,
                   station_Nongzhanguan, station_Shunyi, station_Tiantan, station_Wanliu, station_Wanshouxigong)
    
    #t = dummy_cols(t, select_columns = c("wd", "station"))
    t$wd = NULL
    t$station = NULL
    
    output$modelSummary <- renderPrint({
      p<-predict(m1,newdata = t)
      p = abs(p)/500
      myTestPreds=as.data.frame(p)
      myTestPreds
    })
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(data[, input$show_vars])
  )
  
}

################################################################################################
shinyApp(ui,server)
