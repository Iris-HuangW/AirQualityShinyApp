
data <- read.csv(file="C:\\Users\\Iris Huang\\Downloads\\fullhazedatanomissing.csv", header=TRUE, sep=",")
data <- data[,3:ncol(data)]

#install.packages('shinythemes')
library(shinythemes)
#install.packages('leaflet')
#install.packages('maps')
library(leaflet)
library(DT)

library(maps)

#install.packages('ggmap')
library(ggmap)
library(ggplot2)



#install.packages("shinydashboard")
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
                sidebarPanel(
                    selectInput("SelStation", h4("Select Station"), choices = 
                                  list("Aotizhongxin" = "Aotizhongxin", 
                                       "Changping" = "Changping",
                                       "Dingling" = "Dingling",
                                       "Dongsi" = "Dongsi",
                                       "Guanyuan" = "Guanyuan",
                                       "Gucheng" = "Gucheng",
                                       "Huairou" = "Huairou",
                                       "Nongzhanguan" = "Nongzhanguan",
                                       "Shunyi" = "Shunyi",
                                       "Tiantan" = "Tiantan",
                                       "Wanliu" = "Wanliu",
                                       "Wanshouxigong" = "Wanshouxigong"), 
                                selected = "Tiantan"),
                    sliderInput("SelYear", h4("Select Year"), value = 2015, min = 2013, max = 2017)
                    
                ),
                mainPanel(
                  
                )
        ),
        tabItem(tabName = "map",
                sidebarPanel(
                  
                ),
                mainPanel(
                  leafletOutput("map")
                )
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
                             h5("Project instructor: Matthew Lanham"),
                             h5("Version: 1.0"),
                             h6("Contact us:"),
                             a("666666@purdue.edu")
                             
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
  output$table <- DT::renderDataTable({
    DT::datatable(data[, input$show_vars, drop = FALSE])
  })
  output$map <- renderLeaflet({
    leaflet()%>%addTiles()%>%addMarkers(lng=116.391, lat=39.912, popup="这里是北京")
  })
  
}

################################################################################################
shinyApp(ui,server)
