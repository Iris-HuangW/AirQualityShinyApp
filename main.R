
data <- read.csv(file="C:\\Users\\Iris Huang\\Downloads\\fullhazedatanomissing.csv", header=TRUE, sep=",")
head(data)

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
        tabItem(tabName = "desc"
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
                mainPanel('This app is awesome'))
        
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
