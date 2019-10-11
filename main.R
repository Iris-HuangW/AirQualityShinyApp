
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
ui = tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = "lumen",
    "Beijing Multi-Site Air-Quality Data Application",
    tabPanel("Descriptive Data",
             mainPanel(
               tabsetPanel(
                 tabPanel("Descriptive Graph Name 1",
                          h4("Table"),
                          plotOutput("plot2"),
                          h5("Description")
                 ),
                 tabPanel("Descriptive Graph Name 2",
                          h4("Table"),
                          plotOutput("plot2"),
                          h5("Description")
                 ),
                 tabPanel("Descriptive Graph Name 3",
                          h4("Table"),
                          plotOutput("plot3"),
                          h5("Description")
                 )
               )
             )
    ),
    tabPanel("Maps and Predictive Models", 
             sidebarPanel(
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Deafult actionButton:"),
               actionButton("action", "Search"),
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               
               
             )
    ),
    tabPanel("Data Explorer", 
             sidebarPanel(
               conditionalPanel(
                 'input.dataset === "data"',
                 checkboxGroupInput("show_vars", "Columns to show:",
                                    names(data), selected = names(data))
               )
             ),
             mainPanel(
               dataTableOutput("table")
             )
    ),
    tabPanel("About Us",
             "This shiny app is designed for ....If you have any concern."
    )
  )
)
data <- head(data,n=100)
################################################################################################
server = function(input, output) {
  output$table <- DT::renderDataTable({
    DT::datatable(data[, input$show_vars, drop = FALSE])
  })
  output$map <- renderLeaflet({
    
  })
  
}

################################################################################################
shinyApp(ui,server)
