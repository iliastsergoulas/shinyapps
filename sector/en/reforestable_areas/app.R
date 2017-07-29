# Data: Forest areas in Greece
# This R script is created as a Shiny application to process data 
# from Corine 2012 and create an interactive map.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(maptools)
library(htmltools)
library(shinydashboard)
library(RPostgreSQL)
library(rmapshaper)

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.reforestable") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
#plants <- readShapePoly("C://Users/itsergoulas/Desktop/mydata.shp")
plants<-ms_simplify(plants)

header <- dashboardHeader(title = "Reforestable areas", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    leafletOutput("map_points")
)
body <- dashboardBody(frow1) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output, session) {
    output$map_points <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.Mapnik", 
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolygons(data = plants,opacity = 0.5, fillOpacity = 0.2)
    })
}

shinyApp(ui, server)