# Data: Reforestable areas in Greece
# This R script is created as a Shiny application to process data from Greek 
# Ministry of Agricultural Development and Food and create an interactive map.
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
library(postGIStools)

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- get_postgis_query(con, "SELECT ST_SimplifyPreserveTopology(geom,0.5) AS geom FROM agriculture.reforestable",geom_name = "geom") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)

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
            addPolygons(data = mydata,opacity = 0.2, fillOpacity = 0.2)
    })
}

shinyApp(ui, server)