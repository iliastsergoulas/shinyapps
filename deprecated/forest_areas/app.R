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
library(postGIStools)

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
coniferous <- get_postgis_query(con, "SELECT ST_SimplifyPreserveTopology(geom,500) AS geom from agriculture.corine_coniferous",geom_name = "geom")
broadleaved <- get_postgis_query(con, "SELECT ST_SimplifyPreserveTopology(geom,500) AS geom from agriculture.corine_broadleaved",geom_name = "geom")
mixedforests <- get_postgis_query(con, "SELECT ST_SimplifyPreserveTopology(geom,500) AS geom from agriculture.corine_mixedforests",geom_name = "geom")
dbDisconnect(con)
dbUnloadDriver(drv)

header <- dashboardHeader(title = "Δασικές περιοχές", titleWidth=500) # Header of dashboard
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
            addPolygons(data = coniferous, group="Coniferous", color = "#008d4c", 
                        weight = 1, opacity = 0.5) %>% 
            addPolygons(data = broadleaved, group="Broad-leaved", color = "#24570a", 
                        weight = 1, opacity = 0.5) %>% 
            addPolygons(data = mixedforests, group="Mixed forests", color = "#b0c220", 
                        weight = 1, opacity = 0.5)
    })
}

shinyApp(ui, server)