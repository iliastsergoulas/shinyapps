# Data: Eggs plants in Greece
# This R script is created as a Shiny application to process data 
# from Greek Ministry of Agriculture and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(maptools)
library(htmltools)
library(shinydashboard)

plants <- readShapePoints("/home/itsergoulas/shapefiles/forest_villages/forest_villages.shp")
header <- dashboardHeader(title = "Forest villages", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    leafletOutput("map_points"))

body <- dashboardBody(frow1) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output, session) {
    output$map_points <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.Mapnik",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = plants, 
                       icon = ~ icons(
                           iconUrl = "./hut.png",
                           iconWidth = 20, iconHeight = 20, shadowWidth = 15, shadowHeight = 15),
                       popup = ~htmlEscape(name))
    })
}
shinyApp(ui, server)