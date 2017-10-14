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
library(RPostgreSQL)
library(postGIStools)

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
plants <- get_postgis_query(con, "SELECT * FROM agriculture.forest_villages")
dbDisconnect(con)
dbUnloadDriver(drv)
header <- dashboardHeader(title = "Forest villages", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    leafletOutput("map_points"))
frow2 <- fluidRow(# Creating row of diagram and summary
    title = "Στοιχεία δασικών χωριών",
    status="success",
    collapsible = TRUE,
    theme = shinytheme("spacelab"), 
    mainPanel(
        dataTableOutput("list"),
        width=550)
)

body <- dashboardBody(frow1, frow2) # Binding rows to body of dashboard
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
    output$list <- renderDataTable({ # Creating summary by country
        plants_list}, options = list(lengthMenu = c(5, 25, 50), pageLength = 10)
    )
}
shinyApp(ui, server)