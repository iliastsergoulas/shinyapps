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
library(ggplot2)
library(directlabels)
library(scales)
library(shinydashboard)
library(RPostgreSQL)
library(postGIStools)

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
plants <- get_postgis_query(con, "SELECT * FROM agriculture.eggs")
dbDisconnect(con)
dbUnloadDriver(drv)
plants_edited <- as.data.frame(plants)
plants_per_region <- plants_edited[c("region_nam", "id")]
plants_per_pref <- plants_edited[c("prefecture", "id")]

header <- dashboardHeader(title = "Μονάδες τυποποίησης και συσκευασίας αυγών", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    leafletOutput("map_points")
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Ανά Περιφέρεια",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("regions", width="150%"))
        ),
    box(
        title = "Ανά Περιφερειακή Ενότητα",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("prefectures",  width="150%")
        ))
)
body <- dashboardBody(frow1, frow2) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output, session) {
    output$map_points <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.Mapnik",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = plants, clusterOptions = markerClusterOptions(),
                       icon = ~ icons(
                           iconUrl = "./egg.ico",
                           iconWidth = 20, iconHeight = 20, shadowWidth = 15, shadowHeight = 15),
                       popup = ~htmlEscape(business_n))
    })
    output$regions<-renderPlot({ # Per region
        ggplot(plants_per_region, aes(x = factor(region_nam))) + 
            geom_bar(stat="count", fill="steelblue",width=0.5, color="steelblue") + 
            xlab("Περιφέρεια") + ylab("Αριθμός μονάδων") + 
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            geom_text(stat='count',aes(label=..count..),vjust=-1) + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)
        )  
    })
    output$prefectures<-renderPlot({ # Per prefecture
        ggplot(plants_per_pref, aes(x = factor(prefecture))) + 
            geom_bar(stat="count", fill="steelblue") + 
            xlab("Περιφερειακή ενότητα") + ylab("Αριθμός μονάδων") + 
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            geom_text(stat='count',aes(label=..count..),vjust=-1) + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)
        )  
    })
}

shinyApp(ui, server)