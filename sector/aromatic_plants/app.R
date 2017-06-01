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

plants <- readShapePoints("C://Users/itsergoulas/Dropbox/Website/shapefiles/aromatic/aromatic_plants.shp")
plants_list <- as.data.frame(plants)
plants_list <- plants_list[c("business_n", "type", "website")]
names(plants_list)<-c("Επωνυμία", "Κατηγορία", "Ιστότοπος")
plants_1 <- plants[which(plants$type=='Κατασκευαστής μηχανημάτων επεξεργασίας ΑΦΦ'),]
plants_2 <- plants[which(plants$type=='Κατασκευαστής αποστακτήρων αιθερίων ελαίων'),]
plants_3 <- plants[which(plants$type=='Αποστακτήριο αρωματικών φυτών'),]

icon.glyphicon <- makeAwesomeIcon(icon= 'home', markerColor = 'blue')
icon.fa <- makeAwesomeIcon(icon = 'home', markerColor = 'red')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green')

header <- dashboardHeader(title = "Επιχειρήσεις τομέα αρωματικών και φαρμακευτικών φυτών", titleWidth=700) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    leafletOutput("map_points")
)
frow2 <- fluidRow(# Creating row of diagram and summary
    title = "Στοιχεία επιχειρήσεων",
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
            addAwesomeMarkers(data = plants_1, icon = icon.fa,
                       popup = ~htmlEscape(business_n), group="Κατασκευαστές μηχανημάτων επεξεργασίας ΑΦΦ") %>%
            addAwesomeMarkers(data = plants_2,icon = icon.glyphicon,
                              popup = ~htmlEscape(business_n), group="Κατασκευαστές αποστακτήρων αιθερίων ελαίων") %>% 
            addAwesomeMarkers(data = plants_3,icon = icon.ion,
                              popup = ~htmlEscape(business_n), group="Αποστακτήρια αρωματικών φυτών") %>% 
            addLayersControl(
                overlayGroups = c("Κατασκευαστές μηχανημάτων επεξεργασίας ΑΦΦ", 
                                  "Κατασκευαστές αποστακτήρων αιθερίων ελαίων", 
                                  "Αποστακτήρια αρωματικών φυτών"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    output$list <- renderDataTable({ # Creating summary by country
        plants_list}, options = list(lengthMenu = c(5, 25, 50), pageLength = 10)
    )
}

shinyApp(ui, server)