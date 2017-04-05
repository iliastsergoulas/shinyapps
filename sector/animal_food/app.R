# Data: Animal food plant units in Greece
# This R script is created as a Shiny application to process and map data, as supplied 
# by Greek Ministry of Agricultural Production and Food.
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

plants <- read.csv("C:/Users/itsergoulas/Dropbox/Website/shiny/sector/animal_plants/animal_plants.csv", sep=";")
plants_edited <- as.data.frame(plants)
print(plants_edited)
plants_per_region <- plants_edited[c("region", "id")]
plants_per_pref <- plants_edited[c("prefecture", "id")]


header <- dashboardHeader(title = "Επιχειρήσεις μεταποίησης ζωοτροφών", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of two diagrams
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
body <- dashboardBody(frow1) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output, session) {
    output$regions<-renderPlot({ # Per region
        ggplot(plants_per_region, aes(x = factor(region))) + 
            geom_bar(stat="count", fill="steelblue",width=0.5, color="steelblue") + 
            xlab("Περιφέρεια") + ylab("Αριθμός μονάδων") + 
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            geom_text(stat='count',aes(label=..count..),vjust=-1) + 
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
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)
        )  
    })
}

shinyApp(ui, server)