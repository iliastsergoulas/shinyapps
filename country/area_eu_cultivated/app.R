# This R script is created as a Shiny application to download raw data from Eurostat ((C) EuroGeographics for the administrative boundaries), 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(eurostat)
library(countrycode)

mydata<-get_eurostat("ef_oluaareg", time_format = "raw") # Downloading raw data from Eurostat
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$indic_ef=='AGRAREA_HA' & mydata$agrarea=='TOTAL'), ] # Filtering data
for (i in 1:nrow(mydata)) { # Replacing country codes in order to get correct country names
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name") # Getting country names
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")] # Filtering for country names not found
colnames(mydata)<-c("country", "year", "area")

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel( # Create sidebar panel with conditions
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")),
        conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                         selectInput('year', 'Έτος', choices = unique(mydata$year), selected = "2013"))
    ),
    mainPanel(
        tabsetPanel( # Create tabs
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", htmlOutput("map")), 
            tabPanel("Πίνακας Δεδομένων", dataTableOutput("table")),
            id = "conditionedPanels"
        ),
        print("Πηγή: (C) EuroGeographics for the administrative boundaries")))

server <- function(input, output) {
    data_country <- reactive({ # Add reactive data information
        data_country<-mydata[mydata$country==input$country, c("year", "area")]
        data_country<-aggregate(data_country$area, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Έκταση καλλιεργημένης γης")
        data_country
    })
    data_year <- reactive({ # Add reactive data information
        data_year<-mydata[mydata$year==input$year,  c("country", "area")]
        data_year<-aggregate(data_year$area, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Έκταση καλλιεργημένης γης")
        data_year
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Έκταση καλλιεργημένης γης (εκτάρια)'}", 
                                                     hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=700, height=600, legend='none'))
    })
    output$map <- renderGvis({
        #mapdata<-mydata[mydata$year==2013 & mydata$area>0, c("country", "area")]
        #mapdata<-aggregate(mapdata$area, by=list(Country=mapdata$country), FUN=sum)
        GeoStates <- gvisGeoChart(data_year(), "Χώρα", "Έκταση καλλιεργημένης γης", options=list(region="150", displayMode="regions", 
                                                                                                 datamode='regions',width=700, height=600))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Χώρα", "Έτος", "Έκταση καλλιεργήσιμης γης")
        mydata
    })
}
shinyApp(ui, server)