library(shiny)
library(googleVis)
library(shinythemes)
library(eurostat)
library(countrycode)

mydata<-get_eurostat("ef_oluaareg", time_format = "raw")
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$indic_ef=='AGRAREA_HA' & mydata$agrarea=='TOTAL'), ]
for (i in 1:nrow(mydata)) { 
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name")
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")]
colnames(mydata)<-c("country", "year", "area")

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    #headerPanel("Καλλιεργήσιμη γη"),
    sidebarPanel(
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")),
        conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                         selectInput('year', 'Έτος', choices = unique(mydata$year), selected = "2013"))
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", htmlOutput("map")), 
            tabPanel("Πίνακας Δεδομένων", dataTableOutput("table")),
            id = "conditionedPanels"
        )))

server <- function(input, output) {
    # Add reactive data information
    data_country <- reactive({
        data_country<-mydata[mydata$country==input$country, c("year", "area")]
        data_country<-aggregate(data_country$area, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Έκταση καλλιεργημένης γης")
        data_country
    })
    data_year <- reactive({
        data_year<-mydata[mydata$year==input$year,  c("country", "area")]
        data_year<-aggregate(data_year$area, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Έκταση καλλιεργημένης γης")
        data_year
    })
    output$view <- renderGvis({
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Έκταση καλλιεργημένης γης (εκτάρια)'}", 
                                                     hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=800, height=700, legend='none'))
    })
    output$map <- renderGvis({
        #mapdata<-mydata[mydata$year==2013 & mydata$area>0, c("country", "area")]
        #mapdata<-aggregate(mapdata$area, by=list(Country=mapdata$country), FUN=sum)
        GeoStates <- gvisGeoChart(data_year(), "Χώρα", "Έκταση καλλιεργημένης γης", options=list(region="150", displayMode="regions", 
                                                                                                 datamode='regions',width=800, height=700))
    })
    # Generate an HTML table view of the data
    output$table <- renderDataTable({
        colnames(mydata)<-c("Χώρα", "Έτος", "Έκταση καλλιεργήσιμης γης")
        mydata
    })
}
shinyApp(ui, server)