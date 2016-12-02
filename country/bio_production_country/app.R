library(shiny)
library(googleVis)
library(shinythemes)
library(eurostat)
library(countrycode)

mydata<-get_eurostat("org_croppro", time_format = "raw")
mydata$geo<-as.character(mydata$geo)
for (i in 1:nrow(mydata)) { 
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name")
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")]
colnames(mydata)<-c("country", "year", "quantity")

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
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
    data_country <- reactive({
        data_country<-mydata[mydata$country==input$country & mydata$quantity>0, c("year", "quantity")]
        data_country<-aggregate(data_country$quantity, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Παραγωγή")
        data_country
    })
    data_year <- reactive({
        data_year<-mydata[mydata$year==input$year & mydata$quantity>0,  c("country", "quantity")]
        data_year<-aggregate(data_year$quantity, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Παραγωγή")
        data_year
    })
    output$view <- renderGvis({
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Παραγωγή (τόνοι)'}", hAxis="{title:'Έτος'}",
                                                     backgroundColor="#d9ffb3", width=800, height=700, legend='none'))
    })
    output$map <- renderGvis({
        GeoStates <- gvisGeoChart(data_year(), "Χώρα", "Παραγωγή", 
                                  options=list(region="150", displayMode="regions", datamode='regions', width=800, height=700))
    })
    output$table <- renderDataTable({
        colnames(mydata)<-c("Χώρα", "Έτος", "Παραγωγή")
        mydata
    })
}
shinyApp(ui, server)