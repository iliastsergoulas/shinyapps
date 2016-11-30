library(shiny)
library(googleVis)
library(shinythemes)

mydata<-read.csv("./workforce_agri_country.csv", sep=",")
mydata["percentage"]<-(mydata["workforce_size"]/mydata["population"])*100

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel(
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")),
        conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                         selectInput('year', 'Έτος', choices = unique(mydata$year), selected = "2013"))),
    mainPanel(
        tabsetPanel(
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", htmlOutput("map")), 
            tabPanel("Πίνακας Δεδομένων", dataTableOutput("table")),
            tabPanel("Χρονική ανάλυση", htmlOutput("time_map")),
            id = "conditionedPanels"
        )))

server <- function(input, output) {
    # Add reactive data information
    data_country <- reactive({
        data_country<-mydata[mydata$country==input$country 
                             & mydata$workforce_type=="pers: Sole holders working on the farm" 
                             & mydata$workforce_size>0, c("year", "percentage")]
        data_country<-aggregate(data_country$percentage, by=list(Year=data_country$year), FUN=sum)
    })
    data_year <- reactive({
        data_year<-mydata[mydata$year==input$year  
                          & mydata$workforce_type=="pers: Sole holders working on the farm" 
                          & mydata$workforce_size>0, c("country", "percentage")]
        data_year<-aggregate(data_year$percentage, by=list(Country=data_year$country), FUN=sum)
    })
    data_timeline <- reactive({
        data_timeline<-mydata[mydata$workforce_type=="pers: Sole holders working on the farm" 
                              & mydata$workforce_size>0, c("country", "year", "workforce_size", "percentage")]
        data_timeline<-na.omit(data_timeline)
    })
    output$view <- renderGvis({
        gvisColumnChart(data_country(), options=list(colors="['#336600']", backgroundColor="#d9ffb3", width=900, height=950))
    })
    output$map <- renderGvis({
        geochart <- gvisGeoChart(data_year(), "Country", "x", 
                                 options=list(region="150", displayMode="regions", 
                                              datamode='regions', width=900, height=700))
    })
    # Generate an HTML table view of the data
    output$table <- renderDataTable({
        colnames(mydata) <- c("Χώρα", "Έτος", "Τύπος αγροτικής απασχολησης", "Αριθμός απασχολούμενων", "Πληθυσμός", "Ποσοστό αγροτικής απασχόλησης")
        mydata
    })
    output$time_map<-renderGvis({
        geomap<- gvisMotionChart(data_timeline(), idvar = "country", timevar = "year",
                                 xvar = "workforce_size", yvar = "percentage")
    })
}
shinyApp(ui, server)