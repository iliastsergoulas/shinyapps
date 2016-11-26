library(shiny)
library(googleVis)
library(shinythemes)

mydata<-read.csv("./area_cultivated.csv", sep=",")

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
    data_country<-mydata[mydata$country==input$country & mydata$area>0, c("year", "area")]
    data_country<-aggregate(data_country$area, by=list(Year=data_country$year), FUN=sum)
  })
  data_year <- reactive({
    data_year<-mydata[mydata$year==input$year & mydata$area>0, c("country", "area")]
    data_year<-aggregate(data_year$area, by=list(Country=data_year$country), FUN=sum)
  })
  output$view <- renderGvis({
    gvisColumnChart(data_country(), options=list(colors="['#3399ff']", 
              titleTextStyle="{color:'#3399ff', fontName:'Courier', fontSize:22}", 
              title="Καλλιεργήσιμη γη", backgroundColor="#e6ffff", width=900, height=950))
  })
  output$map <- renderGvis({
    #mapdata<-mydata[mydata$year==2013 & mydata$area>0, c("country", "area")]
    #mapdata<-aggregate(mapdata$area, by=list(Country=mapdata$country), FUN=sum)
    GeoStates <- gvisGeoChart(data_year(), "Country", "x", 
                              options=list(region="150", 
                                           displayMode="regions", 
                                           datamode='regions',
                                           width=900, height=700))
  })
  # Generate an HTML table view of the data
  output$table <- renderDataTable({
    mydata
  })
}
shinyApp(ui, server)