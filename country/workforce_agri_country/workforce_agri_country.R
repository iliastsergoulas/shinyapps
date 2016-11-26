library(shiny)
library(googleVis)
library(shinythemes)

mydata<-read.csv("./workforce_agri_country.csv", sep=",")

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
  # Add reactive data information
  data_country <- reactive({
    data_country<-mydata[mydata$country==input$country 
                         & mydata$workforce_type=="pers: Sole holders working on the farm" 
                         & mydata$number>0, c("year", "number")]
    data_country<-aggregate(data_country$number, by=list(Year=data_country$year), FUN=sum)
  })
  data_year <- reactive({
    data_year<-mydata[mydata$year==input$year  
                      & mydata$workforce_type=="pers: Sole holders working on the farm" 
                      & mydata$number>0, c("country", "number")]
    data_year<-aggregate(data_year$number, by=list(Country=data_year$country), FUN=sum)
  })
  output$view <- renderGvis({
    gvisColumnChart(data_country(), options=list(colors="['#3399ff']", 
              titleTextStyle="{color:'#3399ff', fontName:'Courier', fontSize:22}", 
              title="Αυτοαπασχολούμενοι αγροτικού τομέα", backgroundColor="#e6ffff", width=900, height=950))
  })
  output$map <- renderGvis({
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