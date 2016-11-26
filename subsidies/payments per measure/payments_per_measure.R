library(shiny)
library(googleVis)
require("RPostgreSQL")

mydata <- read.csv("./test.csv", sep=";")

ui <- fluidPage(
  headerPanel("Πληρωμές ΟΠΕΚΕΠΕ"),
  sidebarPanel(
    selectInput('x', 'Μέτρο', choices = unique(mydata$measure), selected = "Ενιαία Ενίσχυση 2013")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Διάγραμμα", htmlOutput("view")),
      tabPanel("Πίνακας Δεδομένων", dataTableOutput("table"))
    )
  ))

server <- function(input, output) {
  #add reactive data information
  dataset <- reactive({
    dataset<-input$x
  })
  output$view <- renderGvis({
    mpgData <- mydata[mydata$measure == dataset(), ]
    mpgData<-aggregate(mpgData$payment_amount, by=list(measure=mpgData$measure), FUN=sum)
    gvisBarChart(mpgData, options=list(width=800, height=850))
  })
  # Generate an HTML table view of the data
  output$table <- renderDataTable({
    mydata
  })
}
shinyApp(ui, server)