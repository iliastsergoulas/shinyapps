library(shiny)
library(googleVis)
library(dplyr)
library(ggplot2)

mydata <- read.csv("./test.csv", sep=";")

ui <- fluidPage(
    headerPanel("Πληρωμές ΟΠΕΚΕΠΕ"),
    sidebarPanel(
        selectInput('x', 'Μέτρο', choices = unique(mydata$measure), selected = "Ενιαία Ενίσχυση 2013")
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", htmlOutput("map")), 
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
        gvisBarChart(mpgData, options=list(width=800, height=850))
    })
    output$map <- renderGvis({
        gvisGeoChart(Andrew, locationvar="LatLong", hovervar="Category", 
                     options=list(height=350, region="GR", dataMode="regions"))
    })
    # Generate an HTML table view of the data
    output$table <- renderDataTable({
        mydata[mydata$measure == dataset(), ]
    })
}
shinyApp(ui, server)