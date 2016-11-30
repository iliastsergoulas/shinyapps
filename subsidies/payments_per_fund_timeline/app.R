library(shiny)
library(googleVis)
library(dplyr)

mydata <- read.csv("./test.csv", sep=";")
print(lapply(mydata, class))
print(mydata)

ui <- fluidPage(
    headerPanel("Πληρωμές ΟΠΕΚΕΠΕ"),
    sidebarPanel(
        selectInput('x', 'Ταμείο', choices = unique(mydata$fund), selected = "ΕΓΤΕ")
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
        mpgData<-mydata[mydata$fund == dataset(), ]
        mpgData %>% group_by(date) %>% summarise(payment_amount = sum(payment_amount))
        #mpgData<-aggregate(mpgData$payment_amount, by=list(date=mpgData$date), FUN=sum)
        gvisLineChart(mpgData, xvar=mpgData$date, yvar=mpgData$x, options=list(width=800, height=850))
    })
    # Generate an HTML table view of the data
    output$table <- renderDataTable({
        mydata
    })
}
shinyApp(ui, server)