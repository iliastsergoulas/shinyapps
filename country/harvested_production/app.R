library(shiny)
library(ggplot2)
library(ggthemes)
library(shinythemes)

mydata <- read.csv("./harvested_production.csv", sep=",")
# Define server logic required to plot various variables against mpg

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    headerPanel("Harvested production per country"),
    sidebarPanel(
        selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")
        # selectInput('y', 'Έτος', choices = unique(mydata$year), selected = "2007")
        # sliderInput('plotHeight', 'Height of plot (in pixels)', min = 100, max = 2000, value = 1000)
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Διάγραμμα", plotOutput("trendPlot")),
            tabPanel("Περίληψη", verbatimTextOutput("summary")), 
            tabPanel("Πίνακας Δεδομένων", dataTableOutput("table"))
        )
    ))

server <- function(input, output) {
    #add reactive data information
    dataset <- reactive({
        dataset<-subset(mydata, country==input$country, select=c(year, sum))
    })
    output$trendPlot <- renderPlot({
        # build graph with ggplot syntax
        ggplot(dataset(), aes(x=dataset()$year, y=dataset()$sum)) + 
            geom_bar(stat="identity", fill="royal blue") + 
            theme_solarized() + scale_colour_solarized() + 
            labs(x="Harvested", y="Hello")
    })
    output$summary <- renderPrint({
        summary(dataset()$sum)
    })
    # Generate an HTML table view of the data
    output$table <- renderDataTable({
        dataset()
    })
}
shinyApp(ui, server)