# This R script is created as a Shiny application based on processed data from OPEKEPE's announcements 
# about Agricultural Development, Fisheries, Direct Payments and Market Measures.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(shinydashboard)
library(forecast)
library(dygraphs)
library(corrplot)
library(xts)
library(htmlwidgets)
library(RPostgreSQL)

valueFormatter<-"function formatValue(v) {
var suffixes = ['', 'χιλ', 'εκατ', 'δις', 'τρις'];
if (v < 1000) return v;
var magnitude = Math.ceil(String(Math.floor(v)).length / 3-1);
if (magnitude > suffixes.length - 1)
magnitude = suffixes.length - 1;
return String(Math.round(v / Math.pow(10, magnitude * 3), 2)) +suffixes[magnitude]}"

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.payments") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata$date <- dmy(mydata$date) # Converting character to date
lastdate=max(mydata$date)
header <- dashboardHeader(title = paste0("Πορεία Προγραμμάτων Επιδότησης (τελευταία ημερομηνία ενημέρωσης ",lastdate, ")"), 
                          titleWidth=1000) # Header of dashboard
sidebar <- dashboardSidebar(sidebarMenu(
    selectInput('fund', 'Ταμείο', choices = unique(mydata$fund)),
    uiOutput("slider_category"),
    uiOutput("slider_measure")),
    selectInput('period', 'Ορίζοντας πρόβλεψης (μήνες)', choices = c("6", "12", "18", "24", "30", "36"), selected='12'),
    tags$footer(tags$p("Η παρούσα εφαρμογή βασίζεται σε επεξεργασμένα δεδομένα από Δελτία Τύπου του ΟΠΕΚΕΠΕ. 
               Το agristats.eu δε φέρει καμία ευθύνη για την ποιότητα των δεδομένων.")))
frow1 <- fluidRow( # Creating row of two diagrams
    title = "Συνολικά",
    status="success",
    collapsible = TRUE,
    theme = shinytheme("spacelab"), 
    mainPanel(
        dygraphOutput("view_total"),width='98%')
)
frow2 <- fluidRow( # Creating row of two diagrams
    title = "Συνολικά",
    status="success",
    collapsible = TRUE,
    theme = shinytheme("spacelab"), 
    mainPanel(
        dygraphOutput("view"),width='98%')
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "Σύνοψη δεδομένων ανά Μέτρο",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),width=550)),

    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData")))
)

body <- dashboardBody(frow1, frow2, frow3) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_total <- reactive({ # Adding reactive data information
        data_total<-mydata[mydata$category==input$category, c("date", "payment_amount")]
        data_total<-mutate(data_total, total=cumsum(payment_amount))
        data_total<-data_total[c("date", "total")]
        data_total<-xts(data_total, order.by=as.POSIXct(data_total$date))
        data_total
    })
    data_measure <- reactive({ # Adding reactive data information
        data_measure<-mydata[mydata$measure==input$measure, c("date", "payment_amount")]
        data_measure<-mutate(data_measure, total=cumsum(payment_amount))
        data_measure<-data_measure[c("date", "total")]
        data_measure<-xts(data_measure, order.by=as.POSIXct(data_measure$date))
        data_measure_predicted <- forecast(as.numeric(data_measure$Value), h=as.numeric(input$period))
        data_measure_predicted <- data.frame(Date = seq(mdy('06/30/2017'), by = 'months', length.out = as.numeric(input$period)),
                                       Forecast = data_measure_predicted$mean,Hi_95 = data_measure_predicted$upper[,2],
                                       Lo_95 = data_measure_predicted$lower[,2])
        data_measure_xts <- xts(data_measure_predicted, order.by = as.POSIXct(data_measure_predicted$Date))
        data_measure_predicted <- merge(data_measure, data_measure_xts)
        data_measure <- data_measure_predicted[,c("Value", "Forecast", "Hi_95", "Lo_95")]
        data_measure
    })
    output$view_total <- renderDygraph({ # Creating chart
        dygraph(data_total()$total, main=paste0("Πορεία Προγράμματος: ", input$category))%>%
            dyAxis("y", label = "Πληρωμές", axisLabelFormatter=JS(valueFormatter))%>%
            dyRangeSelector(height = 20)
    })
    output$view <- renderDygraph({ # Creating chart
        dygraph(data_measure()$total, main=paste0("Πορεία απορρόφησης: ", input$measure))%>%
            dyAxis("y", label = "Πληρωμές", axisLabelFormatter=JS(valueFormatter))%>%
            dyRangeSelector(height = 20)
    })
    output$summary <- renderDataTable({ # Creating summary by measure
        mysummary <- data.frame(
            aggregate(payment_amount~measure, mydata, min),
            aggregate(payment_amount~measure, mydata, max),
            aggregate(payment_amount~measure, mydata, mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Μέτρο", "Ελάχιστη απορρόφηση ημέρας", "Μέγιστη απορρόφηση ημέρας", "Μέση απορρόφηση ημέρας")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 3))
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() {paste('mydata', '.csv', sep='')},
        content = function(file) {
            write.csv(mydata, file)
    })
    output$slider_category <- renderUI({
        selectInput('category', 'Κατηγορία', choices = unique(mydata[which(mydata$fund==input$fund),]$category))
    })
    output$slider_measure <- renderUI({
        selectInput('measure', 'Μέτρο', choices = unique(mydata[which(mydata$category==input$category),]$measure))
    })
}
shinyApp(ui, server)