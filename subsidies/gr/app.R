# This R script is created as a Shiny application based on processed data from OPEKEPE's announcements 
# about Fisheries Programme.
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

valueFormatter<-"function formatValue(v) {
var suffixes = ['', 'χιλ', 'εκατ', 'δις', 'τρις'];
if (v < 1000) return v;
var magnitude = Math.ceil(String(Math.floor(v)).length / 3-1);
if (magnitude > suffixes.length - 1)
magnitude = suffixes.length - 1;
return String(Math.round(v / Math.pow(10, magnitude * 3), 2)) +suffixes[magnitude]}"

mydata<-read.csv("/home/iliastsergoulas/Dropbox/Website/payments.csv", sep=",", encoding="UTF-8", stringsAsFactors = FALSE)
#mydata<-mydata[which(mydata$category=='Πληρωμές ΕΤΑ 2007-2013'), ] # Filtering data
#mydata<-mydata[c("date", "measure", "payment_amount")]
mydata$date <- dmy(mydata$date) # Converting character to date
lastdate=max(mydata$date)
header <- dashboardHeader(title = paste0("Πορεία Προγραμμάτων Επιδότησης (τελευταία ημερομηνία ενημέρωσης ",lastdate, ")"), 
                          titleWidth=1000) # Header of dashboard
sidebar <- dashboardSidebar(sidebarMenu(
    selectInput('fund', 'Ταμείο', choices = unique(mydata$fund)),
    uiOutput("slider_category"),
    uiOutput("slider_measure")
))
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
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
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