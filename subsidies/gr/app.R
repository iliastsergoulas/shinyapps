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
library(ggplot2)
library(stringr)

valueFormatter<-"function formatValue(v) {
var suffixes = ['', 'χιλ', 'εκατ', 'δις', 'τρις'];
if (v < 1000) return v;
var magnitude = Math.ceil(String(Math.floor(v)).length / 3-1);
if (magnitude > suffixes.length - 1)
magnitude = suffixes.length - 1;
return String(Math.round(v / Math.pow(10, magnitude * 3), 2)) +suffixes[magnitude]}"
printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # Loading the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # Creating a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.payments") # Getting data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata$date <- dmy(mydata$date) # Converting character to date
mydata<-mydata[order(mydata$date),]
lastdate=max(mydata$date)
header <- dashboardHeader(title = paste0("Πορεία Προγραμμάτων Επιδότησης (τελευταία ημερομηνία ενημέρωσης ",lastdate, ")"), 
                          titleWidth=1000) # Header of dashboard
sidebar <- dashboardSidebar(sidebarMenu(
    selectInput('fund', 'Ταμείο', choices = unique(mydata$fund),selected='ΕΓΤΑΑ'),
    uiOutput("slider_category"),
    uiOutput("slider_measure")),
    selectInput('period', 'Ορίζοντας πρόβλεψης (μήνες)', choices = c("6", "12", "18", "24", "30", "36"), selected='12'),
    tags$footer(tags$p("Η παρούσα εφαρμογή βασίζεται σε επεξεργασμένα δεδομένα από Δελτία Τύπου του ΟΠΕΚΕΠΕ. 
               Το agristats.eu δε φέρει καμία ευθύνη για την ποιότητα των πρωτογενών δεδομένων. Πάγια θέση του είναι ότι η 
                       πληροφορία αυτή ειναι δημόσιο αγαθό και πρέπει να προσφέρεται υπό μορφή ανοιχτών επεξεργάσιμων δεδομένων.")))
frow1 <- fluidRow( # Creating row of two diagrams
    tabsetPanel(
        tabPanel("Χρονική αποτύπωση",dygraphOutput("view_total"),
            dygraphOutput("view"),
            dataTableOutput("summary"),width='98%'),
        tabPanel("Στατιστικά ανά πρόγραμμα",print("Τα εν λόγω στατιστικά αφορούν την επιλεγμένη χρονική περίοδο."),
                 dateRangeInput("mydate_category", "Ημερομηνία:",min=as.character(min(mydata$date)), max=as.character(max(mydata$date)), 
                                start=as.character(min(mydata$date)),end=as.character(max(mydata$date)), sep=""),
                 plotOutput("chart_category"), dataTableOutput("data_category"), width='98%'),
        tabPanel("Στατιστικά ανά μέτρο",print("Τα εν λόγω στατιστικά αφορούν την επιλεγμένη χρονική περίοδο."),
                 dateRangeInput("mydate_measure", "Ημερομηνία:",min=as.character(min(mydata$date)), max=as.character(max(mydata$date)), 
                                start=as.character(min(mydata$date)),end=as.character(max(mydata$date)), sep=""),
                 plotOutput("chart_measure"), dataTableOutput("data_measure"), width='98%'),
        tabPanel("Αντιπαραβολή προγραμμάτων κατ' έτος", plotOutput("category_per_year"), width='98%'),
        tabPanel("Αντιπαραβολή μέτρων κατ' έτος", plotOutput("measure_per_year", height=600), width='98%'),
        tabPanel("Λήψη δεδομένων", downloadButton("downloadData"), width='98%'))
)

body <- dashboardBody(frow1) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_total <- reactive({ # Adding reactive data information
        data_total<-mydata[mydata$category==input$category, c("date", "payment_amount")]
        data_total<-mutate(data_total, total=cumsum(payment_amount))
        data_total<-data_total[c("date", "total")]
        data_total<-xts(data_total, order.by=as.POSIXct(data_total$date))
        data_total})
    data_measure <- reactive({ # Adding reactive data information
        data_measure<-mydata[mydata$measure==input$measure, c("date", "payment_amount")]
        data_measure<-mutate(data_measure, total=cumsum(payment_amount))
        data_measure<-data_measure[c("date", "total")]
        data_measure<-xts(data_measure, order.by=as.POSIXct(data_measure$date))
        data_measure_predicted <- forecast(as.numeric(data_measure$total), h=as.numeric(input$period))
        data_measure_predicted <- data.frame(Date = seq(mdy('06/30/2017'), by = 'months', length.out = as.numeric(input$period)),
                                       Forecast = data_measure_predicted$mean,Hi_95 = data_measure_predicted$upper[,2],
                                       Lo_95 = data_measure_predicted$lower[,2])
        data_measure_xts <- xts(data_measure_predicted, order.by = as.POSIXct(data_measure_predicted$Date))
        data_measure_predicted <- merge(data_measure, data_measure_xts)
        data_measure <- data_measure_predicted[,c("total", "Forecast", "Hi_95", "Lo_95")]
        data_measure})
    data_per_category <- reactive({ # Adding reactive data information
        data_per_category<-mydata[which(mydata$date>=input$mydate_category[1] & mydata$date<=input$mydate_category[2]),]
        data_per_category<-aggregate(data_per_category$payment_amount, by=list(category=data_per_category$category), FUN=sum)})
    data_per_measure <- reactive({ # Adding reactive data information
        data_per_measure<-mydata[which(mydata$date>=input$mydate_category[1] & mydata$date<=input$mydate_category[2] 
                                       & mydata$category==input$category),]
        data_per_measure<-aggregate(data_per_measure$payment_amount, by=list(measure=data_per_measure$measure), FUN=sum)
        data_per_measure$percentage<-round((data_per_measure$x/sum(data_per_measure$x))*100,2)
        data_per_measure})
    measure_per_year <- reactive({ # Adding reactive data information
        measure_per_year<-mydata[which(mydata$category==input$category),]
        measure_per_year<-measure_per_year[,c("measure","date", "payment_amount")]
        measure_per_year$date<-year(measure_per_year$date)
        measure_per_year<-aggregate(measure_per_year$payment_amount,
                                 by=list(measure=measure_per_year$measure,year=measure_per_year$date), FUN=sum)})
    category_per_year <- reactive({ # Adding reactive data information
        category_per_year<-mydata
        category_per_year<-category_per_year[,c("category","date", "payment_amount")]
        category_per_year$date<-year(category_per_year$date)
        category_per_year<-aggregate(category_per_year$payment_amount,
                                 by=list(category=category_per_year$category,year=category_per_year$date), FUN=sum)})
    # All output to be plotted or printed
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
    output$chart_category<-renderPlot({ # Creating timeline per category
        ggplot(data_per_category(), aes(x=data_per_category()$category, y=data_per_category()$x, fill=data_per_category()$category))+
            geom_bar(width = 1, stat = "identity",position = "dodge") + 
            theme(legend.title=element_blank()) + 
            labs(y='Πληρωμές (ευρώ)') + 
            geom_text(aes(label=printMoney(data_per_category()$x)), position=position_dodge(width=0.9), vjust=-0.25) + 
            theme(axis.text=element_blank()) + 
            theme(axis.title.x = element_blank())
    })
    output$data_category <- renderDataTable({ # Creating summary by water_type
        data_category<-data_per_category()
        data_category$x<-printMoney(data_category$x)
        names(data_category)<-c("Πρόγραμμα", "Πληρωμές")
        data_category}, options = list(lengthMenu = c(20, 40, 60), pageLength = 10))
    output$chart_measure<-renderPlot({ # Creating timeline per category
        data_per_measure<-data_per_measure()[,c("measure","x")]
        ggplot(data_per_measure, aes(x=data_per_measure$measure, y=data_per_measure$x/1000000, fill=data_per_measure$measure))+
            geom_bar(width = 1, stat = "identity",position = "dodge") + 
            theme(legend.title=element_blank(), legend.position='bottom') + 
            labs(y='Πληρωμές (εκατ. ευρώ)') + 
            geom_text(aes(label=round(data_per_measure$x/1000000,2)), position=position_dodge(width=0.9), vjust=-0.25) + 
            theme(axis.text.x=element_blank()) + 
            theme(axis.title.x = element_blank())
    })
    output$data_measure <- renderDataTable({ # Creating summary by water_type
        data_measure<-data_per_measure()
        data_measure$x<-printMoney(data_measure$x)
        names(data_measure)<-c("Μέτρο", "Πληρωμές","Ποσοστό συμμετοχής σε συνολικές πληρωμές προγράμματος")
        data_measure}, options = list(lengthMenu = c(20, 40, 60), pageLength = 10))
    output$measure_per_year<-renderPlot({ # Creating timeline per category
        ggplot(measure_per_year(), aes(x=measure_per_year()$measure, y=measure_per_year()$x/1000000))+
            geom_bar(aes(fill=as.character(measure_per_year()$year)), stat = "identity",position = "dodge") + 
            theme(legend.title=element_blank()) +
            coord_flip() + 
            labs(y='Πληρωμές (εκατ. ευρώ)') + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title=element_blank())
    })
    output$category_per_year<-renderPlot({ # Creating timeline per category
        ggplot(category_per_year(), aes(x=category_per_year()$category, y=category_per_year()$x/1000000))+
            geom_bar(aes(fill=as.character(category_per_year()$year)), stat = "identity",position = "dodge") + 
            theme(legend.title=element_blank()) +
            labs(y='Πληρωμές (εκατ. ευρώ)') + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
    })
}
shinyApp(ui, server)