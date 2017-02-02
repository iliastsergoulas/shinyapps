# This R script is created as a Shiny application to download raw data from World Bank through WDI package, 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(ggplot2)
library(directlabels)
library(scales)
library(dplyr)
library(lubridate)
library(shinydashboard)

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}

mydata<-read.csv("./payments.csv", sep=",", encoding="UTF-8", stringsAsFactors = FALSE)
mydata<-mydata[which(mydata$category=='Άμεσες Ενισχύσεις'), ] # Filtering data
mydata<-mydata[c("date", "measure", "payment_amount")]
mydata$date <- dmy(mydata$date) # Converting character to date

total_payments<-sum(mydata$payment_amount) # Total value
topm<-mydata[which.max(mydata$payment_amount),] # Top measure
header <- dashboardHeader(title = "Πορεία Άμεσων Ενισχύσεων", titleWidth=600) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of infoBoxes
    infoBoxOutput("totalpayments", width=6),
    infoBoxOutput("topmeasure", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Συνολικά",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view_total"),
            print("Πηγή: Δελτία τύπου ΟΠΕΚΕΠΕ"), width='98%')),
    box(
        title = "Ανά Μέτρο",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view"),
            print("Πηγή: Δελτία τύπου ΟΠΕΚΕΠΕ"),
            selectInput('measure', 'Μέτρο', choices = unique(mydata$measure)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "3 μέτρα με μεγαλύτερη απορρόφηση πόρων",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline"),
            print("Πηγή: Δελτία τύπου ΟΠΕΚΕΠΕ"),
            dateRangeInput("mydate", "Ημερομηνία:", start = min(mydata$date), end = Sys.Date()), width='98%')),
    box(
        title = "Σύνοψη δεδομένων ανά Μέτρο",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Πηγή: Δελτία τύπου ΟΠΕΚΕΠΕ")))
)
frow4 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData")))
)

body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_total <- reactive({ # Adding reactive data information
        data_total<-mutate(mydata, cumsum=cumsum(payment_amount))
        data_total<-data_total[c("date", "cumsum")]
        colnames(data_total)<-c("Ημερομηνία", "Καταβληθείσα Δ.Δ.")
        data_total
    })
    data_measure <- reactive({ # Adding reactive data information
        data_measure<-mydata[mydata$measure==input$measure, c("date", "payment_amount")]
        data_measure<-mutate(data_measure, cumsum=cumsum(payment_amount))
        data_measure<-data_measure[c("date", "cumsum")]
        colnames(data_measure)<-c("Ημερομηνία", "Καταβληθείσα Δ.Δ.")
        data_measure
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        # Filtering out groups of countries
        data_date_temp<-mydata[which(mydata$date>=input$mydate[1] & mydata$date<=input$mydate[2]),]
        data_date_temp<-aggregate(data_date_temp$payment_amount, by=list(measure=data_date_temp$measure), FUN=sum)
        data_date_temp<-data_date_temp[order(-data_date_temp$x),]
        data_date_temp<-data_date_temp[1:3,] # Keeping top three measures
        mydata_top_five<-mydata[which(mydata$measure %in% data_date_temp$measure),]
        mydata_top_five<-mutate(mydata_top_five, cumsum=cumsum(payment_amount))
        mydata_top_five
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$date>=input$mydate[1] & mydata$date<=input$mydate[2]),]
    })
    output$view <- renderGvis({ # Creating chart
        gvisLineChart(data_measure(), options=list(colors="['#336600']", vAxis="{title:'Πληρωμές (Ευρώ)'}", 
                    hAxis="{title:'Ημερομηνία'}",backgroundColor="#d9ffb3", width=500, height=500, legend='none'))
    })
    output$view_total <- renderGvis({ # Creating chart
        gvisLineChart(data_total(), options=list(colors="['#336600']", vAxis="{title:'Πληρωμές (Ευρώ)'}", 
                                                   hAxis="{title:'Ημερομηνία'}",backgroundColor="#d9ffb3", width=500, height=500, legend='none'))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Ημερομηνία", "Μέτρο", "Πληρωμές")
        mydata
    })
    output$summary <- renderDataTable({ # Creating summary by measure
        mysummary <- data.frame(
            aggregate(payment_amount~measure, mydata_summary(), min),
            aggregate(payment_amount~measure, mydata_summary(), max),
            aggregate(payment_amount~measure, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Μέτρο", "Ελάχιστη απορρόφηση ημέρας", "Μέγιστη απορρόφηση ημέρας", "Μέση απορρόφηση ημέρας")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$timeline<-renderPlot({ # Creating timeline for top 3 measures
        ggplot(mydata_top_five(), aes(x = date, y = cumsum, group = measure, colour = measure)) + 
            geom_line(linemitre=2) +
            scale_y_continuous(labels = comma) + 
            xlab("Ημερομηνία") + ylab("Πληρωμές (Ευρώ)") + 
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
            theme(legend.position="bottom", legend.direction='vertical')
    })
    output$totalpayments <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΣΥΝΟΛΙΚΗ ΑΠΟΡΡΟΦΗΣΗ ΠΡΟΓΡΑΜΜΑΤΟΣ",
            value = tags$p(style = "font-size: 20px;", paste0(printMoney(total_payments)," €")),
            fill=TRUE,
            icon = icon("money"),
            color = "olive")
    })
    output$topmeasure <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΜΕΤΡΟ ΜΕ ΜΕΓΑΛΥΤΕΡΗ ΑΠΟΡΡΟΦΗΣΗ ΠΟΡΩΝ",
            value = tags$p(style = "font-size: 20px;", paste0(topm$measure," : ", printMoney(topm$payment_amount)," €")), 
            fill=TRUE,
            icon = icon("globe"),
            color = "olive")
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() {paste('mydata', '.csv', sep='')},
        content = function(file) {
            write.csv(mydata, file)
    })
    output$myImage <- renderImage({
        list(src = "output.jpg",
             contentType = 'image/png',
             width = 500,
             height = 500,
             alt = "This is alternate text")
    })
}
shinyApp(ui, server)