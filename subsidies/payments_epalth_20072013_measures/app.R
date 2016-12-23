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

mydata<-read.csv("C:\\Users\\Ilias Tsergoulas\\Dropbox\\Website\\shiny\\subsidies\\payments_epalth_20072013_measures\\payments.csv", sep=",", encoding="UTF-8", stringsAsFactors = FALSE)
mydata<-mydata[which(mydata$fund=='ΕΤΑ'), ] # Filtering data
mydata<-mydata[c("date", "measure", "payment_amount")]
mydata$date <- dmy(mydata$date) # Converting character to date

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel( # Creating sidebar panel with conditions
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('measure', 'Μέτρο', choices = unique(mydata$measure), selected = "Greece")),
        conditionalPanel(condition="input.conditionedPanels == 'Δεδομένα'", downloadButton("downloadData")),
        conditionalPanel(condition="input.conditionedPanels == 'Χρονοσειρά' || input.conditionedPanels == 'Σύνοψη ανά Μέτρο'", 
                         dateRangeInput("mydate", "Ημερομηνία:",
                                        start = min(mydata$date),
                                        end = Sys.Date())),
        width=2),
    mainPanel(
        tabsetPanel( # Creating tabs
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χρονοσειρά", plotOutput("timeline")),
            tabPanel("Δεδομένα", dataTableOutput("table")),
            tabPanel("Σύνοψη ανά Μέτρο", dataTableOutput("summary")),
            id = "conditionedPanels"),
        print("Πηγή: Δελτία τύπου ΟΠΕΚΕΠΕ")))

server <- function(input, output) {
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
        data_date_temp<-data_date_temp[1:5,] # Keeping top five measures
        mydata_top_five<-mydata[which(mydata$measure %in% data_date_temp$measure),]
        mydata_top_five<-mutate(mydata_top_five, cumsum=cumsum(payment_amount))
        print(mydata_top_five)
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$date>=input$mydate[1] & mydata$date<=input$mydate[2]),]
    })
    output$view <- renderGvis({ # Creating chart
        gvisLineChart(data_measure(), options=list(colors="['#336600']", title="Απορρόφηση πόρων Επιχειρησιακού Προγράμματος Αλιείας 2007-2013 ανά Μέτρο", 
                                                     titleTextStyle="{color:'#336600',fontSize:14}", vAxis="{title:'Πληρωμές (Ευρώ)'}", 
                                                     hAxis="{title:'Ημερομηνία'}",backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Ημερομηνία", "Μέτρο", "Πληρωμές")
        mydata
    })
    output$summary <- renderDataTable({ # Creating summary by measure
        mysummary <- data.frame(
            aggregate(payment_amount~measure, mydata_summary(), min),
            aggregate(payment_amount~measure, mydata_summary(), max),
            aggregate(payment_amount~measure, mydata_summary(), mean),
            aggregate(payment_amount~measure, mydata_summary(), median))
        mysummary <- mysummary[,c(1,2,4,6,8)]
        colnames(mysummary) <- c("Μέτρο", "Ελάχιστη απορρόφηση ημέρας", "Μέγιστη απορρόφηση ημέρας", "Μέση απορρόφηση ημέρας", "Διάμεσος")
        mysummary
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = date, y = cumsum, group = measure, colour = measure)) + 
            geom_line(linemitre=2) +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Ημερομηνία") + ylab("Πληρωμές (Ευρώ)") + ggtitle("5 μέτρα με μεγαλύτερη απορρόφηση πόρων") + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) + 
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() {paste('mydata', '.csv', sep='')},
        content = function(file) {
            write.csv(mydata, file)
        })
}
shinyApp(ui, server)