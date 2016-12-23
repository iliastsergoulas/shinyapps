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

mydata<-read.csv("./payments.csv", sep=",", encoding="UTF-8", stringsAsFactors = FALSE)
mydata<-mydata[which(mydata$fund=='ΕΓΤΑΑ'), ] # Filtering data
mydata<-mydata[c("date", "measure", "payment_amount")]
mydata$date <- dmy(mydata$date) # Converting character to date

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel( # Creating sidebar panel with conditions
        conditionalPanel(condition="input.conditionedPanels == 'Δεδομένα'", downloadButton("downloadData")),
        width=2),
    mainPanel(
        tabsetPanel( # Creating tabs
            tabPanel("Συνολική απορρόφηση", htmlOutput("view")),
            tabPanel("Απορρόφηση ανά μήνα και έτος", plotOutput("viewyear")),
            tabPanel("Δεδομένα", dataTableOutput("table")),
            id = "conditionedPanels"),
        print("Πηγή: Δελτία τύπου ΟΠΕΚΕΠΕ")))

server <- function(input, output) {
    data_fund <- reactive({ # Adding reactive data information
        data_fund<-mydata[c("date", "payment_amount")]
        data_fund<-data_fund[order(data_fund$date),]
        data_fund<-mutate(data_fund, cumsum=cumsum(payment_amount))
        data_fund<-data_fund[c("date", "cumsum")]
        colnames(data_fund)<-c("date", "total")
        data_fund
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata[c("date", "payment_amount")]
        data_year<-mutate(data_year, year=year(date))
        data_year<-mutate(data_year, month=month(date, label=TRUE))
        data_year<-data_year[c("year", "month", "payment_amount")]
        data_year<-aggregate(payment_amount~month+year, data_year, sum)
        data_year$year<-as.character(data_year$year)
        colnames(data_year)<-c("month", "year", "total")
        data_year
    })
    output$view <- renderGvis({ # Creating chart
        gvisLineChart(data_fund(), options=list(colors="['#336600']", title="Απορρόφηση πόρων Προγράμματος Αγροτικής Ανάπτυξης 2007-2013", 
                                                titleTextStyle="{color:'#336600',fontSize:14}", vAxis="{title:'Πληρωμές (Ευρώ)'}", 
                                                hAxis="{title:'Ημερομηνία'}", backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$viewyear<-renderPlot({ # Creating cross-check by years
        ggplot(data_year(), aes(x = month, y = total, fill=year)) + 
            geom_bar(stat="identity",position="dodge") + 
            theme(panel.background = element_rect(fill = "#d9ffb3")) + 
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Ημερομηνία") + ylab("Πληρωμές (Ευρώ)") + 
            ggtitle("Απορρόφηση πόρων Προγράμματος Αγροτικής Ανάπτυξης 2007-2013 και αντιπαραβολή ανά μήνα και έτος") + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) + 
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Ημερομηνία", "Μέτρο", "Πληρωμές")
        mydata
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() {paste('mydata', '.csv', sep='')},
        content = function(file) {
            write.csv(mydata, file)
        })
}
shinyApp(ui, server)