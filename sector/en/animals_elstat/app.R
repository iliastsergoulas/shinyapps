# Data: Animals population based on data from Hellenic Statistic Service
# This R script is created as a Shiny application to download raw data from Hellenic Statistical Authority (ELSTAT), , 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleVis)
library(ggplot2)
library(directlabels)
library(scales)
library(reshape2)
library(RPostgreSQL)

printMoney <- function(x){ # A function to show quantity as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show quantity with k decimal places

#credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
#drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
#con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 #host = as.character(credentials$host), port = as.character(credentials$port), 
                 #user = as.character(credentials$user), password = as.character(credentials$password))
#mydata <- dbGetQuery(con, "SELECT * from agriculture.animal_elstat") # Get data
#dbDisconnect(con)
#dbUnloadDriver(drv)
mydata<-read.csv("C://Users/itsergoulas/Desktop/animals_gr.csv", sep=";")
mydata$year<-as.character(mydata$year)
names(mydata)<-c("year", "region", "prefecture","variable_type", "Βοοειδή", "Βουβάλια", "Προβατοειδή", "Αιγοειδή", "Χοίροι",
                 "Ιπποειδή", "Κονικλοειδή","Πουλερικά", "Κυψέλες μελισσών")
mydata_processed<-melt(mydata, id.vars=c("year", "region", "prefecture", "variable_type"))
names(mydata_processed)<-c("year", "region", "prefecture", "variable_type", "variable", "value")
header <- dashboardHeader(title = "Ζωικό κεφάλαιο στην Ελλάδα", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Πορεία ζωικού πληθυσμού ανά είδος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("timeline_category"),
            print("Πηγή: ΕΛΣΤΑΤ"),
            selectInput('variable_type', 'Μεταβλητή', choices = unique(mydata_processed$variable_type)),
            selectInput('animal_category', 'Κατηγορία', choices = unique(mydata_processed$variable)), width='98%')),
    box(
        title = "Πορεία ζωικού πληθυσμού ανά έτος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("timeline_year"),
            print("Πηγή: ΕΛΣΤΑΤ"),
            selectInput('variable_type', 'Μεταβλητή', choices = unique(mydata_processed$variable_type)),
            selectInput('year', 'Έτος', choices = unique(mydata_processed$year)), width='98%'))
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Σύνοψη δεδομένων ανά ζωικό είδος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Πηγή: ΕΛΣΤΑΤ"),
            sliderInput("myyearsummary", "Έτος:",min=min(as.numeric(mydata_processed$year)), 
                        max=max(as.numeric(mydata_processed$year)), 
                        value=c(min(as.numeric(mydata_processed$year))+1,max(as.numeric(mydata_processed$year))-1), sep=""))),
    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData"))))

body <- dashboardBody(frow1, frow2) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    mydata_processed_timeline_category<-reactive({ # Filtering data by chosen category
        mydata_processed_timeline_category<-mydata_processed[which(mydata_processed$variable==input$animal_category & 
                                            mydata_processed$variable_type==input$variable_type),]
        mydata_processed_timeline_category<-aggregate(mydata_processed_timeline_category$value,
                                                      by=list(mydata_processed_timeline_category$year), 
                                                      FUN=sum, na.rm=TRUE,na.action=NULL)
    })
    mydata_processed_timeline_year<-reactive({ # Filtering data by chosen region
        mydata_processed_timeline_year<-mydata_processed[which(mydata_processed$year==input$year & 
                                            mydata_processed$variable_type==input$variable_type),]
        mydata_processed_timeline_year<-aggregate(mydata_processed_timeline_year$value,
                                                    by=list(mydata_processed_timeline_year$variable), 
                                                    FUN=sum, na.rm=TRUE, na.action=NULL)
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata_processed[which(mydata_processed$year>=input$myyearsummary[1] & 
                        mydata_processed$year<=input$myyearsummary[2] & 
                        mydata_processed$variable_type==input$variable_type),] 
    })
    output$timeline_category<-renderGvis({ # Creating timeline per category
        gvisColumnChart(mydata_processed_timeline_category(), options=list(colors="['#336600']", vAxis="{title:'Πληθυσμός'}", 
                        hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$timeline_year<-renderGvis({ # Creating timeline per type
        gvisColumnChart(mydata_processed_timeline_year(), options=list(colors="['#336600']", vAxis="{title:'Πληθυσμός'}", 
                        hAxis="{title:'Ζωικό είδος'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$motion<-renderGvis({
        gvisMotionChart(mydata_category, xvar="Αριθμός εκμεταλλεύσεων", yvar="Ποσοστό",
                        idvar="Κατηγορία", timevar="Έτος")
    })
    output$summary <- renderDataTable({ # Creating summary by
        mysummary <- data.frame(
            aggregate(value~variable, mydata_summary(), min),
            aggregate(value~variable, mydata_summary(), max),
            aggregate(value~variable, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Είδος", "Ελάχιστος πληθυσμός", "Μέγιστος πληθυσμός", "Μέσος πληθυσμός")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
    })
}
shinyApp(ui, server)