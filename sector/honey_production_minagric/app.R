# Data: Honey production based on data by Greek Ministry of Agricultural Production and Food
# This R script is created as a Shiny application to process data 
# from Hellenic Ministry of Agricultural Development and Food and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleVis)
library(RPostgreSQL)
library(ggplot2)
library(directlabels)
library(scales)
library(reshape2)

printMoney <- function(x){ # A function to show quantity as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show quantity with k decimal places

#credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
#drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
#con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 #host = as.character(credentials$host), port = as.character(credentials$port), 
                 #user = as.character(credentials$user), password = as.character(credentials$password))
#mydata_processed <- dbGetQuery(con, "SELECT * from agriculture.honey_production_minagric") # Get data
mydata<-read.csv("C://Users/itsergoulas/Dropbox/Website/scripts to complete/honey_production.csv", 
                 stringsAsFactors = FALSE, sep=";")
#dbDisconnect(con)
#dbUnloadDriver(drv)
mydata_processed<-melt(mydata, id.vars=c("Έτος", "Περιφέρεια", "Περιφερειακή.Ενότητα"))
mydata_processed$value<-as.numeric(mydata_processed$value)
names(mydata_processed)<-c("year","region", "prefecture", "category", "value")
total_per_year<-aggregate(as.numeric(mydata_processed$value), 
                          by=list(year=mydata_processed$year), FUN=sum, na.rm=TRUE)
mean _value<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
header <- dashboardHeader(title = "Παραγωγή μελιού, αριθμός κυψελών και συνολική αξία παραγωγής στην Ελλάδα", titleWidth=750) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean _value", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Αριθμός μελισσοκόμων ανά Περιφερειακή Ενότητα",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("motion"),
            print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων")))
)
frow3 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Αριθμός μελισσοκόμων ανά Περιφέρεια",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("per_region"),
            print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"), width='98%')),
    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData"))))

body <- dashboardBody(frow1, frow2) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_processed[mydata_processed$year==input$year,]
        data_year<-aggregate(data_year$value, by=list(data_year$region), FUN=sum, na.rm=TRUE)
    })
    data_region <- reactive({ # Adding reactive data information
        data_region<-aggregate(mydata_processed$value, 
                             by=list(mydata_processed$region, as.character(mydata_processed$year)), FUN=sum, na.rm=TRUE)
    })
    data_prefecture <- reactive({ # Adding reactive data information
        data_prefecture<-mydata_processed[mydata_processed$prefecture==input$prefecture,]
        data_prefecture<-aggregate(data_prefecture$value, by=list(data_prefecture$year), FUN=sum, na.rm=TRUE)
    })
    output$per_year <- renderGvis({ # Creating chart
        gvisColumnChart(data_year(), options=list(colors="['#336600']", vAxis="{title:'Αριθμός μελισσοκόμων'}", 
                        hAxis="{title:'Περιφέρεια'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$motion<-renderGvis({
        print(mydata)
        gvisMotionChart(mydata, xvar="Αριθμός.κυψελών", yvar="Παραγωγή.μελιού..τν.",
                        idvar="Περιφέρεια", timevar="Έτος")
    })
    output$mean _value <- renderValueBox({ # Filling valuebox
        valueBox(
            specify_decimal(mean _value,2),
            "Μέσος αριθμός μελισσοκόμων ετησίως",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$year," - ", printMoney(topyear$x)),
            "Έτος με μέγιστο αριθμό μελισσοκόμων",
            icon = icon("globe"),
            color = "olive")
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)