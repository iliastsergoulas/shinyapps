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

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.honey_production_minagric") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata_processed<-melt(mydata, id.vars=c("Έτος", "Περιφέρεια", "Περιφερειακή Ενότητα"))
names(mydata_processed)<-c("year","region", "prefecture", "category", "value")
total_per_year<-aggregate(as.numeric(mydata_processed$value), 
                          by=list(year=mydata_processed$year), FUN=sum, na.rm=TRUE)
mean_value<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
header <- dashboardHeader(title = "Παραγωγή μελιού, αριθμός κυψελών και συνολική αξία παραγωγής στην Ελλάδα", titleWidth=750) # Header of dashboard
sidebar <- dashboardSidebar(sidebarMenu(
    selectInput('var_x', 'Χ', choices = c("Αριθμός κυψελών", "Παραγωγή μελιού (τν)", "Συνολική αξία παραγωγής")),
    selectInput('var_y', 'Y', choices = c("Παραγωγή μελιού (τν)", "Αριθμός κυψελών", "Συνολική αξία παραγωγής"))))
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean_value", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    status="success",
    theme = shinytheme("spacelab"), 
    mainPanel(
        htmlOutput("motion_region"),
        print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"))
)
frow3 <- fluidRow( # Creating row of two diagrams
    status="success",
    theme = shinytheme("spacelab"), 
    mainPanel(
        htmlOutput("motion_prefecture"),
        print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"))
)
frow4 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData"))))

body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_region <- reactive({ # Adding reactive data information
        data_region<-aggregate(list(mydata$`Αριθμός κυψελών`,mydata$`Παραγωγή μελιού (τν)`, mydata$`Συνολική αξία παραγωγής`), 
                               by=list(mydata$Έτος, mydata$Περιφέρεια), 
                               FUN=sum, na.rm=TRUE)
        names(data_region)<-c("Έτος", "Περιφέρεια", "Αριθμός κυψελών", "Παραγωγή μελιού (τν)", "Συνολική αξία παραγωγής")
        data_region$`Αριθμός κυψελών`<-as.numeric(data_region$`Αριθμός κυψελών`)
        data_region
    })
    data_prefecture <- reactive({ # Adding reactive data information
        data_prefecture<-aggregate(list(mydata$`Αριθμός κυψελών`,mydata$`Παραγωγή μελιού (τν)`, mydata$`Συνολική αξία παραγωγής`), 
                               by=list(mydata$Έτος, mydata$`Περιφερειακή Ενότητα`), 
                               FUN=sum, na.rm=TRUE)
        names(data_prefecture)<-c("Έτος", "Περιφερειακή Ενότητα", "Αριθμός κυψελών", "Παραγωγή μελιού (τν)", "Συνολική αξία παραγωγής")
        data_prefecture$`Αριθμός κυψελών`<-as.numeric(data_prefecture$`Αριθμός κυψελών`)
        data_prefecture
    })
    output$motion_region<-renderGvis({
        gvisMotionChart(data_region(), xvar=input$var_x, yvar=input$var_y,
                        idvar="Περιφέρεια", timevar="Έτος",
                        options=list(width=1000, height=500))
    })
    output$motion_prefecture<-renderGvis({
        gvisMotionChart(data_prefecture(), xvar="Αριθμός κυψελών", yvar="Παραγωγή μελιού (τν)",
                        idvar="Περιφερειακή Ενότητα", timevar="Έτος",
                        options=list(width=1000, height=500))
    })
    output$mean_value <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(mean_value,2), " τόνοι"),
            "Μέση παραγωγή μελιού ετησίως",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$year," - ", printMoney(topyear$x)),
            "Έτος με μέγιστη παραγωγή μελιού",
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