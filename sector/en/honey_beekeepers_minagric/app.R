# Data: Honey beekeepers based on data by Greek Ministry of Agricultural Production and Food
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
mydata <- dbGetQuery(con, "SELECT * from agriculture.beekeepers") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata_processed<-mydata[,c("Περιφέρεια", "Περιφερειακή Ενότητα", "Έτος", "Αριθμός μελισσοκόμων")]
names(mydata_processed)<-c("region", "prefecture", "year", "number_of_beekeepers")
total_per_year<-aggregate(mydata_processed$number_of_beekeepers, 
                          by=list(year=as.character(mydata_processed$year)), FUN=sum, na.rm=TRUE)
mean_number_beekeepers<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
header <- dashboardHeader(title = "Geographic structure of beekeepers in Greece", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean_number_beekeepers", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Per prefecture",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("per_prefecture"),
            print("Source: Ministry of Agricultural Development and Food"),
            selectInput('prefecture', 'Prefecture', choices = unique(mydata_processed$prefecture)), width='98%')),
    box(
        title = "Per year",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("per_year"),
            print("Source: Ministry of Agricultural Development and Food"),
            selectInput('year', 'Year', choices = unique(mydata_processed$year)), width='98%'))
)
frow3 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Per region",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("per_region"),
            print("Source: Ministry of Agricultural Development and Food"), width='98%')),
    box(
        title = "Download data",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData"))))

body <- dashboardBody(frow1, frow2, frow3) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_processed[mydata_processed$year==input$year,]
        data_year<-aggregate(data_year$number_of_beekeepers, by=list(data_year$region), FUN=sum, na.rm=TRUE)
    })
    data_region <- reactive({ # Adding reactive data information
        data_region<-aggregate(mydata_processed$number_of_beekeepers, 
                             by=list(mydata_processed$region, as.character(mydata_processed$year)), FUN=sum, na.rm=TRUE)
    })
    data_prefecture <- reactive({ # Adding reactive data information
        data_prefecture<-mydata_processed[mydata_processed$prefecture==input$prefecture,]
        data_prefecture<-aggregate(data_prefecture$number_of_beekeepers, by=list(data_prefecture$year), FUN=sum, na.rm=TRUE)
    })
    output$per_year <- renderGvis({ # Creating chart
        gvisColumnChart(data_year(), options=list(colors="['#336600']", vAxis="{title:'Number of beekeepers'}", 
                        hAxis="{title:'Region'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$per_region <- renderPlot({ # Creating chart
        ggplot(data_region(), aes(x = Group.2, y = x, group = Group.1, colour = Group.1)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Year") + ylab("Number of beekeepers") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    })
    output$per_prefecture <- renderGvis({ # Creating chart
        gvisColumnChart(data_prefecture(), options=list(colors="['#336600']", vAxis="{title:'Number of beekeepers'}", 
                        hAxis="{title:'Year'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$mean_number_beekeepers <- renderValueBox({ # Filling valuebox
        valueBox(
            specify_decimal(mean_number_beekeepers,2),
            "Mean number of beekeepers annually",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$year," - ", printMoney(topyear$x)),
            "Year with maximum number of beekeepers",
            icon = icon("globe"),
            color = "olive")
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
    })
}
shinyApp(ui, server)