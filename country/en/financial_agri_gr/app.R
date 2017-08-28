# Data: Financial accounts, agriculture in Greece
# This R script is created as a Shiny application processing raw data downloaded from Hellenic Statistical Authority.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(ggplot2)
library(directlabels)
library(scales)
library(reshape2)
library(shinydashboard)
library(RPostgreSQL)

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show number with k decimal places

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.financial_agri_gr") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata_processed<-melt(mydata, id.vars=c("category", "subcategory"))
names(mydata_processed)<-c("category", "subcategory", "year", "value")
mydata_processed$year<-substring(as.character(mydata_processed$year),2)

header <- dashboardHeader(title = "Financial course of Greek agriculture", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of two diagrams
    theme = shinytheme("spacelab"), 
    mainPanel(
        plotOutput("agri_sector"),
        print("Source: Hellenic Statistical Authority"), width='98%')
)
frow2 <- fluidRow( # Creating row of two diagrams
    theme = shinytheme("spacelab"), 
    mainPanel(
        plotOutput("agri_sector_category"),
        print("Source: Hellenic Statistical Authority"), 
        selectInput('category', 'Category', choices = unique(mydata_processed$category)), width='98%')
)
frow3 <- fluidRow( # Creating row of two diagrams
    title = "Download data",
    status="success",
    collapsed = TRUE,
    theme = shinytheme("spacelab"), 
    mainPanel(downloadButton("downloadData")))

body <- dashboardBody(frow1, frow2, frow3) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    mydata_processed_category<-reactive({ # Subsetting data according to year interval
        mydata_processed_category<-mydata_processed[which(mydata_processed$category==input$category),] 
    })
    output$agri_sector<-renderPlot({ # Creating timeline per category
        ggplot(mydata_processed, aes(x = year, y = value, group = subcategory, colour = subcategory)) +   
            geom_bar(aes(fill = subcategory), position = "dodge", stat="identity") + 
            xlab("Year") + ylab("Euro (millions)") + 
            ggtitle("Agricultural sector course") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14),
                  axis.text.y=element_blank(),axis.ticks.y=element_blank()) 
    })
    output$agri_sector_category<-renderPlot({ # Creating timeline per category
        ggplot(mydata_processed_category(), aes(x = year, y = value, group = subcategory, colour = subcategory)) +   
            geom_bar(aes(fill = subcategory), position = "dodge", stat="identity") + 
            xlab("Year") + ylab("Euro (millions)") + 
            ggtitle('Agricultural sector course per category') + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14),
                  axis.text.y=element_blank(),axis.ticks.y=element_blank()) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
        })
}
shinyApp(ui, server)