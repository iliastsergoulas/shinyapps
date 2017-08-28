# Data: Organic products: activities, producers etc
# This R script is created as a Shiny application processing raw data downloaded from 
# Greek Ministry of Agricultural Development and Food.
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
mydata_activities <- dbGetQuery(con, "SELECT * from agriculture.organic_producers_activities") # Getting data
mydata_products <- dbGetQuery(con, "SELECT * from agriculture.organic_plants") # Getting data
dbDisconnect(con)
dbUnloadDriver(drv)

mydata_products <- melt(mydata_products, id=c("year","crop_type"))
header <- dashboardHeader(title = "Organic products in Greece", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of two diagrams
    theme = shinytheme("spacelab"), 
    mainPanel(
        plotOutput("organic"),
        print("Source: Ministry of Agricultural Development and Food"), width='98%')
)
frow2 <- fluidRow( # Creating row of two diagrams
    theme = shinytheme("spacelab"), 
    mainPanel(
        plotOutput("organic_products"),
        print("Source: Ministry of Agricultural Development and Food"), 
        selectInput('variable', 'Variable', choices = unique(mydata_activities$variable)), width='98%')
)
frow3 <- fluidRow( # Creating row of two diagrams
    title = "Download data",
    status="success",
    collapsed = TRUE,
    theme = shinytheme("spacelab"), 
    mainPanel(downloadButton("downloadData")))

body <- dashboardBody(frow1, frow3) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    output$organic<-renderPlot({ # Creating timeline per category
        mydata_activities_players<-mydata_activities[which(mydata_activities$variable=='Παραγωγοί' | mydata_activities$variable=='Μεταποιητές' 
                                     | mydata_activities$variable=='Εξαγωγείς' | mydata_activities$variable=='Εισαγωγείς'),]
        ggplot(mydata_activities_players, aes(x = year, y = value, group = variable, colour = variable)) +   
            geom_bar(aes(fill = variable), position = "dodge", stat="identity") + 
            xlab("Year") + ylab("Euro (millions)") + 
            ggtitle("Organic products main players") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$organic_products<-renderPlot({ # Creating timeline per category
        ggplot(mydata_products, aes(x = year, y = value, group = variable, colour = variable)) +   
            geom_bar(aes(fill = variable), position = "dodge", stat="identity") + 
            xlab("Year") + ylab("Euro (millions)") + 
            ggtitle("Organic products") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_activities', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_activities, file)
        })
}
shinyApp(ui, server)