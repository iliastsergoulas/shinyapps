# Data: Pork population based on data from Hellenic Statistic Service
# This R script is created as a Shiny application to process data 
# from Hellenic Statistical Authority (ΕΛΣΤΑΤ) and create plots and maps.
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

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.pork_classes_elstat") # Get data
mydata_processed<-melt(mydata, id.vars=c("Έτος", "Περιφέρεια"))
names(mydata_processed)<-c("year", "region", "variable", "value")
mydata_processed$variable<-chartr(".", " ", mydata_processed$variable)
total_per_year<-mydata_processed[which(mydata_processed$variable=='Σύνολο'),]
total_per_year<-aggregate(total_per_year$value, by=list(total_per_year$year), FUN=sum)
mean_pork_population<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
mydata_category<-aggregate(mydata_processed$value, by=list(mydata_processed$year, mydata_processed$variable), FUN=sum)
mydata_category$pct_category<-mydata_category$x/mydata_category[which(mydata_category$Group.2=='Σύνολο'),]$x
mydata_category<-mydata_category[which(mydata_category$Group.2!='Σύνολο'),]
names(mydata_category)<-c("Year", "Class", "Holdings", "Percentage")
header <- dashboardHeader(title = "Holdings per pork population class in Greece", titleWidth=650) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean_pork_population", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Holdings per population class",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_category"),
            print("Source: Hellenic Statistical Authority"),
            selectInput('pork_category', 'Κατηγορία', choices = unique(mydata_processed$variable)), width='98%')),
    box(
        title = "Holdings per Region",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_region"),
            print("Source: Hellenic Statistical Authority"),
            selectInput('region', 'Περιφέρεια', choices = unique(mydata_processed$region)), width='98%'))
)
frow3 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Temporal analysis of pork holdings structure",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("motion"),
            print("Source: Hellenic Statistical Authority"),width='98%')),
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
    })
    mydata_processed_timeline_category<-reactive({ # Filtering data by chosen wine category
        mydata_processed_timeline_category<-mydata_processed[which(mydata_processed$variable==input$pork_category),]
    })
    mydata_processed_timeline_region<-reactive({ # Filtering data by chosen wine type
        mydata_processed_timeline_region<-mydata_processed[which(mydata_processed$region==input$region),]
    })
    output$mean_pork_population <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(mean_pork_population,2)),
            "Mean pork holdings",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$Group.1, " - ", printMoney(topyear$x)),
            "Year with maximum holdings",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline_category<-renderPlot({ # Creating timeline per wine category
        ggplot(mydata_processed_timeline_category(), aes(x = year, y = value, group = region, colour = region)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Year") + ylab("Population") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$timeline_region<-renderPlot({ # Creating timeline per wine type
        ggplot(mydata_processed_timeline_region(), aes(x = year, y = value, group = variable, colour = variable)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Year") + ylab("Population") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$motion<-renderGvis({
        gvisMotionChart(mydata_category, xvar="Holdings", yvar="Percentage",
                        idvar="Class", timevar="Year")
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
        })
}
shinyApp(ui, server)