# Data: Employment in agriculture (% of total employment)
# This R script is created as a Shiny application processing raw data from Hellenic
# Statistic Authority (ELSTAT).
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(directlabels)
library(scales)
library(shinydashboard)
library(reshape2)
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
mydata <- dbGetQuery(con, "SELECT * from agriculture.aquaculture_employment") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata<-melt(mydata, id.vars=c("year", "water_type"))
colnames(mydata)<-c("year", "water_type", "employment_type","employment")
mydata$year<-as.character(mydata$year)
meanvalue<-mean((aggregate(mydata$employment, by=list(year=mydata$year), FUN=mean)$x)) # Mean value
topc<-mydata[which.max(mydata$employment),] # Top water_type
header <- dashboardHeader(title = "Aquaculture employment", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("employment", width=6),
    valueBoxOutput("topwater_type", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Per water type",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view_water"),
            print("Source: ELSTAT"),
            selectInput('water_type', 'Χώρα', choices = unique(mydata$water_type)), width='98%')),
    box(
        title = "Per employment type",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view_employment"),
            print("Source: ELSTAT"),
            selectInput('year', 'Year', choices = unique(mydata$year)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "Timeline of aquaculture employment",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("timeline", width = "150%"),
            print("Source: ELSTAT"),
            sliderInput("myyear", "Year:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep=""))),
    box(
        title = "Data synopsis per water type",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Source: ELSTAT"),
            sliderInput("myyearsummary", "Year:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep="")))
)
frow4 <- fluidRow( # Creating row of download button
    box(
        title = "Download data",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData")))
)

body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard


server <- function(input, output) {
    data_water_type <- reactive({ # Adding reactive data information
        data_water_type<-mydata[mydata$water==input$water_type, c("year", "employment")]
        data_water_type<-aggregate(data_water_type$employment, by=list(Year=data_water_type$year), FUN=sum)
        colnames(data_water_type)<-c("Year", "Employment")
        data_water_type
    })
    data_employment_type <- reactive({ # Addding reactive data information
        data_employment_type<-mydata[mydata$year==input$year,  c("employment_type", "employment")]
        data_employment_type<-aggregate(data_employment_type$employment, by=list(employment_type=data_employment_type$employment_type), FUN=sum)
        colnames(data_employment_type)<-c("employment type", "Employment")
        data_employment_type
    })
    mydata_timeline<-reactive({ # Subsetting data according to year interval and getting top five countries
        mydata_timeline<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),]
        mydata_timeline<-aggregate(mydata_timeline$employment, by=list(mydata_timeline$year), FUN=sum)
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyearsummary[1] & mydata$year<=input$myyearsummary[2]),] 
    })
    output$view_water <- renderGvis({ # Creating chart
        gvisColumnChart(data_water_type(), options=list(colors="['#336600']", vAxis="{title:'Employment'}", 
                        hAxis="{title:'Year'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$view_employment <- renderGvis({ # Creating map
        gvisColumnChart(data_employment_type(), options=list(colors="['#336600']", vAxis="{title:'Employment'}", 
                        hAxis="{title:'Employment type'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$summary <- renderDataTable({ # Creating summary by water_type
        mysummary <- data.frame(
            aggregate(employment~water_type, mydata_summary(), min),
            aggregate(employment~water_type, mydata_summary(), max),
            aggregate(employment~water_type, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Employment type", "Minimum employment", "Maximum employment", "Mean employment")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$employment <- renderValueBox({ # Filling valuebox
        valueBox(
            printMoney(meanvalue),
            "Mean employment",
            icon = icon("user"),
            color = "olive")
    })
    output$topwater_type <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topc$water_type," - ", topc$year),
            "Year with highest employment",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline<-renderGvis({ # Creating timeline for top 5 countries
        gvisLineChart(mydata_timeline(), options=list(colors="['#336600']", vAxis="{title:'Employment'}", 
                    hAxis="{title:'Year'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)