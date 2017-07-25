# Data: Horses population based on data from Hellenic Statistic Service
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

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.horses_elstat") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata$year<-as.character(mydata$year)
names(mydata)<-c("year", "region", "Horses male", "Horses female", "Donkeys male", "Donkeys female")
mydata_processed<-melt(mydata, id.vars=c("year", "region"))
names(mydata_processed)<-c("year", "region", "variable", "value")
total_per_year<-aggregate(mydata_processed$value, by=list(mydata_processed$year), FUN=sum)
mean_horses_population<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
header <- dashboardHeader(title = "Horses-donkeys in Greece", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean_horses_population", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Population course per type",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_category"),
            print("Source: ELSTAT"),
            selectInput('horses_category', 'Type', choices = unique(mydata_processed$variable)), width='98%')),
    box(
        title = "Population course per region",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_region"),
            print("Source: ELSTAT"),
            selectInput('region', 'Region', choices = unique(mydata_processed$region)), width='98%'))
)
frow3 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Data synopsis per type",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Source: ELSTAT"),
            sliderInput("myyearsummary", "Year:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep=""))),
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
        mydata_processed_timeline_category<-mydata_processed[which(mydata_processed$variable==input$horses_category),]
    })
    mydata_processed_timeline_region<-reactive({ # Filtering data by chosen wine type
        mydata_processed_timeline_region<-mydata_processed[which(mydata_processed$region==input$region),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata_processed[which(mydata_processed$year>=input$myyearsummary[1] & mydata_processed$year<=input$myyearsummary[2]),] 
    })
    output$mean_horses_population <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(mean_horses_population,2)),
            "Mean national population annually",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$Group.1, " - ", printMoney(topyear$x)),
            "Year with largest national population",
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
    output$summary <- renderDataTable({ # Creating summary by water_type
        mysummary <- data.frame(
            aggregate(value~variable, mydata_summary(), min),
            aggregate(value~variable, mydata_summary(), max),
            aggregate(value~variable, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Type", "Minimum population", "Maximum population", "Mean population")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
    })
}
shinyApp(ui, server)