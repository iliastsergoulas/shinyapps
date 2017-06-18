# Data: Agriculture, value added (constant 2000 US$)
# This R script is created as a Shiny application processing raw data downloaded from World Bank through WDI package, 
# and creating plots and maps as WDI(country = "all", indicator = "NV.AGR.TOTL.KD", extra = FALSE, cache = NULL)
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(ggplot2)
library(directlabels)
library(scales)
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
mydata <- dbGetQuery(con, "SELECT * from agriculture.agri_added_value_wb_country") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata$year<-as.character(mydata$year)
names(mydata)[names(mydata)=="NV.AGR.TOTL.KD"] <- "total_added_value"
mydata<-mydata[which(!is.na(mydata$total_added_value)),] # Filtering for NA values
# Filtering out groups of countries
mydata_filtered<-mydata[which(!startsWith(mydata$country, "Euro")),]
mydata_filtered<-mydata_filtered[which(!endsWith(mydata_filtered$country, "income")),]
mydata_filtered<-mydata_filtered[which(!endsWith(mydata_filtered$country, "dividend")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "East Asia")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "IDA")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Latin")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Sub")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "OECD")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "South Asia")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Least")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Middle")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "World")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "IBRD")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Arab")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Heavily")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Fragile")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Central Europe")),]
mydata_filtered<-mydata_filtered[which(!endsWith(mydata_filtered$country, "states")),]
mydata_filtered<-mydata_filtered[which(!endsWith(mydata_filtered$country, "America")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "Africa")),]
mydata_filtered<-mydata_filtered[which(!startsWith(mydata_filtered$country, "North Africa")),]

meanvalue<-mean((aggregate(mydata_filtered$total_added_value, by=list(year=mydata_filtered$year), FUN=mean)$x)) # Mean value
topc<-mydata_filtered[which.max(mydata_filtered$total_added_value),] # Top country
header <- dashboardHeader(title = "Agricultural added value", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("totaladdedvalue", width=6),
    valueBoxOutput("topcountry", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Per country",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view"),
            print("Source: World Bank"),
            selectInput('country', 'Country', choices = unique(mydata$country)), width='98%')),
    box(
        title = "Per year",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("map"),
            print("Source: World Bank"),
            selectInput('year', 'Year', choices = unique(mydata$year)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "5 countries with highest added value",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline", width = "150%"),
            print("Source: World Bank"),
            sliderInput("myyear", "Year:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep=""))),
    box(
        title = "Data synopsis per country",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Source: World Bank"),
            sliderInput("myyearsummary", "Year:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep="")))
)
frow4 <- fluidRow( # Creating row of download button
    box(
        title = "Download data",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData"))))
    
body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_country <- reactive({ # Adding reactive data information
        data_country<-mydata[mydata$country==input$country, c("year", "total_added_value")]
        data_country<-aggregate(data_country$total_added_value, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Year", "Added value of agriculture ($ in 2000 prices)")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_filtered[mydata_filtered$year==input$year,  c("country", "total_added_value")]
        data_year<-aggregate(data_year$total_added_value, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Country", "Added value of agriculture ($ in 2000 prices)")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        mydata_top_five<-mydata_filtered[which(mydata_filtered$year>=input$myyear[1] & mydata_filtered$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_top_five$total_added_value, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyearsummary[1] & mydata$year<=input$myyearsummary[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#008d4c']", vAxis="{title:'Value ($ in 2000 prices)'}", 
                        hAxis="{title:'Year'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Country", "Added value of agriculture ($ in 2000 prices)", 
                     options=list(displayMode="regions", datamode='regions',width=550, height=500))
    })
    output$totaladdedvalue <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(printMoney(meanvalue)," $"),
            "Mean added value among countries (prices 2000)",
            icon = icon("money"),
            color = "olive")
    })
    output$topcountry <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topc$country," - ", topc$year),
            "Country with top added value",
            icon = icon("globe"),
            color = "olive")
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(total_added_value~country, mydata_summary(), min),
            aggregate(total_added_value~country, mydata_summary(), max),
            aggregate(total_added_value~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Country", "Minimum added value", "Maximum added value", "Mean added value")
        mysummary
        }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5)
    )
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = total_added_value, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 1)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Year") + ylab("Added value of agriculture ($ in 2000 prices)") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12)) + 
            geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)