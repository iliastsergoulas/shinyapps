# Data: Organic crop production by crops (from 2012 onwards): fresh vegetables (including melons)
# This R script is created as a Shiny application to download raw data from Eurostat ((C) EuroGeographics for the administrative boundaries), 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(eurostat)
library(countrycode)
library(ggplot2)
library(directlabels)
library(scales)
library(shinydashboard)

printMoney <- function(x){ # A function to show quantity as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show quantity with k decimal places

mydata<-get_eurostat("org_croppro", time_format = "raw") # Downloading raw data from Eurostat
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$crops=='V0000'), ] # Filtering data
for (i in 1:nrow(mydata)) { # Replacing country codes in order to get correct country names
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name") # Getting country names
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")] # Filtering for country names not found
colnames(mydata)<-c("country", "year", "quantity")

meanvalue<-mean((aggregate(mydata$quantity, by=list(year=mydata$year), FUN=mean)$x)) # Mean value
topc<-mydata[which.max(mydata$quantity),] # Top country
header <- dashboardHeader(title = "Organic vegetables production in EU countries", titleWidth=550) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("quantity", width=6),
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
            print("Source: (C) EuroGeographics for the administrative boundaries"),
            selectInput('country', 'Country', choices = unique(mydata$country)), width='98%')),
    box(
        title = "Per year",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("map"),
            print("Source: (C) EuroGeographics for the administrative boundaries"),
            selectInput('year', 'Year', choices = unique(mydata$year)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "5 countries with largest organic vegetables production",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline", width = "150%"),
            print("Source: (C) EuroGeographics for the administrative boundaries"),
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
            print("Source: (C) EuroGeographics for the administrative boundaries"),
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
    data_country <- reactive({ # Adding reactive data information
        data_country<-mydata[mydata$country==input$country & mydata$quantity>0, c("year", "quantity")]
        data_country<-aggregate(data_country$quantity, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Year", "Production")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata[mydata$year==input$year & mydata$quantity>0,  c("country", "quantity")]
        data_year<-aggregate(data_year$quantity, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Country", "Production")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        # Filtering out groups of countries
        mydata_top_five<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_top_five$quantity, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyearsummary[1] & mydata$year<=input$myyearsummary[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", 
                        titleTextStyle="{color:'#336600',fontSize:14}", vAxis="{title:'Production (tonnes)'}", 
                        hAxis="{title:'Year'}", backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Country", "Production", 
                                  options=list(region="150", displayMode="regions", datamode='regions', width=550, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Country", "Year", "Production")
        mydata
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(quantity~country, mydata_summary(), min),
            aggregate(quantity~country, mydata_summary(), max),
            aggregate(quantity~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Country", "Minimum Production", "Maximum Production", "Mean Production")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = quantity, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Year") + ylab("Production (τόνοι)") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
            geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)