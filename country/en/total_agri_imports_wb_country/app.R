# Data: Total agricultural imports (FAO, current US$)
# This R script is created as a Shiny application to download raw data from World Bank through WDI package, 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(WDI)
library(countrycode)
library(ggplot2)
library(directlabels)
library(scales)
library(shinydashboard)

printMoney <- function(x){ # A function to show quantity as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show quantity with k decimal places

mydata<-WDI(country = "all", indicator = "BM.GSR.AGRI.CD", extra = FALSE, cache = NULL) # Downloading raw data from World Bank
mydata$year<-as.character(mydata$year)
names(mydata)[names(mydata)=="BM.GSR.AGRI.CD"] <- "total_agri_imports"
mydata<-mydata[which(!is.na(mydata$total_agri_imports)),] # Filtering for NA values
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

meanvalue<-mean((aggregate(mydata_filtered$total_agri_imports, by=list(year=mydata_filtered$year), FUN=mean)$x)) # Mean value
topc<-mydata_filtered[which.max(mydata_filtered$total_agri_imports),] # Top country
header <- dashboardHeader(title = "Imports of agricultural products (value $)", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("total_agri_imports", width=6),
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
        title = "5 countries with highest value of agricultural products' imports",
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
        mainPanel(downloadButton("downloadData")))
)

body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_country <- reactive({ # Adding reactive data information
        data_country<-mydata[mydata$country==input$country, c("year", "total_agri_imports")]
        data_country<-aggregate(data_country$total_agri_imports, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Year", "Total imports ($)")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_filtered[mydata_filtered$year==input$year,  c("country", "total_agri_imports")]
        data_year<-aggregate(data_year$total_agri_imports, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Country", "Total imports ($)")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        mydata_top_five<-mydata_filtered[which(mydata_filtered$year>=input$myyear[1] & mydata_filtered$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_top_five$total_agri_imports, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyearsummary[1] & mydata$year<=input$myyearsummary[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Total imports ($)'}", 
                        hAxis="{title:'Year'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Country", "Total imports ($)", 
                     options=list(displayMode="regions", datamode='regions',width=550, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Κωδικός", "Country", "Total imports ($)", "Year")
        mydata[c("Country", "Συνολικές εισαγωγές", "Year")]
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(total_agri_imports~country, mydata_summary(), min),
            aggregate(total_agri_imports~country, mydata_summary(), max),
            aggregate(total_agri_imports~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Country", "Minimum imports value", "Maximum imports value", "Mean imports value")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$total_agri_imports <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(printMoney(meanvalue), " $"),
            "Mean value of agricultural products' imports globally",
            icon = icon("dollar"),
            color = "olive")
    })
    output$topcountry <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topc$country," - ", topc$year),
            "Country with highest value of agricultural products' imports",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = total_agri_imports, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Year") + ylab("Total imports ($)") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
            geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.8))  
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() {paste('mydata', '.csv', sep='')},
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)