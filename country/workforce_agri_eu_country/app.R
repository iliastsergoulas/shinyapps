# Data: Agricultural Labour Input Statistics: absolute figures (1 000 annual work units)
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

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show number with k decimal places

mydata<-get_eurostat("aact_ali01", time_format = "raw") # Downloading raw data from Eurostat
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$itm_newa=='40000'), ] # Filtering data
for (i in 1:nrow(mydata)) { # Replacing country codes in order to get correct country names
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name") # Getting country names
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")] # Filtering for country names not found
colnames(mydata)<-c("country", "year", "number")

meanvalue<-mean((aggregate(mydata$number, by=list(year=mydata$year), FUN=mean)$x)) # Mean value
topc<-mydata[which.max(mydata$number),] # Top country
header <- dashboardHeader(title = "Εργατικό δυναμικό αγροτικού τομέα (χιλ. εργαζόμενοι)", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("number", width=6),
    valueBoxOutput("topcountry", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Ανά χώρα",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view"),
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            selectInput('country', 'Χώρα', choices = unique(mydata$country)), width='98%')),
    box(
        title = "Ανά έτος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("map"),
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            selectInput('year', 'Έτος', choices = unique(mydata$year)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "5 χώρες με το μεγαλύτερο ποσοστό αγροτικής απασχόλησης",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline", width = "150%"),
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            sliderInput("myyear", "Έτος:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep=""))),
    box(
        title = "Σύνοψη δεδομένων ανά χώρα",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            sliderInput("myyearsummary", "Έτος:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                        value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep="")))
)
frow4 <- fluidRow( # Creating row of download button
    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData")))
)

body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    data_country <- reactive({ # Adding reactive data information
        data_country<-mydata[mydata$country==input$country, c("year", "number")]
        data_country<-aggregate(data_country$number, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Μέγεθος αγροτικής απασχόλησης")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata[mydata$year==input$year,  c("country", "number")]
        data_year<-aggregate(data_year$number, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Μέγεθος αγροτικής απασχόλησης")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        # Filtering out groups of countries
        mydata_top_five<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_top_five$number, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        print(data_year_temp)
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Μέγεθος αγροτικής απασχόλησης (χιλιάδες εργαζόμενοι)'}", hAxis="{title:'Έτος'}",
                        backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Χώρα", "Μέγεθος αγροτικής απασχόλησης", 
                        options=list(region="150", displayMode="regions", datamode='regions',
                        width=700, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Χώρα", "Έτος", "Μέγεθος αγροτικής απασχόλησης")
        mydata
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(number~country, mydata_summary(), min),
            aggregate(number~country, mydata_summary(), max),
            aggregate(number~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Χώρα", "Ελάχιστη αγροτική απασχόληση", "Μέγιστη αγροτική απασχόληση", "Μέση αγροτική απασχόληση")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$number <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(meanvalue,2), " χιλιάδες εργαζόμενοι"),
            "Μέσο μέγεθος αγροτικής απασχόλησης στις χώρες της Ε.Ε.",
            icon = icon("user"),
            color = "olive")
    })
    output$topcountry <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topc$country," - ", topc$year),
            "Χώρα με μεγαλύτερη αγροτική απασχόληση",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = number, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Μέγεθος αγροτικής απασχόλησης") + 
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