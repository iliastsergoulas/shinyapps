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

mydata<-get_eurostat("ef_oluaareg", time_format = "raw") # Downloading raw data from Eurostat
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$indic_ef=='AGRAREA_HA' & mydata$agrarea=='TOTAL'), ] # Filtering data
for (i in 1:nrow(mydata)) { # Replacing country codes in order to get correct country names
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name") # Getting country names
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")] # Filtering for country names not found
colnames(mydata)<-c("country", "year", "area")

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel( # Creating sidebar panel with conditions
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")),
        conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                         selectInput('year', 'Έτος', choices = unique(mydata$year), selected = "2013")),
        conditionalPanel(condition="input.conditionedPanels == 'Δεδομένα'", downloadButton("downloadData")),
        conditionalPanel(condition="input.conditionedPanels == 'Χρονοσειρά' || input.conditionedPanels == 'Σύνοψη ανά χώρα'", 
                         sliderInput("myyear", "Έτος:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                                     value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep="")),
        width=2),
    mainPanel(
        tabsetPanel( # Creating tabs
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", htmlOutput("map")), 
            tabPanel("Χρονοσειρά", plotOutput("timeline")),
            tabPanel("Δεδομένα", dataTableOutput("table")),
            tabPanel("Σύνοψη ανά χώρα", dataTableOutput("summary")),
            id = "conditionedPanels"),
        print("Πηγή: (C) EuroGeographics for the administrative boundaries")))

server <- function(input, output) {
    data_country <- reactive({ # Adding reactive data information
        data_country<-mydata[mydata$country==input$country, c("year", "area")]
        data_country<-aggregate(data_country$area, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Έκταση καλλιεργημένης γης")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata[mydata$year==input$year,  c("country", "area")]
        data_year<-aggregate(data_year$area, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Έκταση καλλιεργημένης γης")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        # Filtering out groups of countries
        mydata_top_five<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_top_five$area, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", title="Καλλιεργημένη γη στις χώρες της Ε.Ε.", 
                        titleTextStyle="{color:'#336600',fontSize:14}", vAxis="{title:'Έκταση καλλιεργημένης γης (εκτάρια)'}", 
                        hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Χώρα", "Έκταση καλλιεργημένης γης", options=list(region="150", 
                                displayMode="regions", datamode='regions',width=700, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Χώρα", "Έτος", "Έκταση καλλιεργήσιμης γης")
        mydata
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(area~country, mydata_summary(), min),
            aggregate(area~country, mydata_summary(), max),
            aggregate(area~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Χώρα", "Ελάχιστη έκταση", "Μέγιστη έκταση", "Μέση έκταση")
        mysummary
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = area, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Έκταση καλλιεργημένης γης (εκτάρια)") + ggtitle("5 χώρες με μεγαλύτερη έκταση καλλιεργημένης γης") + 
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