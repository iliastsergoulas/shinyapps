# This R script is created as a Shiny application to download raw data from Eurostat ((C) EuroGeographics for the administrative boundaries), 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(ggplot2)
library(directlabels)
library(scales)

mydata<-read.csv("./workforce_agri_country.csv", sep=",")
mydata["percentage"]<-(mydata["workforce_size"]/mydata["population"])*100

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel(
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")),
        conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                         selectInput('year', 'Έτος', choices = unique(mydata$year), selected = "2013")),
        conditionalPanel(condition="input.conditionedPanels == 'Χρονοσειρά' || input.conditionedPanels == 'Σύνοψη'", 
                         sliderInput("myyear", "Έτος:",min=min(as.numeric(mydata$year)), max=max(as.numeric(mydata$year)), 
                                     value=c(min(as.numeric(mydata$year))+1,max(as.numeric(mydata$year))-1), sep="")),
        width=2),
    mainPanel(
        tabsetPanel(
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", htmlOutput("map")), 
            tabPanel("Χρονοσειρά", plotOutput("timeline")),
            tabPanel("Δεδομένα", dataTableOutput("table")),
            tabPanel("Σύνοψη", dataTableOutput("summary")),
            id = "conditionedPanels"),
        print("Πηγή: (C) EuroGeographics for the administrative boundaries")))

server <- function(input, output) {
    # Add reactive data information
    data_country <- reactive({
        data_country<-mydata[mydata$country==input$country 
                             & mydata$workforce_type=="pers: Sole holders working on the farm" 
                             & mydata$workforce_size>0, c("year", "percentage")]
        data_country<-aggregate(data_country$percentage, by=list(Year=data_country$year), FUN=sum)
    })
    data_year <- reactive({
        data_year<-mydata[mydata$year==input$year  
                          & mydata$workforce_type=="pers: Sole holders working on the farm" 
                          & mydata$workforce_size>0, c("country", "percentage")]
        data_year<-aggregate(data_year$percentage, by=list(Country=data_year$country), FUN=sum)
    })
    mydata_top_five<-reactive({ # Subset data according to year interval and getting top five countries
        # Filtering out groups of countries
        mydata_top_five<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_top_five$total_agri_exports, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        print(data_year_temp)
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),] 
    })
    data_timeline <- reactive({
        data_timeline<-mydata[mydata$workforce_type=="pers: Sole holders working on the farm" 
                              & mydata$workforce_size>0, c("country", "year", "workforce_size", "percentage")]
        data_timeline<-na.omit(data_timeline)
    })
    output$view <- renderGvis({
        gvisColumnChart(data_country(), options=list(colors="['#336600']", title="Ποσοστό αγροτικής απασχόλησης στις χώρες της Ε.Ε.", 
                                                     titleTextStyle="{color:'#336600',fontSize:14}", backgroundColor="#d9ffb3", width=900, height=950))
    })
    output$map <- renderGvis({
        geochart <- gvisGeoChart(data_year(), "Country", "x", 
                                 options=list(region="150", displayMode="regions", 
                                              datamode='regions', width=900, height=700))
    })
    # Generate an HTML table view of the data
    output$table <- renderDataTable({
        colnames(mydata) <- c("Χώρα", "Έτος", "Τύπος αγροτικής απασχολησης", "Αριθμός απασχολούμενων", "Πληθυσμός", "Ποσοστό αγροτικής απασχόλησης")
        mydata
    })
    output$summary <- renderDataTable({ # Creating data table
        mysummary <- data.frame(
            aggregate(quantity~country, mydata_adjusted(), min),
            aggregate(quantity~country, mydata_adjusted(), max),
            aggregate(quantity~country, mydata_adjusted(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Χώρα", "Ελάχιστη Παραγωγή", "Μέγιστη Παραγωγή", "Μέση Παραγωγή")
        mysummary
    })
    output$timeline<-renderPlot({
        ggplot(mydata_adjusted(), aes(x = year, y = quantity, group = country, colour = country)) + 
            geom_line() +
            scale_colour_discrete(guide = 'none') +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Παραγωγή (χιλ. τόνοι)") + 
            geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) 
    })
    output$time_map<-renderGvis({
        geomap<- gvisMotionChart(data_timeline(), idvar = "country", timevar = "year",
                                 xvar = "workforce_size", yvar = "percentage")
    })
}
shinyApp(ui, server)