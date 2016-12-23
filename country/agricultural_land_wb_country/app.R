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

mydata<-WDI(country = "all", indicator = "AG.LND.AGRI.ZS", extra = FALSE, cache = NULL) # Downloading raw data from World Bank
mydata$year<-as.character(mydata$year)
names(mydata)[names(mydata)=="AG.LND.AGRI.ZS"] <- "agri_area_percentage"
mydata<-mydata[which(!is.na(mydata$agri_area_percentage)),] # Filtering for NA values

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
        print("Πηγή: World Bank")))

server <- function(input, output) {
    data_country <- reactive({ # Adding reactive data information
        data_country<-mydata[mydata$country==input$country, c("year", "agri_area_percentage")]
        data_country<-aggregate(data_country$agri_area_percentage, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Ποσοστό αγροτικής γης")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata[mydata$year==input$year,  c("country", "agri_area_percentage")]
        data_year<-aggregate(data_year$agri_area_percentage, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Ποσοστό αγροτικής γης")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        # Filtering out groups of countries
        mydata_top_five<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Euro")),]
        mydata_top_five<-mydata_top_five[which(!endsWith(mydata_top_five$country, "income")),]
        mydata_top_five<-mydata_top_five[which(!endsWith(mydata_top_five$country, "dividend")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "East Asia")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "IDA")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Latin")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Sub")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "OECD")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "South Asia")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Least")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Middle")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "World")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "IBRD")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Arab")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Heavily")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Fragile")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Central Europe")),]
        mydata_top_five<-mydata_top_five[which(!endsWith(mydata_top_five$country, "states")),]
        mydata_top_five<-mydata_top_five[which(!endsWith(mydata_top_five$country, "America")),]
        data_year_temp<-aggregate(mydata_top_five$agri_area_percentage, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", title="Ποσοστό αγροτικής γης", 
                        titleTextStyle="{color:'#336600',fontSize:14}", vAxis="{title:'Ποσοστό αγροτικής γης (%)'}", 
                        hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Χώρα", "Ποσοστό αγροτικής γης", options=list(displayMode="regions", datamode='regions',width=700, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Κωδικός", "Χώρα", "Ποσοστό αγροτικής γης", "Έτος")
        mydata[c("Χώρα", "Ποσοστό αγροτικής γης", "Έτος")]
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(agri_area_percentage~country, mydata_summary(), min),
            aggregate(agri_area_percentage~country, mydata_summary(), max),
            aggregate(agri_area_percentage~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Χώρα", "Ελάχιστο ποσοστό αγροτικής γης", "Μέγιστο ποσοστό αγροτικής γης", "Μέσο ποσοστό αγροτικής γης")
        mysummary
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = agri_area_percentage, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Ποσοστό αγροτικής γης") + ggtitle("5 χώρες με υψηλότερο ποσοστό αγροτικής γης") + 
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