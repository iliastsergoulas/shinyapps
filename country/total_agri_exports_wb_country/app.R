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

mydata<-WDI(country = "all", indicator = "BX.GSR.AGRI.CD", extra = FALSE, cache = NULL) # Downloading raw data from World Bank
mydata$year<-as.character(mydata$year)
names(mydata)[names(mydata)=="BX.GSR.AGRI.CD"] <- "total_agri_exports"
mydata<-mydata[which(!is.na(mydata$total_agri_exports)),] # Filtering for NA values

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
        data_country<-mydata[mydata$country==input$country, c("year", "total_agri_exports")]
        data_country<-aggregate(data_country$total_agri_exports, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Συνολικές εξαγωγές (δολάρια)")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata[mydata$year==input$year,  c("country", "total_agri_exports")]
        data_year<-aggregate(data_year$total_agri_exports, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Συνολικές εξαγωγές (δολάρια)")
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
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "North")),]
        mydata_top_five<-mydata_top_five[which(!startsWith(mydata_top_five$country, "Africa")),]
        data_year_temp<-aggregate(mydata_top_five$total_agri_exports, by=list(Country=mydata_top_five$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyear[1] & mydata$year<=input$myyear[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", title="Εξαγωγές αγροτικών προϊόντων (αξία) στις χώρες της Ε.Ε.", 
                                        titleTextStyle="{color:'#336600',fontSize:14}", vAxis="{title:'Συνολικές εξαγωγές (δολάρια)'}", 
                        hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Χώρα", "Συνολικές εξαγωγές (δολάρια)", options=list(displayMode="regions", datamode='regions',width=700, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Κωδικός", "Χώρα", "Συνολικές εξαγωγές", "Έτος")
        mydata[c("Χώρα", "Συνολικές εξαγωγές", "Έτος")]
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(total_agri_exports~country, mydata_summary(), min),
            aggregate(total_agri_exports~country, mydata_summary(), max),
            aggregate(total_agri_exports~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Χώρα", "Ελάχιστη αξία εξαγωγών", "Μέγιστη αξία εξαγωγών", "Μέση αξία εξαγωγών")
        mysummary
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = total_agri_exports, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Συνολικές εξαγωγές (δολάρια)") + ggtitle("5 χώρες με τη μεγαλύτερη αξία εξαγόμενων προϊόντων") + 
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