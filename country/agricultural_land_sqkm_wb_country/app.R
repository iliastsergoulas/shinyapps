# Data: Agricultural land (sq. km)
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

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show number with k decimal places
<<<<<<< HEAD
windowsFonts(Arial=windowsFont("TT Arial"))
=======
>>>>>>> 483748c51f3c4c0aaa4ff22099a470be18afb6a2

mydata<-WDI(country = "all", indicator = "AG.LND.AGRI.K2", extra = FALSE, cache = NULL) # Downloading raw data from World Bank
mydata$year<-as.character(mydata$year)
names(mydata)[names(mydata)=="AG.LND.AGRI.K2"] <- "agri_area_sqkm"
mydata<-mydata[which(!is.na(mydata$agri_area_sqkm)),] # Filtering for NA values
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

meanvalue<-mean((aggregate(mydata_filtered$agri_area_sqkm, by=list(year=mydata_filtered$year), FUN=mean)$x)) # Mean value
topc<-mydata_filtered[which.max(mydata_filtered$agri_area_sqkm),] # Top country
header <- dashboardHeader(title = "Έκταση αγροτικής γης (τ. χλμ.)", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("agri_area_sqkm", width=6),
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
            print("Πηγή: World Bank"),
            selectInput('country', 'Χώρα', choices = unique(mydata$country)), width='98%')),
    box(
        title = "Ανά έτος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("map"),
            print("Πηγή: World Bank"),
            selectInput('year', 'Έτος', choices = unique(mydata$year)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "5 χώρες με μεγαλύτερη έκταση αγροτικής γης",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline", width = "150%"),
            print("Πηγή: World Bank"),
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
            print("Πηγή: World Bank"),
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
        data_country<-mydata[mydata$country==input$country, c("year", "agri_area_sqkm")]
        data_country<-aggregate(data_country$agri_area_sqkm, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Έκταση αγροτικής γης (τ.χλμ.)")
        data_country
    })
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_filtered[mydata_filtered$year==input$year,  c("country", "agri_area_sqkm")]
        data_year<-aggregate(data_year$agri_area_sqkm, by=list(Country=data_year$country), FUN=sum)
        colnames(data_year)<-c("Χώρα", "Έκταση αγροτικής γης (τ.χλμ.)")
        data_year
    })
    mydata_top_five<-reactive({ # Subsetting data according to year interval and getting top five countries
        mydata_top_five<-mydata_filtered[which(mydata_filtered$year>=input$myyear[1] & mydata_filtered$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mydata_filtered$agri_area_sqkm, by=list(Country=mydata_filtered$country), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mydata_top_five<-mydata_top_five[which(mydata_top_five$country %in% data_year_temp$Country),]
    })
    mydata_summary<-reactive({ # Subsetting data according to year interval
        mydata_summary<-mydata[which(mydata$year>=input$myyearsummary[1] & mydata$year<=input$myyearsummary[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Έκταση αγροτικής γης (τ.χλμ.)'}", 
                        hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", width=700, height=500, legend='none'))
    })
    output$map <- renderGvis({ # Creating map
        gvisGeoChart(data_year(), "Χώρα", "Έκταση αγροτικής γης (τ.χλμ.)", options=list(displayMode="regions", datamode='regions',width=700, height=500))
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Κωδικός", "Χώρα", "Έκταση αγροτικής γης (τ.χλμ.)", "Έτος")
        mydata[c("Χώρα", "Έκταση αγροτικής γης (τ.χλμ.)", "Έτος")]
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(agri_area_sqkm~country, mydata_summary(), min),
            aggregate(agri_area_sqkm~country, mydata_summary(), max),
            aggregate(agri_area_sqkm~country, mydata_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Χώρα", "Ελάχιστη έκταση αγροτικής γης", "Μέγιστη έκταση αγροτικής γης", "Μέση έκταση αγροτικής γης")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$agri_area_sqkm <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(printMoney(meanvalue), " τ. χλμ."),
            "Μέση έκταση αγροτικής γης παγκοσμίως",
            icon = icon("user"),
            color = "olive")
    })
    output$topcountry <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topc$country," - ", topc$year),
            "Χώρα με μεγαλύτερη έκταση αγροτικής γης",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mydata_top_five(), aes(x = year, y = agri_area_sqkm, group = country, colour = country)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Έκταση αγροτικής γης") + 
<<<<<<< HEAD
            theme(plot.title = element_text(family = "Arial", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=14)) + 
=======
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
>>>>>>> 483748c51f3c4c0aaa4ff22099a470be18afb6a2
            geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)