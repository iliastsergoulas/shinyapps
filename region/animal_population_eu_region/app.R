# Data: Animal population (EU regions)
# This R script is created as a Shiny application processing raw data from Eurostat ((C) EuroGeographics for the administrative boundaries), 
# and creating plots and maps as get_eurostat("agr_r_animal", time_format = "raw")
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
library(countrycode)
library(eurostat)

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show number with k decimal places

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.animal_population_eu_region") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
for (i in 1:nrow(mydata)) { # Replacing region codes in order to get correct region names
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
mydata_reg<-label_eurostat(mydata)
mydata$regionname <- mydata_reg$geo
mydata<-mydata[c("regionname", "geo", "time", "values")]
colnames(mydata)<-c("region", "geo", "year", "number")
mydata<-mydata[which(!startsWith(mydata$region, "Euro")),]
mymap<- merge_eurostat_geodata(data = mydata, geocolumn = "geo", # Creating geodata
                                 resolution = "60", all_regions = FALSE, output_class = "df")
mymap$class<-with(mymap, factor(findInterval(number, c(-Inf, # Creating classes out of values
                quantile(number, probs=c(0.25, .5, .75)), Inf)), 
                labels=c("Q1","Q2","Q3","Q4")))
mymap<-mymap[order(mymap$year, decreasing=TRUE),]

meanvalue<-mean((aggregate(mymap$number, by=list(year=mymap$year), FUN=mean)$x)) # Mean value
topc<-mymap[which.max(mymap$number),] # Top region
header <- dashboardHeader(title = "Μέγεθος ζωικού κεφαλαίου (χιλ. κεφαλές) - Περιφέρειες Ε.Ε.", titleWidth=600) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard

frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("number", width=6),
    valueBoxOutput("topregion", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Ανά περιφέρεια",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            htmlOutput("view"),
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            selectInput('region', 'Περιφέρεια', choices = unique(mymap$region)), width='98%')),
    box(
        title = "Ανά έτος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("map"),
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            selectInput('year', 'Έτος', choices = unique(mymap$year)), width='98%'))
)
frow3 <- fluidRow(# Creating row of diagram and summary
    box(
        title = "5 περιφέρειες με υψηλότερο μέγεθος ζωικού κεφαλαίου",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline", width = "150%"),
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            sliderInput("myyear", "Έτος:",min=min(as.numeric(mymap$year)), max=max(as.numeric(mymap$year)), 
                        value=c(min(as.numeric(mymap$year))+1,max(as.numeric(mymap$year))-1), sep=""))),
    box(
        title = "Σύνοψη δεδομένων ανά περιφέρεια",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            dataTableOutput("summary"),
            width=550,
            print("Πηγή: (C) EuroGeographics for the administrative boundaries"),
            sliderInput("myyearsummary", "Έτος:",min=min(as.numeric(mymap$year)), max=max(as.numeric(mymap$year)), 
                        value=c(min(as.numeric(mymap$year))+1,max(as.numeric(mymap$year))-1), sep="")))
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
    data_region <- reactive({ # Add reactive data information
        data_region<-mymap[mymap$region==input$region, c("year", "number")]
        data_region<-aggregate(data_region$number, by=list(Year=data_region$year), FUN=sum)
        colnames(data_region)<-c("Έτος", "Μέγεθος ζωικού κεφαλαίου")
        data_region
    })
    data_year <- reactive({ # Add reactive data information
        data_year<-mymap[which(mymap$year==input$year),]
    })
    mymap_top_five<-reactive({ # Subsetting data according to year interval and getting top five regions
        # Filtering out groups of regions
        mymap_top_five<-mymap[which(mymap$year>=input$myyear[1] & mymap$year<=input$myyear[2]),]
        data_year_temp<-aggregate(mymap_top_five$number, by=list(Region=mymap_top_five$region), FUN=mean)
        data_year_temp<-data_year_temp[order(-data_year_temp$x),]
        data_year_temp<-data_year_temp[1:5,] # Keeping top five countries
        mymap_top_five<-mymap_top_five[which(mymap_top_five$region %in% data_year_temp$Region),]
    })
    mymap_summary<-reactive({ # Subsetting data according to year interval
        mymap_summary<-mymap[which(mymap$year>=input$myyearsummary[1] & mymap$year<=input$myyearsummary[2]),] 
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_region(), options=list(colors="['#336600']", 
                        vAxis="{title:'Μέγεθος ζωικού κεφαλαίου (χιλ. κεφαλές)'}", hAxis="{title:'Έτος'}",
                        backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$map <- renderPlot({ # Creating map
        p <- ggplot(data=data_year(), aes(long,lat,group=group))
        p <- p + geom_polygon(data = data_year(), aes(long,lat),fill=NA,colour="white",size = 1.5)
        p <- p + geom_polygon(aes(fill = class),colour="dim grey",size=.2)
        p <- p + scale_fill_manual(values=c("#d0ed6a","#a0b74e","#7a8c3c","#4e5925","#333a18")) 
        p <- p + coord_map(project="orthographic", xlim=c(-22,34), ylim=c(25,70))
        p <- p + theme(legend.position = c(0.1,0.50), 
                        legend.justification=c(0,0),
                        legend.key.size=unit(6,'mm'),
                        legend.direction = "vertical",
                        legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                        legend.text=element_text(size=12), 
                        legend.title=element_text(size=12), 
                        title=element_text(size=16), 
                        panel.background = element_blank(), 
                        plot.background = element_blank(),
                        panel.grid.minor = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                        panel.grid.major = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                        axis.text = element_blank(), 
                        axis.title = element_blank(), 
                        axis.ticks = element_blank(), 
                        plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm"))
        p <- p + guides(fill = guide_legend(title = "Μέγεθος ζωικού κεφαλαίου",
                                            title.position = "top", 
                                            title.hjust=0))
        print(p)
    })
    output$timeline<-renderPlot({ # Creating timeline for top 5 countries
        ggplot(mymap_top_five(), aes(x = year, y = number, group = region, colour = region)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Μέγεθος ζωικού κεφαλαίου") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))  
    })
    output$number <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(meanvalue,2), " χιλιάδες κεφαλές"),
            "Μέσο ζωικό κεφάλαιο στις περιφέρειες της Ε.Ε.",
            icon = icon("user"),
            color = "olive")
    })
    output$topregion <- renderValueBox({ # Filling valuebox
        valueBox(
            topc$region,
            "Περιφέρεια με μεγαλύτερο ζωικό κεφάλαιο",
            icon = icon("globe"),
            color = "olive")
    })
    output$summary <- renderDataTable({ # Creating summary by country
        mysummary <- data.frame(
            aggregate(number~region, mymap_summary(), min),
            aggregate(number~region, mymap_summary(), max),
            aggregate(number~region, mymap_summary(), mean))
        mysummary <- mysummary[,c(1,2,4,6)]
        colnames(mysummary) <- c("Περιφέρεια", "Ελάχιστο μέγεθος ζωικού κεφαλαίου", "Μέγιστο μέγεθος ζωικού κεφαλαίου", "Μέσο μέγεθος ζωικού κεφαλαίου")
        mysummary
    }, options = list(lengthMenu = c(5, 25, 50), pageLength = 5))
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata, file)
    })
}
shinyApp(ui, server)