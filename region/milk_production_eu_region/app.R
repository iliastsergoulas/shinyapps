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

mydata<-get_eurostat("agr_r_milkpr", time_format = "raw") # Downloading raw data from Eurostat
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$dairyprod=='D1110A' & nchar(mydata$geo)>3 
                     & mydata$milkitem=='PRO' & !is.na(mydata$values)), ] # Filtering data
for (i in 1:nrow(mydata)) { # Replacing country codes in order to get correct country names
    if(as.character(mydata$geo[i])=="EL") {
        mydata$geo[i] <- "GR"}
    if(as.character(mydata$geo[i])=="UK") {
        mydata$geo[i] <- "GB"}}
#mydata$countryname <- countrycode(mydata$geo, "region", "country.name") # Getting country names
mydata<-mydata[c("geo", "time", "values")]
colnames(mydata)<-c("geo", "year", "quantity")
mymap<- merge_eurostat_geodata(data = mydata, geocolumn = "geo", # Creating geodata
                                 resolution = "60", all_regions = FALSE, output_class = "df")
mymap$class<-with(mymap, factor(findInterval(quantity, c(-Inf, # Creating classes out of values
                quantile(quantity, probs=c(0.25, .5, .75)), Inf)), 
                labels=c("Q1","Q2","Q3","Q4")))
mymap<-mymap[order(mymap$year, decreasing=TRUE),]

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    sidebarPanel( # Create sidebar panel with conditions
        conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                         selectInput('geo', 'Χώρα', choices = unique(mymap$geo))),
        conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                         selectInput('year', 'Έτος', choices = unique(mymap$year), selected = "2013"))
    ),
    mainPanel(
        tabsetPanel( # Create tabs
            tabPanel("Διάγραμμα", htmlOutput("view")),
            tabPanel("Χάρτης", plotOutput("map")), 
            tabPanel("Πίνακας Δεδομένων", dataTableOutput("table")),
            id = "conditionedPanels"
        )))

server <- function(input, output) {
    data_country <- reactive({ # Add reactive data information
        data_country<-mymap[mymap$geo==input$geo, c("year", "quantity")]
        data_country<-aggregate(data_country$quantity, by=list(Year=data_country$year), FUN=sum)
        colnames(data_country)<-c("Έτος", "Παραγωγή")
        data_country
    })
    data_year <- reactive({ # Add reactive data information
        data_year<-mymap[which(mymap$year==input$year),]
    })
    output$view <- renderGvis({ # Creating chart
        gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Παραγωγή (τόνοι)'}", hAxis="{title:'Έτος'}",
                                                     backgroundColor="#d9ffb3", width=800, height=700, legend='none'))
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
        p <- p + guides(fill = guide_legend(title = "Τόνοι γάλακτος",
                                            title.position = "top", 
                                            title.hjust=0))
        p <- p + labs(title = "Παραγωγή γάλακτος ανά περιφέρεια Ε.Ε.")
        print(p)
    })
    output$table <- renderDataTable({ # Creating data table
        colnames(mydata)<-c("Περιφέρεια", "Έτος", "Παραγωγή")
        mydata
    })
}
shinyApp(ui, server)