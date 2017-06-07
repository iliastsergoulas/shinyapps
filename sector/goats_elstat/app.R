# Data: Goats population based on data from Hellenic Statistic Service
# This R script is created as a Shiny application to download raw data from Eurostat (ΕΛΣΤΑΤ), 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleVis)
library(ggplot2)
library(directlabels)
library(scales)
library(reshape2)

printMoney <- function(x){ # A function to show quantity as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show quantity with k decimal places

mydata<-read.csv("/home/iliastsergoulas/Dropbox/Website/shiny/sector/goats_elstat/goats.csv", 
                 sep=";", encoding='UTF-8', stringsAsFactors = FALSE)
mydata_processed<-melt(mydata, id.vars=c("Έτος", "Περιφέρεια"))
names(mydata_processed)<-c("year", "region", "variable", "value")
total_per_year<-mydata_processed[which(mydata_processed$variable=='Σύνολο.αιγοειδών'),]
total_per_year<-aggregate(total_per_year$value, by=list(total_per_year$year), FUN=sum)
mean_goats_population<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
mydata_category<-aggregate(mydata_processed$value, by=list(c(mydata_processed$year, mydata_processed$variable)), FUN=sum)
mydata_category$pct_category<-mydata_category$value/mydata_category[which(mydata_category$variable=='Σύνολο.αιγοειδών'),]$value
header <- dashboardHeader(title = "Αιγοειδή στην Ελλάδα", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean_goats_population", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Πορεία πληθυσμού αιγοειδών ανά κατηγορία",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_category"),
            print("Πηγή: ΕΛΣΤΑΤ"),
            selectInput('goats_category', 'Κατηγορία', choices = unique(mydata_processed$variable)), width='98%')),
    box(
        title = "Πορεία πληθυσμού αιγοειδών ανά Περιφέρεια",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_region"),
            print("Πηγή: ΕΛΣΤΑΤ"),
            selectInput('region', 'Περιφέρεια', choices = unique(mydata_processed$region)), width='98%'))
)
frow3 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Διάρθρωση Πληθυσμός ανά έτος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("view"),
            print("Πηγή: ΕΛΣΤΑΤ"),
            selectInput('year', 'Έτος', choices = unique(mydata_processed$year)), width='98%')),
    box(
        title = "Λήψη δεδομένων",
        status="success",
        collapsed = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(downloadButton("downloadData"))))

body <- dashboardBody(frow1, frow2, frow3) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard


server <- function(input, output) {
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_processed[mydata_processed$year==input$year,]
    })
    mydata_processed_timeline_category<-reactive({ # Filtering data by chosen wine category
        mydata_processed_timeline_category<-mydata_processed[which(mydata_processed$variable==input$goats_category),]
    })
    mydata_processed_timeline_region<-reactive({ # Filtering data by chosen wine type
        mydata_processed_timeline_region<-mydata_processed[which(mydata_processed$region==input$region),]
    })
    output$mean_goats_population <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(mean_goats_population,2)),
            "Μέσος εθνικός πληθυσμός αιγοειδών ετησίως",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$Group.1, " - ", printMoney(topyear$x)),
            "Έτος με μέγιστο συνολικό εθνικό πληθυσμό αιγοειδών",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline_category<-renderPlot({ # Creating timeline per wine category
        ggplot(mydata_processed_timeline_category(), aes(x = year, y = value, group = region, colour = region)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Πληθυσμός") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$timeline_region<-renderPlot({ # Creating timeline per wine type
        ggplot(mydata_processed_timeline_region(), aes(x = year, y = value, group = variable, colour = variable)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Πληθυσμός") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
    })
}
shinyApp(ui, server)