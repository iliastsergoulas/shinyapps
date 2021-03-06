# Data: Wine production based on data by Greek Ministry of Agricultural Production and Food
# This R script is created as a Shiny application to download raw data from Eurostat (Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων), 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinydashboard)
library(shinythemes)
library(RPostgreSQL)
library(ggplot2)
library(directlabels)
library(scales)
library(reshape2)

printMoney <- function(x){ # A function to show quantity as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k, decimal.mark=",", big.mark=".") # A function to show quantity with k decimal places

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from agriculture.wine_production_minagric") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata_processed<-melt(mydata, id.vars=c("wine_category", "year"))
total_per_year<-aggregate(mydata_processed$value, by=list(c(year=mydata_processed$year, variable=mydata_processed$total)), FUN=sum)
mean_wine_production<-mean(total_per_year$x) # Mean value
topyear<-total_per_year[which.max(total_per_year$x),] # Top country
header <- dashboardHeader(title = "Διάρθρωση παραγωγής οίνου στην Ελλάδα", titleWidth=500) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of valueboxes
    valueBoxOutput("mean_wine_production", width=6),
    valueBoxOutput("topyear", width=6)
)
frow2 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Πορεία παραγωγής ανά κατηγορία",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_category"),
            print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"),
            selectInput('wine_category', 'Κατηγορία', choices = unique(mydata_processed$wine_category)), width='98%')),
    box(
        title = "Πορεία παραγωγής ανά τύπο",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("timeline_type"),
            print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"),
            selectInput('type', 'Τύπος', choices = unique(mydata_processed$variable)), width='98%'))
)
frow3 <- fluidRow( # Creating row of two diagrams
    box(
        title = "Διάρθρωση παραγωγής ανά έτος",
        status="success",
        collapsible = TRUE,
        theme = shinytheme("spacelab"), 
        mainPanel(
            plotOutput("view"),
            print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"),
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
        mydata_processed_timeline_category<-mydata_processed[which(mydata_processed$wine_category==input$wine_category),]
    })
    mydata_processed_timeline_type<-reactive({ # Filtering data by chosen wine type
        mydata_processed_timeline_type<-mydata_processed[which(mydata_processed$variable==input$type),]
    })
    output$view <- renderPlot({ # Creating chart
        ggplot(data_year(), aes(x = variable, y = value, fill = wine_category, label = value)) + 
            xlab("Τύπος") + ylab("Παραγωγή (τόνοι)") + 
            theme(legend.title=element_blank()) + 
            geom_bar(stat = "identity")
    })
    output$mean_wine_production <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(specify_decimal(mean_wine_production,2), " τόνοι"),
            "Μέση παραγωγή οίνου ετησίως",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$Group.1," - ", printMoney(topc$x), " τόνοι"),
            "Έτος με μέγιστη συνολική παραγωγή οίνου",
            icon = icon("globe"),
            color = "olive")
    })
    output$timeline_category<-renderPlot({ # Creating timeline per wine category
        ggplot(mydata_processed_timeline_category(), aes(x = year, y = value, group = variable, colour = variable)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Παραγωγή (τόνοι)") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
            geom_dl(aes(label = variable), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) 
    })
    output$timeline_type<-renderPlot({ # Creating timeline per wine type
        ggplot(mydata_processed_timeline_type(), aes(x = year, y = value, group = wine_category, colour = wine_category)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Παραγωγή (τόνοι)") + 
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