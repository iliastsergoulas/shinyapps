# Data: Honey beekeepers based on data by Greek Ministry of Agricultural Production and Food
# This R script is created as a Shiny application to process data 
# from Hellenic Ministry of Agricultural Development and Food and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(googleVis)
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
mydata <- dbGetQuery(con, "SELECT * from agriculture.beekeepers") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
mydata_processed<-mydata[,c("Περιφέρεια", "Περιφερειακή Ενότητα", "Έτος", "Αριθμός μελισσοκόμων")]
names(mydata_processed)<-c("region", "prefecture", "year", "number_of_beekeepers")
ui <- fluidPage(
        mainPanel(
            htmlOutput("per_prefecture"),
            print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων"),
            selectInput('prefecture', 'Περιφερειακή Ενότητα', choices = unique(mydata_processed$prefecture)), width='98%')
        )

server <- function(input, output) {
    data_year <- reactive({ # Adding reactive data information
        data_year<-mydata_processed[mydata_processed$year==input$year,]
        data_year<-aggregate(data_year$number_of_beekeepers, by=list(data_year$region), FUN=sum, na.rm=TRUE)
    })
    data_region <- reactive({ # Adding reactive data information
        data_region<-aggregate(mydata_processed$number_of_beekeepers, 
                             by=list(mydata_processed$region, as.character(mydata_processed$year)), FUN=sum, na.rm=TRUE)
    })
    data_prefecture <- reactive({ # Adding reactive data information
        data_prefecture<-mydata_processed[mydata_processed$prefecture==input$prefecture,]
        data_prefecture<-aggregate(data_prefecture$number_of_beekeepers, by=list(data_prefecture$year), FUN=sum, na.rm=TRUE)
    })
    output$per_year <- renderGvis({ # Creating chart
        gvisColumnChart(data_year(), options=list(colors="['#336600']", vAxis="{title:'Αριθμός μελισσοκόμων'}", 
                        hAxis="{title:'Περιφέρεια'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$per_region <- renderPlot({ # Creating chart
        ggplot(data_region(), aes(x = Group.2, y = x, group = Group.1, colour = Group.1)) + 
            geom_line() +
            scale_x_discrete(expand=c(0, 0.5)) + 
            scale_y_continuous(labels = comma) + 
            xlab("Έτος") + ylab("Αριθμός μελισσοκόμων") + 
            theme(legend.title=element_blank()) + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) +
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    })
    output$per_prefecture <- renderGvis({ # Creating chart
        gvisColumnChart(data_prefecture(), options=list(colors="['#336600']", vAxis="{title:'Αριθμός μελισσοκόμων'}", 
                        hAxis="{title:'Έτος'}",backgroundColor="#d9ffb3", height=500, legend='none'))
    })
    output$mean_number_beekeepers <- renderValueBox({ # Filling valuebox
        valueBox(
            specify_decimal(mean_number_beekeepers,2),
            "Μέσος αριθμός μελισσοκόμων ετησίως",
            icon = icon("map"),
            color = "olive")
    })
    output$topyear <- renderValueBox({ # Filling valuebox
        valueBox(
            paste0(topyear$year," - ", printMoney(topyear$x)),
            "Έτος με μέγιστο αριθμό μελισσοκόμων",
            icon = icon("globe"),
            color = "olive")
    })
    output$downloadData <- downloadHandler( # Creating download button
        filename = function() { paste('mydata_processed', '.csv', sep='') },
        content = function(file) {
            write.csv(mydata_processed, file)
    })
}
shinyApp(ui, server)