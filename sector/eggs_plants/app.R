# This R script is created as a Shiny application to download raw data from Eurostat ((C) EuroGeographics for the administrative boundaries), 
# process it and create plots and maps.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(googleVis)
library(leaflet)

mydata<-read.csv("./eggs_plants.csv", sep=",")

ui <- fluidPage(
    theme = shinytheme("spacelab"),
    mainPanel(
        tabsetPanel( # Create tabs
            tabPanel("Ανά Περιφέρεια", htmlOutput("view_region")),
            tabPanel("Ανά Περιφερειακή Ενότητα", htmlOutput("view_prefecture")),
            tabPanel("Πίνακας Δεδομένων", dataTableOutput("table")),
            id = "conditionedPanels"
        ),
        print("Πηγή: Υπουργείο Αγροτικής Ανάπτυξης και Τροφίμων (http://www.minagric.gr/images/stories/docs/agrotis/kthn_egkatastaseis/kentra_sysk_typ_avgwn021216.xls)")))

server <- function(input, output) {
    data_region<-aggregate(cbind("Αριθμός μονάδων" = approval_code) ~ region_name_gr, 
                                       data = mydata, FUN = function(x){NROW(x)})
    data_prefecture<-aggregate(cbind("Αριθμός μονάδων" = approval_code) ~ prefecture_name_gr, 
                           data = mydata, FUN = function(x){NROW(x)})
    output$view_region <- renderGvis({ # Creating chart
        gvisColumnChart(data_region, options=list(colors="['#336600']", vAxis="{title:'Αριθμός μονάδων'}", 
                        hAxis="{title:'Περιφέρεια'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$view_prefecture <- renderGvis({ # Creating chart
        gvisColumnChart(data_prefecture, options=list(colors="['#336600']", vAxis="{title:'Αριθμός μονάδων'}", 
                        hAxis="{title:'Περιφερειακή Ενότητα'}",backgroundColor="#d9ffb3", width=550, height=500, legend='none'))
    })
    output$table <- renderDataTable({ # Creating data table
        mydat<-mydata[c("approval_code", "business_name", "location", "prefecture_name_gr", "region_name_gr")]
        colnames(mydat) <- c("Κωδικός Έγκρισης", "Επωνυμία", "Τοποθεσία", "Περιφερειακή Ενότητα", "Περιφέρεια")
        mydat
    })
}
shinyApp(ui, server)