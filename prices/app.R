# This R script is created as a Shiny application to use processed data from OPEKEPE (Greek Payment Agency) 
# and create plots and statistics.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(ggplot2)
library(directlabels)
library(scales)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(Quandl)

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
percent <- function(x, digits = 2, format = "f", ...) { # A function to show number as percentage
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k) # A function to show number with k decimal places

Quandl.api_key("....") # Setting API key to have unlimited access to databases
data_codes<-c("COM/WLD_SUGAR_EU", "COM/WLD_SUGAR_WLD", "COM/WLD_SUGAR_US", "COM/PSUGAEEC_USD", # Setting wanted Quandl database codes
              "COM/COFFEE_BRZL", "COM/COFFEE_CLMB", "COM/WLD_COFFEE_ROBUS", "COM/WLD_COFFEE_ARABIC",
              "COM/RICE_2", "COM/WLD_RICE_25", "COM/WLD_RICE_05", "COM/WLD_RICE_05_VNM", "COM/PRICENPQ_USD", 
              "COM/WLD_RICE_A1") 

data_descr<-c("Sugar Price, EU, cents/kg", "Sugar Price, world, cents/kg", "Sugar Price, US, cents/kg", # Setting Quandl codes respective description
              "Sugar, European import price, CIF Europe, US cents per pound", "Coffee, Brazilian, Comp.", "Coffee, Colombian, NY lb.",
              "Coffee Price, Robusta, cents/kg", "Coffee Price, Arabica, cents/kg",
              "Rice, Long Grain Milled, No. 2 AR", "Rice Price, Thailand, 25%, $/mt", "Rice Price, Thailand, 5%, $/mt",
              "Rice Price, Vietnamese, 5%, $/mt", "Rice, 5 percent broken milled white rice, Thailand nominal price quote, US$ per metric ton",
              "Rice Price, Thai, A1.Special, $/mt")
data_quandl<-data.frame(data_descr, data_codes) # Binding codes and description to dataframe

header <- dashboardHeader(title = "Τιμές αγροτικών προϊόντων", titleWidth=600) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of infoBoxes
    infoBoxOutput("last_price", width=3),
    infoBoxOutput("diff_yday", width=3),
    infoBoxOutput("highest_price", width=3),
    infoBoxOutput("lowest_price", width=3)
)
frow2 <- fluidRow( # Creating row of two diagrams
    title = "Συνολικά",
    status="success",
    collapsible = TRUE,
    theme = shinytheme("darkly"), 
    mainPanel(
        htmlOutput("view"),
        print("Πηγή: Quandl"), 
        selectInput('commodity', 'Προϊόν', choices = unique(data_quandl$data_descr)), 
        dateRangeInput("mydate", "Ημερομηνία:", start = "01-01-1960", end = Sys.Date()), width='98%')
)

body <- dashboardBody(frow1, frow2) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="green") # Binding elements of dashboard

server <- function(input, output) {
    mydata <- reactive({ # Adding reactive data information
        mydata<-Quandl(as.character(data_quandl[which(data_quandl$data_descr==input$commodity),]$data_codes))
        mydata<-mydata[which(mydata$Date>=input$mydate[1] & mydata$Date<=input$mydate[2]),]
        mydata$Date<-strptime(as.character(mydata$Date), "%Y-%m-%d")
        mydata$Date<-format(mydata$Date, "%d/%m/%Y")
        colnames(mydata)<-c("Date", "Price")
        mydata
    })
    output$view <- renderGvis({ # Creating chart
        gvisLineChart(mydata()[rev(rownames(mydata())),], options=list(colors="['#336600']", vAxis="{title:'Τιμή'}", 
                hAxis="{title:'Ημερομηνία'}",backgroundColor="#d9ffb3", width=1350, height=500, legend='none'))
    })
    output$last_price <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΣΗΜΕΡΙΝΗ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", paste0(specify_decimal(mydata()[1,2], 3), " $ - ", mydata()[1,1])),
            fill=TRUE,
            icon = icon("money"),
            color = "olive")
    })
    output$diff_yday <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΜΕΤΑΒΟΛΗ ΑΠΟ ΤΕΛΕΥΤΑΙΑ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", percent(((mydata()[1,2]-mydata()[2,2])/mydata()[2,2]))), 
            fill=TRUE,
            icon = icon("globe"),
            color = "olive")
    })
    output$highest_price <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΧΑΜΗΛΟΤΕΡΗ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", 
                           paste0(specify_decimal(mydata()[which.min(mydata()[,2]),][,2], 3), " $ - ", mydata()[which.min(mydata()[,2]),][,1])),
            fill=TRUE,
            icon = icon("money"),
            color = "olive")
    })
    output$lowest_price <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΥΨΗΛΟΤΕΡΗ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", 
                           paste0(specify_decimal(mydata()[which.max(mydata()[,2]),][,2],3), " $ - ", mydata()[which.max(mydata()[,2]),][,1])),
            fill=TRUE,
            icon = icon("money"),
            color = "olive")
    })
}
shinyApp(ui, server)