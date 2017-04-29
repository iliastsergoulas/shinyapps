# This R script is created as a Shiny application to use raw agricultural commodities data, 
# available by Quandl, and create plots and statistics.
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
library(corrplot)
library(Quandl)
library(forecast)
library(dygraphs)

printMoney <- function(x){ # A function to show number as currency
    format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}
percent <- function(x, digits = 2, format = "f", ...) { # A function to show number as percentage
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
specify_decimal <- function(x, k) format(round(x, k), nsmall=k) # A function to show number with k decimal places

Quandl.api_key("KCo4sXzWEzSAb81ff3VP") # Setting API key to have unlimited access to databases
data_codes<-c("COM/WLD_SUGAR_EU", "COM/WLD_SUGAR_WLD", "COM/WLD_SUGAR_US", # Setting wanted Quandl database codes
              "COM/COFFEE_BRZL", "COM/COFFEE_CLMB", "COM/WLD_COFFEE_ARABIC",
              "COM/RICE_2", "COM/WLD_RICE_05", "COM/WLD_RICE_05_VNM") 
# Setting Quandl codes respective description
data_descr<-c("Sugar Price, EU, cents/kg", "Sugar Price, world, cents/kg", "Sugar Price, US, cents/kg", 
              "Coffee, Brazilian, Comp.", "Coffee, Colombian, NY lb.", "Coffee Price, Arabica, cents/kg",
              "Rice, Long Grain Milled, No. 2 AR", "Rice Price, Thailand, 5%, $/mt", "Rice Price, Vietnamese, 5%, $/mt")
data_product<-c("Sugar","Sugar","Sugar", 
                "Coffee","Coffee","Coffee", 
                "Rice","Rice","Rice")
data_quandl<-data.frame(data_descr, data_codes, data_product) # Binding codes and description to dataframe

header <- dashboardHeader(title = "Τιμές αγροτικών προϊόντων (Πηγή: Quandl)", titleWidth=600) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of two diagrams
    title = "Συνολικά",
    status="success",
    collapsible = TRUE, 
    mainPanel(
        htmlOutput("view"),
        selectInput('commodity', 'Προϊόν', choices = unique(data_quandl$data_product)), 
        dateRangeInput("mydate", "Ημερομηνία:", start = "01-01-1960", end = Sys.Date()), width='98%')
)
frow2 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(dygraphOutput("mytimeline"), width='98%')
)
frow3 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(plotOutput("timeline_2"), width='98%')
)
frow4 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(plotOutput("timeline_3"),width='98%')
)

body <- dashboardBody(frow1, frow2, frow3, frow4) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="yellow") # Binding elements of dashboard

server <- function(input, output) {
    mydata <- reactive({ # Adding reactive data information
        data_filtered<-as.data.frame(data_quandl[which(data_quandl$data_product==input$commodity),])
        mydata<-data.frame(Date= character(0), Value= character(0), Description=character(0))
        for (i in 1:nrow(data_filtered)){
            temp<-Quandl(as.character(data_filtered[i,2]))
            temp$Description<-as.character(data_filtered[i,1])
            colnames(temp)<-c("Date", "Value", "Description")
            mydata<-rbind(mydata, temp)
        }
        mydata<-mydata[which(mydata$Date>=input$mydate[1] & mydata$Date<=input$mydate[2]),]
        mydata$Date<-strptime(as.character(mydata$Date), "%Y-%m-%d")
        mydata$Date<-format(mydata$Date, "%d/%m/%Y")
        mydata
    })
    
    mydata_multiple<- reactive({ # Reshaping mydata dataframe
        mydata_multiple<-reshape(mydata(), direction = "wide", idvar = "Date", timevar = "Description")
    })
    
    mydata_1 <- reactive({
        mydata_1_product <- unique(mydata()$Description)[1]
        mydata_1<-mydata()[which(mydata()$Description==mydata_1_product),]
    }) 
    mydata_2 <- reactive({
        mydata_2_product <- unique(mydata()$Description)[2]
        mydata_2<-mydata()[which(mydata()$Description==mydata_2_product),]
    }) 
    mydata_3 <- reactive({
        mydata_3_product <- unique(mydata()$Description)[3]
        mydata_3<-mydata()[which(mydata()$Description==mydata_3_product),]
    }) 
    
    output$view <- renderGvis({ # Creating chart
        gvisLineChart(mydata_multiple()[rev(rownames(mydata_multiple())),], 
                      options=list(vAxis="{title:'Τιμή'}", hAxis="{title:'Ημερομηνία'}",
                                   width=1350, height=500, legend='none'))
    })
    output$mytimeline<-renderDygraph({ # Creating timeline for commodities
        dygraph(data_quandl[which(data_quandl$data_product==input$commodity),])
    })
    output$timeline_1<-renderPlot({ # Creating timeline for commodities
        ggplot(mydata_1()[rev(rownames(mydata_1())),], aes(x = as.Date(Date, "%d/%m/%Y"), y = Value, group=Description, colour=Description)) + 
            geom_line() + 
            ggtitle(mydata_1()[1,3]) + 
            scale_y_continuous(labels = comma) + 
            xlab("Ημερομηνία") + ylab("Τιμή") + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) + 
            theme(legend.position="none") + 
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$timeline_2<-renderPlot({ # Creating timeline for commodities
        ggplot(mydata_2()[rev(rownames(mydata_2())),], 
               aes(x = as.Date(Date, "%d/%m/%Y"), y = Value, group=Description, colour=Description)) + 
            geom_line() + 
            ggtitle(mydata_2()[1,3]) + 
            scale_y_continuous(labels = comma) + 
            xlab("Ημερομηνία") + ylab("Τιμή") + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) + 
            theme(legend.position="none") + 
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$timeline_3<-renderPlot({ # Creating timeline for commodities
        ggplot(mydata_3()[rev(rownames(mydata_3())),], 
               aes(x = as.Date(Date, "%d/%m/%Y"), y = Value, group=Description, colour=Description)) + 
            geom_line() + 
            ggtitle(mydata_3()[1,3]) + 
            scale_y_continuous(labels = comma) + 
            xlab("Ημερομηνία") + ylab("Τιμή") + 
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20)) + 
            theme(legend.position="none") + 
            theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
    })
    output$last_price <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΣΗΜΕΡΙΝΗ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", paste0(specify_decimal(mydata_1()[1,2], 3), " $ - ", mydata_1()[1,1], " - ", mydata_1()[1,3])),
            fill=TRUE,
            icon = icon("money"),
            color = "aqua")
    })
    output$diff_yday <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΜΕΤΑΒΟΛΗ ΑΠΟ ΤΕΛΕΥΤΑΙΑ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", percent(((mydata_1()[1,2]-mydata_1()[2,2])/mydata_1()[2,2]))), 
            fill=TRUE,
            icon = icon("percent"),
            color = "aqua")
    })
    output$highest_price <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΧΑΜΗΛΟΤΕΡΗ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", 
                           paste0(specify_decimal(mydata()[which.min(mydata_1()[,2]),][,2], 3), " $ - ", mydata_1()[which.min(mydata_1()[,2]),][,1])),
            fill=TRUE,
            icon = icon("level-down"),
            color = "aqua")
    })
    output$lowest_price <- renderInfoBox({ # Filling infoBox
        infoBox(
            "ΥΨΗΛΟΤΕΡΗ ΤΙΜΗ",
            value = tags$p(style = "font-size: 20px;", 
                           paste0(specify_decimal(mydata()[which.max(mydata_1()[,2]),][,2],3), " $ - ", mydata_1()[which.max(mydata_1()[,2]),][,1])),
            fill=TRUE,
            icon = icon("level-up"),
            color = "aqua")
    })
}
shinyApp(ui, server)