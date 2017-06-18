# This R script is created as a Shiny application to use raw agricultural commodities data, 
# available by Quandl, and create plots and statistics.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
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
              "COM/RICE_2", "COM/WLD_RICE_05", "COM/WLD_RICE_05_VNM",
              "COM/BEEF_S", "COM/BEEF_C", "COM/WLD_BEEF",
              "COM/WLD_BANANA_EU", "COM/WLD_BANANA_US", "COM/PBANSOP_USD") 
# Setting Quandl codes respective description
data_descr<-c("Sugar Price, EU, cents/kg", "Sugar Price, world, cents/kg", "Sugar Price, US, cents/kg", 
              "Coffee, Brazilian, Comp.", "Coffee, Colombian, NY lb.", "Coffee Price, Arabica, cents/kg",
              "Rice, Long Grain Milled, No. 2 AR", "Rice Price, Thailand, 5%, $/mt", "Rice Price, Vietnamese, 5%, $/mt",
              "Beef - Select 1", "Beef - Choice 1", "Beef,($/kg)",
              "Banana, Europe,($/kg)", "Banana, US,($/kg)", "Bananas, Central American and Ecuador, FOB U.S. Ports, US$ per metric ton")
data_product<-c("Sugar","Sugar","Sugar", 
                "Coffee","Coffee","Coffee", 
                "Rice","Rice","Rice",
                "Beef", "Beef", "Beef",
                "Banana", "Banana", "Banana")
data_quandl<-data.frame(data_descr, data_codes, data_product) # Binding codes and description to dataframe

header <- dashboardHeader(title = "Prices of agricultural commodities (Source: Quandl)", titleWidth=600) # Header of dashboard
sidebar <- dashboardSidebar(disable = TRUE)# Disabling sidebar of dashboard
frow1 <- fluidRow( # Creating row of two diagrams
    title = "Total view",
    status="success",
    collapsible = TRUE, 
    mainPanel(
        dygraphOutput("view"),
        selectInput('commodity', 'Commodity', choices = unique(data_quandl$data_product)), width='98%')
)
frow2 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(dygraphOutput("timeline_1"), width='98%')
)
frow3 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(dygraphOutput("timeline_2"), width='98%')
)
frow4 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(dygraphOutput("timeline_3"),width='98%')
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
        mydata
    })
    mydata_multiple<- reactive({ # Reshaping mydata dataframe
        unique_descriptions<-unique(mydata()$Description)
        mydata_multiple<-reshape(mydata(), direction = "wide", idvar = "Date", timevar = "Description")
        colnames(mydata_multiple)<-c("Date", unique_descriptions[1], unique_descriptions[2], unique_descriptions[3])
        mydata_multiple<-xts(mydata_multiple, order.by=as.POSIXct(mydata_multiple$Date))
        mydata_multiple<-mydata_multiple[,-c(1)]
    })
    mydata_1 <- reactive({
        mydata_1_product <- unique(mydata()$Description)[1]
        mydata_1<-mydata()[which(mydata()$Description==mydata_1_product),]
        mydata_1<-xts(mydata_1, order.by=as.POSIXct(mydata_1$Date))
    }) 
    mydata_2 <- reactive({
        mydata_2_product <- unique(mydata()$Description)[2]
        mydata_2<-mydata()[which(mydata()$Description==mydata_2_product),]
        mydata_2<-xts(mydata_2, order.by=as.POSIXct(mydata_2$Date))
    }) 
    mydata_3 <- reactive({
        mydata_3_product <- unique(mydata()$Description)[3]
        mydata_3<-mydata()[which(mydata()$Description==mydata_3_product),]
        mydata_3<-xts(mydata_3, order.by=as.POSIXct(mydata_3$Date))
    }) 
    output$view <- renderDygraph({ # Creating chart
        dygraph(mydata_multiple(), main="Comparing commodities' prices", group = "commodities")%>%
        dyAxis("y", label = "Commodity Price")%>%
        dyRangeSelector(height = 20)
    })
    output$timeline_1<-renderDygraph({ # Creating timeline for commodities
        dygraph(mydata_1()$Value, main=mydata_1()[1,3], group = "commodities")%>%
        dyAxis("y", label = "Commodity Price")%>%
        dyRangeSelector(height = 20)
    })
    output$timeline_2<-renderDygraph({ # Creating timeline for commodities
        dygraph(mydata_2()$Value, main=mydata_2()[1,3], group = "commodities")%>%
        dyAxis("y", label = "Commodity Price")%>%
        dyRangeSelector(height = 20)
    })
    output$timeline_3<-renderDygraph({ # Creating timeline for commodities
        dygraph(mydata_3()$Value, main=mydata_3()[1,3], group = "commodities")%>%
        dyAxis("y", label = "Commodity Price")%>%
        dyRangeSelector(height = 20)
    })
}
shinyApp(ui, server)