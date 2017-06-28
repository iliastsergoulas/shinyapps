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
library(lubridate)

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
              "COM/WLD_RICE_05", "COM/WLD_RICE_25", "COM/WLD_RICE_05_VNM",
              "COM/BEEF_S", "COM/BEEF_C", "COM/WLD_BEEF",
              "COM/WLD_BANANA_EU", "COM/WLD_BANANA_US", "COM/PBANSOP_USD",
              "COM/WLD_COCOA", "COM/WLD_COTTON_A_INDX", "COM/OATS", "COM/MILK",
              "COM/EGGS", "COM/BUTTER", "COM/WLD_TOBAC_US","COM/WLD_ORANGE",
              "COM/WLD_WHEAT_CANADI", "COM/WLD_WHEAT_US_HRW", "COM/WLD_WHEAT_US_SRW", "COM/PWHEAMT_USD",
              "COM/WOOL", "COM/WOOL_60_62", "COM/WOOL_60", "COM/WOOL_58", "COM/WOOL_62",
              "COM/CORN_MEAL", "COM/CORN_FEED", "COM/WLD_MAIZE", "COM/PMAIZMT_USD",
              "COM/WLD_LAMB", "COM/WLD_CHICKEN", "COM/PSHRI_USD", "COM/WLD_SHRIMP_MEX",
              "COM/WLD_SUNFLOWER_OIL", "COM/WLD_GRNUT_OIL", "COM/WLD_COCONUT_OIL", "COM/WLD_RAPESEED_OIL", 
              "COM/WLD_PALM_OIL", "COM/WLD_SOYBEAN_OIL", "COM/POLVOIL_USD",
              "COM/WLD_TEA_KOLKATA", "COM/WLD_TEA_MOMBASA", "COM/WLD_TEA_COLOMBO", "COM/WLD_TEA_AVG",
              "COM/WLD_IBEVERAGES", "COM/WLD_IGRAINS", "COM/WLD_IFOOD", "COM/WLD_IFERTILIZERS", "COM/WLD_IAGRICULTURE", "COM/WLD_IENERGY") 
# Setting Quandl codes respective description
data_descr<-c("Sugar Price, EU, cents/kg", "Sugar Price, world, cents/kg", "Sugar Price, US, cents/kg", 
              "Coffee, Brazilian, Comp.", "Coffee, Colombian, NY lb.", "Coffee Price, Arabica, cents/kg",
              "Rice, Thai 5% ,($/mt)", "Rice, Thai 25% ,($/mt)", "Rice, Viet Namese 5%,($/mt)",
              "Beef - Select 1", "Beef - Choice 1", "Beef,($/kg)",
              "Banana, Europe,($/kg)", "Banana, US,($/kg)", "Bananas, Central American and Ecuador, $/mt",
              "Cocoa,($/kg)","Cotton, A Index,($/kg)", "Oats, No. 2 milling, Mnpls; $ per bu", "Milk, Nonfat dry, Chicago",
              "Eggs, large white, Chicago dozen", "Butter, AA Chicago, lb","Tobacco, US import u.v.,($/mt)",
              "Orange,($/kg)", 
              "Wheat, Canadian,($/mt)", "Wheat, US HRW,($/mt)", "Wheat, US SRW,($/mt)", "Wheat, No.1 Hard Red Winter ($/mt)",
              "Wool, 64s", "Wool, 60-62s", "Wool, 60s", "Wool, 58s", "Wool, 62s",
              "Corn gluten meal, Midwest, ton", "Corn gluten feed, Midwest, ton", "Maize,($/mt)", "Maize (corn), U.S. No.2 Yellow, FOB Gulf of Mexico, U.S. price, US$ per metric ton",
              "Meat, sheep,($/kg)", "Meat, chicken,($/kg)", "Shrimp, shell-on headless, 26-30 count/pound, Mexican origin, $/kg", "Shirmps, Mexican,($/kg)",
              "Sunflower oil,($/mt)", "Groundnut oil,($/mt)", "Coconut oil,($/mt)", "Rapeseed oil,($/mt)", "Palm oil,($/mt)",
              "Soybean oil,($/mt)", "Olive Oil, extra virgin less than 1% free fatty acid,($/mt)",
              "Tea, Kolkata,($/kg)", "Tea, Mombasa,($/kg)", "Tea, Colombo,($/kg)", "Tea, avg 3 auctions,($/kg)",
              "Beverages Index", "Grains Index", "Food Index", "Fertilizers Index", "Agriculture Index", "Energy Index")
data_product<-c("Ζάχαρη","Ζάχαρη","Ζάχαρη", 
                "Καφές","Καφές","Καφές", 
                "Ρύζι","Ρύζι","Ρύζι",
                "Βοδινό", "Βοδινό", "Βοδινό",
                "Μπανάνες", "Μπανάνες", "Μπανάνες",
                "Κακάο", "Βαμβάκι", "Βρώμη","Γάλα",
                "Αυγά", "Βούτυρο", "Καπνός", "Πορτοκάλια",
                "Σιτάρι", "Σιτάρι", "Σιτάρι", "Σιτάρι",
                "Μαλλί", "Μαλλί", "Μαλλί", "Μαλλί", "Μαλλί",
                "Καλαμπόκι", "Καλαμπόκι", "Καλαμπόκι", "Καλαμπόκι",
                "Κρέας", "Κρέας", "Γαρίδες", "Γαρίδες",
                "Έλαια", "Έλαια", "Έλαια", "Έλαια", "Έλαια", "Έλαια", "Έλαια",
                "Τσάι", "Τσάι", "Τσάι", "Τσάι",
                "Δείκτες", "Δείκτες", "Δείκτες", "Δείκτες", "Δείκτες", "Δείκτες")
data_quandl<-data.frame(data_descr, data_codes, data_product) # Binding codes and description to dataframe

header <- dashboardHeader(title = "Τιμές αγροτικών προϊόντων ", titleWidth=600) # Header of dashboard
sidebar <- dashboardSidebar(sidebarMenu(
    selectInput('commodity', 'Προϊόν', choices = unique(data_quandl$data_product)),
    tags$footer(
        tags$p("Η παρούσα εφαρμογή βασίζεται σε δεδομένα του ιστοτόπου Quandl."))))
frow1 <- fluidRow( # Creating row of two diagrams
    title = "Συνολικά",
    status="success",
    collapsible = TRUE, 
    mainPanel(dygraphOutput("view"), width='98%')
)
frow2 <- fluidRow( # Creating row of two diagrams
    status="success",
    collapsible = TRUE, 
    mainPanel(uiOutput("plots"), width='98%')
)

body <- dashboardBody(frow1, frow2) # Binding rows to body of dashboard
ui <- dashboardPage(header, sidebar, body, skin="yellow") # Binding elements of dashboard

server <- function(input, output) {
    mydata <- reactive({ # Adding reactive data information
        data_filtered<-as.data.frame(data_quandl[which(data_quandl$data_product==input$commodity),])
        mydata<-data.frame(Date= character(0), Value= character(0), Description=character(0))
        for (i in 1:nrow(data_filtered)){
            temp<-Quandl(as.character(data_filtered[i,2]), collapse = "monthly")
            temp$Description<-as.character(data_filtered[i,1])
            colnames(temp)<-c("Date", "Value", "Description")
            mydata<-rbind(mydata, temp)
        }
        mydata
    })
    mydata_multiple<- reactive({ # Reshaping mydata dataframe
        unique_descriptions<-unique(mydata()$Description)
        mydata_multiple<-reshape(mydata(), direction = "wide", idvar = "Date", timevar = "Description")
        #colnames(mydata_multiple)<-c("Date", unique_descriptions[1], unique_descriptions[2], unique_descriptions[3])
        mydata_multiple<-xts(mydata_multiple, order.by=as.POSIXct(mydata_multiple$Date))
        mydata_multiple<-mydata_multiple[,-c(1)]
    })
    output$view <- renderDygraph({ # Creating chart
        #combined <- cbind(mydata_multiple(), actual=mydata_multiple())
        dygraph(mydata_multiple(), main="Αντιπαραβολή τιμών αγροτικών προϊόντων", group = "commodities")%>%
            dyAxis("y", label = "Τιμή προϊόντος")%>%
            dyRangeSelector(height = 20)
    })
    mylength<-reactive({
        mylength<-length(unique(mydata()$Description))
    })
    output$plots <- renderUI({
        createPlots()
        plot_output_list <- lapply(1:mylength(), function(i) {
            plotname <- paste("plot", i, sep="")
            dygraphOutput(plotname)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })
    createPlots <- reactive ({
        # Call renderPlot for each one. Plots are only actually generated when they
        # are visible on the web page.
        for (i in 1:mylength()) {
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderPlot() will be the same across all instances, because
            # of when the expression is evaluated.
            local({
                my_i <- i
                plotname=paste("plot", my_i, sep="")
                mydata_product <- unique(mydata()$Description)[my_i]
                mydata_ts<-mydata()[which(mydata()$Description==mydata_product),]
                mydata_ts<-xts(mydata_ts, order.by=as.POSIXct(mydata_ts$Date))
                mydata_predicted <- forecast(as.numeric(mydata_ts$Value), h=24)
                mydata_predicted <- data.frame(Date = seq(mdy('06/30/2017'), by = 'months', length.out = 24),
                                    Forecast = mydata_predicted$mean,Hi_95 = mydata_predicted$upper[,2],
                                    Lo_95 = mydata_predicted$lower[,2])
                mydata_xts <- xts(mydata_predicted, order.by = as.POSIXct(mydata_predicted$Date))
                mydata_predicted <- merge(mydata_ts, mydata_xts)
                mydata_predicted <- mydata_predicted[,c("Value", "Forecast", "Hi_95", "Lo_95")]
                output[[plotname]] <- renderDygraph({ # Creating timeline for commodities
                    dygraph(mydata_predicted, main=mydata_ts[1,3], group = "commodities")%>%
                        dyAxis("y", label = "Τιμή προϊόντος")%>%
                        dyRangeSelector(height = 20)
                })
            })
        }
    })
}
shinyApp(ui, server)