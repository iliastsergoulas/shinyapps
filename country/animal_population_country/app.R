library(shiny)
library(googleVis)
library(shinythemes)
library(eurostat)
library(countrycode)

mydata<-get_eurostat("agr_r_animal", time_format = "raw") # Downloading raw data from Eurostat
mydata$geo<-as.character(mydata$geo)
mydata<-mydata[which(mydata$animals=='A2000'), ] # Filtering data
for (i in 1:nrow(mydata)) { # Replacing country codes in order to get correct country names
  if(as.character(mydata$geo[i])=="EL") {
    mydata$geo[i] <- "GR"}
  if(as.character(mydata$geo[i])=="UK") {
    mydata$geo[i] <- "GB"}}
mydata$countryname <- countrycode(mydata$geo, "iso2c", "country.name") # Getting country names
mydata<-mydata[which(!is.na(mydata$countryname)), c("countryname", "time", "values")] # Filtering for country names not found
colnames(mydata)<-c("country", "year", "number")

ui <- fluidPage(
  theme = shinytheme("spacelab"), 
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'",
                     selectInput('country', 'Χώρα', choices = unique(mydata$country), selected = "Greece")),
    conditionalPanel(condition="input.conditionedPanels == 'Χάρτης'",
                     selectInput('year', 'Έτος', choices = unique(mydata$year), selected = "2011"))),
  mainPanel(
    tabsetPanel(
      tabPanel("Διάγραμμα", htmlOutput("view")),
      tabPanel("Χάρτης", htmlOutput("map")), 
      tabPanel("Δεδομένα", dataTableOutput("table")),
      id = "conditionedPanels"),
    print("Πηγή: Eurostat (μέσω του R πακέτου eurostat)"))
  )

server <- function(input, output) {
  data_country <- reactive({ # Add reactive data information
    data_country<-mydata[mydata$country==input$country, c("year", "number")]
    data_country<-aggregate(data_country$number, by=list(Year=data_country$year), FUN=sum)
    colnames(data_country)<-c("Έτος", "Παραγωγή")
    data_country
  })
  data_year <- reactive({
    data_year<-mydata[mydata$year==input$year,  c("country", "number")]
    data_year<-aggregate(data_year$number, by=list(Country=data_year$country), FUN=sum)
    colnames(data_year)<-c("Χώρα", "Μέγεθος ζωικού κεφαλαίου")
    data_year
  })
  output$view <- renderGvis({ #Creating chart
    gvisColumnChart(data_country(), options=list(colors="['#336600']", vAxis="{title:'Μέγεθος ζωικού κεφαλαίου (χιλ. κεφαλές)'}", hAxis="{title:'Έτος'}",
                                                 backgroundColor="#d9ffb3", width=800, height=700, legend='none'))
  })
  output$map <- renderGvis({ # Creating map
    geomap <- gvisGeoChart(data_year(), "Χώρα", "Μέγεθος ζωικού κεφαλαίου", 
                           options=list(region="150", displayMode="regions", 
                                        datamode='regions',width=800, height=700))
  })
  output$table <- renderDataTable({ # Creating data table
    colnames(mydata)<-c("Χώρα", "Έτος", "Μέγεθος ζωικού κεφαλαίου")
    mydata
  })
}
shinyApp(ui, server)