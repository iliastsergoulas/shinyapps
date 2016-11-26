library(shiny)
library(shinythemes)
library(googleVis)
library(leaflet)

mydata<-read.csv("./eggs_plants.csv", sep=",")

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels == 'Διάγραμμα'", selectInput("variable", "Μεταβλητή:",
                list("Περιφέρεια" = "region", "Περιφερειακή Ενότητα" = "prefecture")))),
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    tabsetPanel(
      tabPanel("Διάγραμμα", htmlOutput("view")),
      tabPanel("Χάρτης", leafletOutput("mymap")), 
      tabPanel("Πίνακας Δεδομένων", dataTableOutput("table")),
      id = "conditionedPanels"
    )))

# Define server logic required to plot various variables against mpg
server <- function(input, output) {
  data_geographic <- reactive({
    if (input$variable=="region"){
      data_geographic<-aggregate(cbind("Αριθμός μονάδων" = approval_code) ~ region_name_gr, 
                        data = mydata, FUN = function(x){NROW(x)})}
    else if (input$variable=="prefecture"){
      data_geographic<-aggregate(cbind("Αριθμός μονάδων" = approval_code) ~ prefecture_name_gr, 
                        data = mydata, FUN = function(x){NROW(x)})}
  })
  output$view <- renderGvis({
    gvisColumnChart(data_geographic(), options=list(titleTextStyle="{color:'#3399ff', fontName:'Courier', fontSize:22}", 
                    title="Μονάδες αυγοπαραγωγής", backgroundColor="#e6ffff", width=800, height=850))
  })
  output$map <- renderGvis({
    gvisGeoChart(data_geographic(), locationvar="region_name_gr",  
                 options=list(width=800, height=850, region="GR", displayMode="regions", 
                              resolution="provinces", dataMode="regions"))
  })
  # Generate an HTML table view of the data
  output$table <- renderDataTable({
    mydat<-mydata[c("approval_code", "business_name", "location", "prefecture_name_gr", "region_name_gr")]
    colnames(mydat) <- c("Κωδικός Έγκρισης", "Επωνυμία", "Τοποθεσία", "Περιφερειακή Ενότητα", "Περιφέρεια")
    mydat
  })
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=mydata$longitude, lat=mydata$latitude, popup=mydata$business_name)
  })}
shinyApp(ui, server)