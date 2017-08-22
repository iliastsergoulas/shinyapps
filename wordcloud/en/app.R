# This R script is created as a Shiny application to generate a wordcloud out of tweets 
# of Greek agricultural press. 
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(twitteR)
library(wordcloud)
library(tm)
library(base64enc)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(RPostgreSQL)

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = as.character(credentials$host), port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mydata <- dbGetQuery(con, "SELECT * from public.tweets_en") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
myCorpus <- Corpus(VectorSource(mydata$`0`)) # Building a corpus
# Creating matrix od tweets after "cleaning" them from anything unnecessary
myDtm <- TermDocumentMatrix(myCorpus, control = 
            list(removePunctuation = TRUE, 
                 stopwords = c("agronewsgr", "https", "amp", "food", "read", "https","without","bring",
                               "thanks", "look", "looking", "see", "people", "way","fms","hemp","due",
                               "gaid", "agchattruth", "foodtruth","agchat","want","list","make","range",
                               "agriculture","will", "talk", "now", "new", "forget","like","using","ghg",
                               "farm", "thank", "book", "year", "week", "photo","today","farmers",
                               "add", "can", "truths", "life", "news", "every","lelyknowhow","farming",
                               "time", "action", "know", "share", "just", "found","always","day","farms",
                               "latest", "find", "get", "great", "table", "join", "word", "countries",
                               "thunderclap", "excited", "better", "voice", "video","use","asa","ahdb",
                               "paper", "getting", "blogs", "help", "one","unfao","via","...","_",
                               "best", "animal", "need", "joy", "many", "awesome", "support", "first",
                               "futureofcap","sacfarmclimate","offers","shows","play","-","means","hort",
                               "allow","provides","nfutweets","never","century","address","say","since",
                               "almost","philhoganeu","farmwildlifeuk","min","nyc","around","devex","xhnews",
                               "review", stopwords("english")),
                 removeNumbers = TRUE, tolower = TRUE))
m <- as.matrix(myDtm) # Converting to matrix
v <- sort(rowSums(m), decreasing=TRUE) # Calculating frequency of words
myNames <- names(v) # Getting words
d <- data.frame(word=myNames, freq=v) # Creating dataframe with each word and its frequency
ui <- fluidPage( # Creating shiny app's UI
    theme = shinytheme("spacelab"), 
    mainPanel(plotOutput("view"))
)
server <- function(input, output) {
    output$view <- renderPlot({ # Creating wordcloud
        wordcloud(d$word, d$freq, min.freq=3, max.words=100, scale=c(10,.5), 
                  rot.per=.5,colors=brewer.pal(8, "Dark2"))
    })
}
shinyApp(ui, server)