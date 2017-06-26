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
                 stopwords = c("agronewsgr", "https", "amp", "food", "read", "https","without",
                               "thanks", "look", "looking", "see", "people", "way","fms","hemp",
                               "gaid", "agchattruth", "foodtruth","agchat","want","list",
                               "agriculture","will", "talk", "now", "new", "forget","like",
                               "farm", "thank", "book", "year", "week", "photo","today",
                               "add", "can", "truths", "life", "news", "every","lelyknowhow",
                               "time", "action", "know", "share", "just", "found","always","day",
                               "latest", "find", "get", "great", "table", "join", "word", 
                               "thunderclap", "excited", "better", "voice", "video","use","asa",
                               "paper", "getting", "blogs", "help", "at ...", "...", "one",
                               "best", "animal", "need", "joy", "many", "awesome", "support", 
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
        wordcloud(d$word, d$freq, min.freq=2, max.words=50, scale=c(5,.5), 
                  rot.per=.5,colors=brewer.pal(8, "Dark2"))
    })
}
shinyApp(ui, server)