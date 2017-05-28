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

# Setting twitter credentials
credentials<-read.table("/home/iliastsergoulas/credentials.txt")
setup_twitter_oauth(as.character(credentials[1,1]), as.character(credentials[2,1]), 
                    as.character(credentials[3,1]), as.character(credentials[4,1]))
# Retrieving the first 50 tweets from the timeline of the main agricultural press's users
rdmTweets1 <- userTimeline("DaniNierenberg", n=50)
rdmTweets2 <- userTimeline("FAOnews", n=50)
rdmTweets3 <- userTimeline("mpaynspeaker", n=50)
rdmTweets4 <- userTimeline("ASA_CSSA_SSSA", n=50)
df1 <- do.call("rbind", lapply(rdmTweets1, as.data.frame))
df2 <- do.call("rbind", lapply(rdmTweets2, as.data.frame))
df3 <- do.call("rbind", lapply(rdmTweets3, as.data.frame))
df4 <- do.call("rbind", lapply(rdmTweets4, as.data.frame))
df <- rbind(df1, df2, df3, df4) # Creating a single dataframe with all the tweets
myCorpus <- Corpus(VectorSource(df$text)) # Building a corpus
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