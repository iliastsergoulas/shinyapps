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
credentials<-read.table("C:/Users/itsergoulas/Dropbox/Website/shiny/wordcloud/credentials.txt")
setup_twitter_oauth(as.character(credentials[1,1]), as.character(credentials[2,1]), 
                    as.character(credentials[3,1]), as.character(credentials[4,1]))
# Retrieving the first 50 tweets from the timeline of the main agricultural press's users
rdmTweets1 <- userTimeline("ypaithros", n=50)
rdmTweets2 <- userTimeline("Agronewsgr", n=50)
rdmTweets3 <- userTimeline("agrocapital", n=50)
rdmTweets4 <- userTimeline("agroteseu", n=50)
rdmTweets5 <- userTimeline("Agrotypos", n=50)
df1 <- do.call("rbind", lapply(rdmTweets1, as.data.frame))
df2 <- do.call("rbind", lapply(rdmTweets2, as.data.frame))
df3 <- do.call("rbind", lapply(rdmTweets3, as.data.frame))
df4 <- do.call("rbind", lapply(rdmTweets4, as.data.frame))
df5 <- do.call("rbind", lapply(rdmTweets5, as.data.frame))
df <- rbind(df1, df2, df3, df4, df5) # Creating a single dataframe with all the tweets
myCorpus <- Corpus(VectorSource(df$text)) # Building a corpus
corpus_clean <- tm_map(myCorpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords('english'))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, trim)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
# Creating matrix od tweets after "cleaning" them from anything unnecessary
myDtm <- TermDocumentMatrix(corpus_clean)
m <- as.matrix(myDtm)
v <- sort(rowSums(m), decreasing=TRUE) # Calculating frequency of words
myNames <- names(v) # Getting words
d <- data.frame(word=myNames, freq=v) # Creating dataframe with each word and its frequency
ui <- fluidPage( # Creating shiny app's UI
    theme = shinytheme("spacelab"), 
    mainPanel(plotOutput("view"))
)
server <- function(input, output) {
    output$view <- renderPlot({ # Creating wordcloud
        wordcloud(d$word, d$freq, min.freq=2, max.words=50, scale=c(3,.5), 
                  rot.per=.5,colors=brewer.pal(8, "Dark2"))
    })
}
shinyApp(ui, server)