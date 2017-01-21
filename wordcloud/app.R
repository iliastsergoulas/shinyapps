# This R script is created as a Shiny application to generate a wordcloud out of tweets of Greek agricultural press. 
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(googleVis)
library(shinythemes)
library(ggplot2)
library(directlabels)
library(scales)
library(twitteR)
library(wordcloud)
library(tm)
library(base64enc)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)

setup_twitter_oauth("", 
                    "", 
                    "", 
                    "")
# retrieve the first 100 tweets (or all tweets if fewer than 100)
# from the user timeline of @rdatammining
rdmTweets <- userTimeline("ypaithros", n=200)
n <- length(rdmTweets)
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(df$text))
myDtm <- TermDocumentMatrix(myCorpus, control = 
            list(removePunctuation = TRUE, 
                 stopwords = c("agronewsgr","για","και","από","των",
                               "την","στις","της","του","τον","τους",
                               "τις","στους","αύριο","στην","προς", 
                               "που","στα","κάθε","λέει","στο","στη",
                               "ζωντανά","αγρότες","αγροτικής","μήνα",
                               "ημέρες","μέρες","στον", "έως", "λόγω",
                               "αγροτικό","ζητά","αλλά","χωρίς", "προ",
                               stopwords("english")),
                 removeNumbers = TRUE, tolower = TRUE))

m <- as.matrix(myDtm)

ui <- fluidPage(
    theme = shinytheme("spacelab"), 
    mainPanel(plotOutput("view"))
)

server <- function(input, output) {
    output$view <- renderPlot({ # Creating chart
        wordcloud(d$word, d$freq, min.freq=3, colors=brewer.pal(8, "Dark2"))
    })
}
shinyApp(ui, server)