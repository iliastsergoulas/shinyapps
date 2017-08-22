# This R script is created as a Shiny application to generate a wordcloud out of tweets 
# of Greek agricultural press. 
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/shinyapps/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
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
mydata <- dbGetQuery(con, "SELECT * from public.tweets_gr") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)
myCorpus <- Corpus(VectorSource(mydata$`0`)) # Building a corpus
# Creating matrix od tweets after "cleaning" them from anything unnecessary
myDtm <- TermDocumentMatrix(myCorpus, control = 
            list(removePunctuation = TRUE, 
                 stopwords = c("agronewsgr","για","και","από","των", "οι", "...",
                               "την","στις","της","του","τον","τους", "τα","να", "τέλος",
                               "τις","στους","αύριο","στην","προς", "θα", "ως", "ευρώ",
                               "που","στα","κάθε","λέει","στο","στη", "σε", "agrenda",
                               "ζωντανά","αγρότες","αγροτικής","μήνα", "τη", "φεκ",
                               "ημέρες","μέρες","στον", "έως", "λόγω", "εκατ","amp",
                               "αγροτικό","ζητά","αλλά","χωρίς", "προ", "ύψους","κατά",
                               "αγροτικού", "δείτε", "πριν", "πού", "με", "το","αγροτικές",
                               "πιο", "όλοι", "φωτό","νέα", "δισ", "δεν", "να","ειδήσεις",
                               "ειδήσεις","αγροτικές", "μέχρι","μετά","γίνει","agrotikanew",
                               "είναι","ανά","νέο","αγροτική","αγροτών","κιλό","https",
                               "€","με","έχει","εφόσον","μια",
                               stopwords("english")),
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
        wordcloud(d$word, d$freq, min.freq=2, max.words=50, scale=c(3,.5), 
                  rot.per=.5,colors=brewer.pal(8, "Dark2"))
    })
}
shinyApp(ui, server)