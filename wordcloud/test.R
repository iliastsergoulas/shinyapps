library(twitteR)
library(wordcloud)
library(tm)
library(base64enc)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)

setup_twitter_oauth("mBT6s3eZaNUT8D6WVXakDvf8R", 
                    "u1J8XAQu0BAZensQZBOehelUG0I9wPklgmmaTZ7vQ3yLjM2yAy", 
                    "801401824855072772-sZjXHFGWvJvcBZGK8n88kmvbBhUEzjv", 
                    "Dr8TpgfJx8Q1NZXiJGIW91hnMzpzQyHekqfamOk3VEepW")
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
                               "αγροτικό",
                               stopwords("english")),
                 removeNumbers = TRUE, tolower = TRUE))

m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="miners")
myNames[k] <- "mining"
d <- data.frame(word=myNames, freq=v)
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(d$word, d$freq, min.freq=3, colors=brewer.pal(8, "Dark2"))
dev.off()