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
                 stopwords = c("agronewsgr","για","και","από","των", "οι", "...","ώστε","μέσα", "αυτά","περίπου",
                               "την","στις","της","του","τον","τους", "τα","να", "τέλος","στιγμή", "ούτε", "μία", "ακόμη","παράδειγμα",
                               "τις","στους","αύριο","στην","προς", "θα", "ως", "ευρώ","κάτι", "είπε","ενώ","πως","έχω","λένε","κάποια",
                               "που","στα","κάθε","λέει","στο","στη", "σε", "agrenda","όσο", "πάνω", "δούμε", "κάνει","εμείς","απόφαση",
                               "ζωντανά","αγρότες","αγροτικής","μήνα", "τη", "φεκ","όχι","μπορεί","εκεί","βεβαίως","καμία","μπορούμε",
                               "ημέρες","μέρες","στον", "έως", "λόγω", "εκατ","ότι","ήδη", "όλες", "έχω","ακόμα","αυτή","αυτές",
                               "αγροτικό","ζητά","αλλά","χωρίς", "προ", "ύψους","όλα","όπου", "τσιρώνης", "ίδια", "εγώ","υπάρχουν",
                               "αγροτικού", "δείτε", "πριν", "πού", "με", "το","πάλι","σημείωσε", "οποία", "κύριο","εάν","χώρο","άνθρωποι",
                               "πιο", "όλοι", "φωτό","νέα", "δισ", "δεν", "να","μια","άλλων","μόνο","ήταν","αφορά","μόνο","χώρα", 
                               "ειδήσεις","αγροτικές", "μέχρι","μετά","γίνει","έχουν","χώρας","δήλωσε","news","λοιπόν","έτσι","αποτελεί",
                               "είναι","ανά","νέο","αγροτική","αγροτών","κιλό","https","δόθηκαν","επίσης","μεγάλο","κάνουν","μεγάλη",
                               "σας","μας","αυτό","έχει","όπως","θέλουν","καθώς","ένα","ελλάδα","είμαστε","δημοσιογράφος","όμως",
                               "έχουμε","κάνουμε","θέμα","επιπλέον","μην","μου","είτε","στοιχεία","μάλιστα","πρώτη","πρέπει","ένας",
                               "κατά", "υπό", "πώς", "δις", "όταν", "αποστόλου", "υπουργός", "τότε", "διότι","βάση","είχαμε","δίνει",
                               "αυτήν","μέσω","χώρου","κυρίως", "μεταξύ","αυτοί","δηλαδή", "ξέρουν","άλλη","σχετικά","οποίες","τρόπο",
                               "πρώτα","όλη","τώρα","τόνισε","πολύ","οποίο","γιατί","μπορούν","περιοχές","δύο","μιλάμε","κάποιες",
                               "τόσο","σήμερα","λέω","πληρώσει","ώρα","εδώ","yoleni","μέρα","άλλες","πάρει","εεδ","γιάννης","θέματα",
                               "γίνεται","άρα","νέας","όλους","ποια","πρώτο","άλλα","θέλω","υπουργέ","συριζα", "θέλω","θέλει","υπάρξει",
                               "περίοδο","χρόνο", "υπάρχει","τι","αν","κι","σημασία","λπ","περίπτωση","-","yoleni's", "ββ", "ξένοι",
                               "τομέα","κόσμος","αναπληρωτής","ελληνική","κόσμος","αυτών","πλέον","χρόνια","έρχονται","σχέση","τομέας",
                               "κάποιος","βλέπουμε","υποστηρίζουμε","δώσουμε","μεγάλες","δε","πάει","κλπ","γι'","κάτω","πίσω","παρά",
                               "επομένως","αυτός","κόσμο","κύριε","πω","μεγάλα","άλλο","πιστεύω","πάρουν","ας","έναν","είπα","ίδιο","καλά",
                               "ιδιαίτερα","πούμε","πολλές","μη","πει","μαζί","επειδή","έγινε","γι'","αυτούς","πολλά","πάρα", "αυτούς",
                               "θέλουμε","κάναμε","θυμίζω","φορά","βοηθήσουμε","είχαν","προκειμένου","πάνε","έχετε","τέτοια","λίγο","είχαν",
                               "προσπαθούμε","είχε","δημοσιογράφοσ","υπουργόσ","σειρά","αυτού","πάμε","πασεγεσ","όσα","σχεδόν","όλων",
                               "εκ","κύριος","ενώ","άλλωστε","υπουργείο","διαδικασία","δουλειά","δυο","πραγματικά","υποστηρίξουμε",
                               "άρθρο","πλαίσιο","καν","σύμφωνα","παράγραφος","υπόψη","στοιχείο","εν","αριθ","αναφέρονται","αφορούν","€",
                               "σχετικές","βάσει","κανονισμού","εε","σημείο","παα","μέτρου","μέτρων","",
                                "βλέπε","σύνολο","πίνακας","άρθρου","όσον","|","εντός", "με",
                               "agronewsgr", "https", "amp", "food", "read", "https","without","bring","usda", "agency","gov","navigation",
                               "thanks", "look", "looking", "see", "people", "way","fms","hemp","due","information","press","release","releases",
                               "gaid", "agchattruth", "foodtruth","agchat","want","list","make","range","site","commission","europa","european",
                               "agriculture","will", "talk", "now", "new", "forget","like","using","ghg","member","policy","search","also",
                               "farm", "thank", "book", "year", "week", "photo","today","farmers","states","tools","div","gpt","count",
                               "add", "can", "truths", "life", "news", "every","lelyknowhow","farming","fao","said","wfp","office","america",
                               "time", "action", "know", "share", "just", "found","always","day","farms","graziano","silva","communities",
                               "latest", "find", "get", "great", "table", "join", "word", "countries","across","director","program",
                               "thunderclap", "excited", "better", "voice", "video","use","asa","ahdb","vilsack","secretary","programs",               
                               "paper", "getting", "blogs", "help", "one","unfao","via","...","_","agrotikanew",
                               "best", "animal", "need", "joy", "many", "awesome", "support", "first","million","national","service",
                               "futureofcap","sacfarmclimate","offers","shows","play","-","means","hort","percent","|","l","state",
                               "allow","provides","nfutweets","never","century","address","say","since","conservation","opportunity",
                               "assistance","general","world", "almost","philhoganeu","farmwildlifeuk","min","nyc","around","devex",
                               "xhnews","review","agrocapital","honda","civic","cor","bryanndyaguma","katrinasasse","aislisbon","tobloc",
                               stopwords("english")),
                 removeNumbers = TRUE, tolower = TRUE))
m <- as.matrix(myDtm) # Converting to matrix
v <- sort(rowSums(m), decreasing=TRUE) # Calculating frequency of words
myNames <- names(v) # Getting words
d <- data.frame(word=myNames, freq=v) # Creating dataframe with each word and its frequency
script.dir <- getSrcDirectory(function(x) {x})
print(script.dir)
setwd(script.dir)
png("../twittercloudgr.png", width=400,height=400)
wordcloud(d$word, d$freq, min.freq=2, max.words=50, scale=c(3,.5), 
          rot.per=.5,colors=brewer.pal(8, "Dark2"))
dev.off()
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