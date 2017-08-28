library(readr)
library(RPostgreSQL)
mystring <- read_file("/home/iliastsergoulas/Downloads/programme.txt")
df <- data.frame(title="Πρόγραμμα Αγροτικής Ανάπτυξης 2014-2020", date="11/12/2015",
                 person_gr="Πρόγραμμα Αγροτικής Ανάπτυξης 2014-2020",person_en="Rural Development Programme 2014-2020 - Greece",
                 text=mystring,url="www.agrotikianaptixi.gr", stringsAsFactors=FALSE)
credentials<-read.csv("/home/iliastsergoulas/Dropbox/Website/dbcredentials.csv")
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # creates a connection to the postgres database
                 host = "88.99.13.199", port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
dbWriteTable(con, c("agriculture", "agritexts"), value = df, append = TRUE, row.names = FALSE)
dbDisconnect(con)
dbUnloadDriver(drv)