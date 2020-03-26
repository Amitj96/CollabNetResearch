setwd("C:/Users/amitd/Desktop/FA/Authors")


install.packages("dbConnect") # connect to MySQL database
install.packages("splitstackshape") # parse out names with cSplit function.
install.packages("igraph") # network object transformation and statistcs.

library(dbConnect) # load dbConnect to establish the connection with the database
library(RMySQL)    # load RMySQL to access the functions for MySQL 
library(splitstackshape) # load splitstackshape for manipulation and cleaning
library(igraph) # load igraph for network functions
library(plyr)


# Loop through per year. Instrumentation to mark the progress when run in Linux. 
for (i in 1992:2018){
  dbDisconnect(con)
  con <- dbConnect(MySQL(),user="********", 
                   password="*********",
                   host="**********", 
                   dbname="**********") # Define the database connection through SSH provide the pw and select db.
  
  query <- paste0("SELECT r.authors,r.id FROM RefPublication as r WHERE r.year='",i,"';") # Make a string to loop through 1994-2018
  
  D <- dbGetQuery(con, query) # Query the database with the string defined above.
  
  
  df.auths<- data.frame(D) # make the query results a dataframe
  
  # parse out the "and" and substitute for the three variants in formatting for the author names. 
  df.auths$authors<- gsub(" and",",", df.auths$authors) 
  df.auths$authors <- gsub(" and ",",", df.auths$authors)
  df.auths$authors <- gsub("and ",",", df.auths$authors)
  
  df.auths<- cSplit(df.auths, "authors", ".,",'wide') # split the author cell into columns (horizontally), where comma is the delimiter.
  
  nrow <- dim(df.auths)[1] # count the number of rows: this is the total number of submissions for that year.
  ncol <- dim(df.auths)[2]
  authorslist <- paste0("AuthorlistPub-",i,".csv")
  write.csv(df.auths, authorslist ,row.names=FALSE)
}
  
