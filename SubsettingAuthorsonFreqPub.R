setwd("C:/Users/amitd/Desktop/FA")
for (i in 1992:2018){
#df<-read.csv("C:/Users/amitd/Desktop/FA/AuthorFrequencyPub2005.csv")
name <- paste("AuthorFrequencyPub",i, ".csv", sep = "")
df<-read.csv(name)
subdf = subset(df, Frequency<=21 & Frequency>=2)
authorsfreq <- paste0("SubsetAuthorFrequency-",i,".csv")
write.csv(subdf, authorsfreq ,row.names=FALSE)
}
