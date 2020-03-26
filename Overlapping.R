install.packages("igraph")
library(igraph)
library(data.table)
require(dplyr)

#setwd("C:/Users/amitd/Desktop/FA/publication/publication")
overlappercent <- data.frame(year=integer(0),DataSubmissions=integer(0),Publications=integer(0),Patents=integer(0),OverlapDSPub=integer(0),OverlapDSPat=integer(0),OverlapPubPat=integer(0),Percentdspub=numeric(0),Percentdspat=numeric(0),Percentpubpat=numeric(0))
for (i in 1992:2018){
  setwd("C:/Users/amitd/Desktop/FA/publication/publication")
  year = i
  load(file = paste0("pub-network-graph",i,".rda"))
  
  comp <- clusters(g)
  count_components(g)
  subg <- induced.subgraph(g, which(comp$membership == which.max(comp$csize)))
  subg
  degree.cent <- degree(subg, v = V(subg), mode = c("all"))
  bn<-as.data.frame(degree.cent)
  bn1 <- setDT(bn, keep.rownames = TRUE)[]
  bn2<- bn1 %>% top_n(1000, degree.cent)
  pub <- paste("pub-", i, ".csv", sep = "")
  write.csv(bn2,pub)
  #bn2<- head(order(bn1$degree.cent,decreasing=TRUE), n = 1000)
  


    
  setwd("C:/Users/amitd/Desktop/FA/datasubmissions/datasubmissions")
  load(file = paste0("sub-network-graph",i,".rda"))
  
  comp <- clusters(g)
  count_components(g)
  subg <- induced.subgraph(g, which(comp$membership == which.max(comp$csize)))
  subg
  degree.cent <- degree(subg, v = V(subg), mode = c("all"))
  degree.cent
  bn3<-as.data.frame(degree.cent)
  bn4 <- setDT(bn3, keep.rownames = TRUE)[]
  bn5<- bn4 %>% top_n(1000, degree.cent)
  sub <- paste("sub-", i, ".csv", sep = "")
  write.csv(bn5,sub)

  
  setwd("C:/Users/amitd/Desktop/FA/patent/patent")

    
  load(file = paste0("pat-network-graph",i,".rda"))
  
  comp <- clusters(g)
  count_components(g)
  subg <- induced.subgraph(g, which(comp$membership == which.max(comp$csize)))
  subg
  degree.cent <- degree(subg, v = V(subg), mode = c("all"))
  bn6<-as.data.frame(degree.cent)
  bn7 <- setDT(bn6, keep.rownames = TRUE)[]
  bn8<- bn7 %>% top_n(1000, degree.cent)
  pat <- paste("pat-", i, ".csv", sep = "")
  write.csv(bn8,pat)
  
  
  dsandpub<-merge(x=bn5,y=bn2,by="rn")
  dsandpat<-merge(x=bn5,y=bn8,by="rn")
  pubandpat<-merge(x=bn2,y=bn8,by="rn")
  
  overlap1 <- paste("dsandpub-", i, ".csv", sep = "")
  overlap2 <- paste("dsandpat-", i, ".csv", sep = "")
  overlap3 <- paste("pubandpat-", i, ".csv", sep = "")
  write.csv(dsandpub, overlap1)
  write.csv(dsandpat, overlap2)
  write.csv(pubandpat, overlap3)
  numds <- nrow(bn3)
  numpub<-nrow(bn)
  numpat<-nrow(bn6)
  numdspub <- nrow(dsandpub)
  numdspat <- nrow(dsandpat)
  numpubpat <- nrow(pubandpat)
  perc1 <- nrow(dsandpub)/1000
  perc2 <- nrow(dsandpat)/1000
  perc3 <- nrow(pubandpat)/1000
  overlappercent[nrow(overlappercent)+1, ] <- c(year,numds,numpub,numpat,numdspub,numdspat,numpubpat,perc1*100, perc2*100,perc3*100)

  
  print(year)
}
write.csv(overlappercent, file= "OverlapPercent.csv", ,row.names=FALSE)


