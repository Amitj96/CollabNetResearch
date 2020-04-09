install.packages("igraph") # network object transformation and statistcs.
library(igraph) # load igraph for network functions
# Loop through per year. Instrumentation to mark the progress when run in Linux. 
setwd("C:/Users/amitd/Desktop/FA/QuartilesData")
getwd()
for (i in 1997:2018)
{
  start.time <- Sys.time()
  name <- paste("df",i, ".csv", sep = "")
  df.auths<- read.csv(name, na.strings=c("","NA"))
  #df.auths<- read.csv("C:/Users/amitd/Desktop/FA/df1997.csv", na.strings=c("","NA"))
  nrow <- dim(df.auths)[1] # count the number of rows: this is the total number of submissions for that year.
  ncol <- dim(df.auths)[2] 
  df.auths$teamsize <- rowSums(!is.na(df.auths)) # Create a column containing teamsize by using rowSums to count the number not NA cells, to get the count of authors on a submission.
  
  teamsize_summary <- summary(df.auths$teamsize)
  
  min_teamsize <- teamsize_summary[1]
  teamsize_1stquartile <- teamsize_summary[2]
  teamsize_median <- teamsize_summary[3]
  teamsize_mean <- teamsize_summary[4]
  teamsize_3rdquartile <- teamsize_summary[5]
  teamsize_Max <- teamsize_summary[6]
  
  
  N <- data.frame(df.auths)
  N$iz.na <- is.na(N[,2]) # If the second column has NA's, this indicates solo-authorshup.
  false_aka_coAUTH <- table(N$iz.na)[1]
  true_aka_SOLO <- table(N$iz.na)[2]
  
  
  # Artifact of the data submission process, i.e., submitter only puts own name, even if produced a pub with a team, or truly reflective of the process co-authorship?
  # The solo authors may or may not be connected to the giant component. For now, we are excluding them from analysis because data formatting code 
  # gives issues. They will likely be engaged in other collaborations, however, so we shouldadd them somehow. Ask Jeff.
  
  N<-N[!N$iz.na,] # Remove the rows with 
  N$iz.na <- NULL # Remove the NA column.
  N$teamsize <- NULL # Remove the teamsize column. 
  portion_coauthorship <- false_aka_coAUTH/nrow
  portion_soloauthorship <- true_aka_SOLO/nrow
  
  tmpN <- t(combn(N[1,!is.na(N[1,])], 2)) #all possible combinations of pairs in first row then transposed.
  
  tmpN[1,] #the first pair
  df.N <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)  # Order doesn't matter. 
  # This is not a permutation; (I checked) there are no "reverse" pairings. 
  # i.e. Honda and Yakayama only appear as "H, Y" not "Y, H". 
  
  # Loop for all rows to transpose the authors into a format for iGraph analysis. 
  
  for (my.row in 2:dim(N)[1]) 
  {
    # my.row <- 1 + my.row
    # print(my.row)
    tmpN <- t(combn(N[my.row,!is.na(N[my.row,])], 2))
    tmp.df <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)
    
    colnames(tmp.df) <- c("Auth1", "Auth2")
    
    # tmp.df$ref.years<- ref.years[my.row] # Add if you want to give pair attributes, such as the year.
    # tmp.df$ref.type <- ref.type[my.row]  # Add if you want to give pair attributes, such as the reference type.
    
    df.N <- rbind(df.N, tmp.df)
    gc() # for every var written and overwritten, R 'holds onto the memory'. Allows R to recalim memory. Will keep vars in global environment. 
    
  }
  # Write the Linkedlist of the submission network for that year to a .csv file. 
  
  # no spaces = paste0.
  
  linkedlist <- paste("publication-linkedlistfirstq-", i, ".csv", sep = "")
  write.csv(df.N, linkedlist)
  
  
  ######################################## End Cleaning and Formatting of data ############################################
  
  ########################################################################################################################
  ###################################### Network: Create Graph object and Calculate parametrics of network ###############
  ########################################################################################################################
  
  
  df2<-df.N
  g <- graph.data.frame(df2, directed=FALSE)  
  E(g)$weight <- 1 
  g <- simplify(g, edge.attr.comb="sum") # Simplify: Simple graphs are graphs which do not contain loop and multiple edges. 
  # The simplify function collapses multiple collaborations by same authors. 
  # We summed the collaborations and added it as a graph attribute when we added: edge.attr.comb="sum"
  
  
  ### Calculate the giant components ##################
  
  comp <- clusters(g)    # Calculate the maximal (weakly or strongly) connected components of a graph
  subg <- induced.subgraph(g, which(comp$membership == which.max(comp$csize)))  # subgraph creates a subgraph of a graph. containing only the vertices and edges of components with the largest size: (max(comp$csize)) 
  clu <- components(g) # Calculate the maximal (weakly or strongly) connected components of a graph
  # View(clu) # Contains the items in each component, with cluster assignment number. 
  groups <- groups(clu) # group author names into clusters.Create a list of vertex groups from some graph clustering or community structure.
  subg <- induced.subgraph(g, which(comp$membership == which.max(comp$csize)))
  
  ###### Network statistics: Record the parameters ################
  nodeNum <- vcount(g) # number of nodes, that is, authors. The graph is simplified, so this is unique number of authors.
  edgeNum <- ecount(g) # number of links. Does this include the count of the links? If it is less than the total number of edges from the count of the false_akaCOAUTH
  
  meandegree <- round(edgeNum/nodeNum,digits=2)
  no.cluster <- no.clusters(g) # numeric constant, the number of clusters.
  simplified <- simplify(g)
  density <-round( graph.density(g,loops=FALSE),digit=6) # The density of a graph is the ratio of the number of edges and the number of possible edges.
  
  ### Parameters of the giant component ###
  node.giant <- vcount(subg)
  edge.giant <- ecount(subg)
  size.giant <- (node.giant/nodeNum)*100
  diameter<-diameter(subg)
  cluster.coeffient <- (transitivity(subg))*100
  
  assortativity <- (assortativity.degree(subg))*100
  unweighted <- max(degree(subg))
  namnet <- simplify(subg)
  weighted <- max(degree(namnet))
  unweighted <- max(degree(subg))
  
  
  #############################################################
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  timetaken <- time.taken
  
  network.metric <- c(nrow, ncol, false_aka_coAUTH, true_aka_SOLO, portion_coauthorship, portion_soloauthorship, min_teamsize, teamsize_1stquartile,
                      teamsize_median, teamsize_mean, teamsize_3rdquartile, teamsize_Max, nodeNum, edgeNum, no.cluster, density, node.giant, 
                      edge.giant, size.giant, meandegree, weighted, unweighted, cluster.coeffient,assortativity, timetaken)
  
  
  metric.name <- c("nrow", "ncol", "false_aka_coAUTH", "true_aka_SOLO", "portion_coauthorship", "portion_soloauthorship", "min_teamsize", "teamsize_1stquartile",
                   "teamsize_median", "teamsize_mean", "teamsize_3rdquartile", "teamsize_Max", "nodeNum", "edgeNum", "no.cluster", "density", "node.giant", 
                   "edge.giant", "size.giant", "meandegree", "weighted", "unweighted", "cluster.coeffient", "assortativity", "timetaken")
  
  net.metric <- data.frame(metric.name, network.metric)
  
  metric <- paste0("pub-network-metric-firstq-",i,".csv")
  
  write.csv(net.metric, metric) 
  
  networkRDA <- paste0("pub-network-graph-firstq",i,".rda")
  save(g, file=networkRDA)
  
  
  
  # 3 files get created: 1) linked (rda), 2) network (rda), 3) file of metrics (csv)
  
  # Instrumenting: what loop we're on 
  print(paste("Loop: ", i, "nrow: ", nrow, "ncol:", ncol)) # readability, interpretabilty: instrumenting! knittr! 
  
}

