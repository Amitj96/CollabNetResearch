install.packages("igraph")
library(igraph)

# we will read in a graph object, saved as .rda. 
# start with data submission, test for one year (1998)

# getwd()
setwd("C:/Users/amitd/Desktop/FA/publication/publication")
df1 = data.frame(year=integer(0),percent.v.connected=numeric(0),percent.e.connected=numeric(0))
for (i in 1992){
  year = i
  load(file = paste0("pub-network-graph",i,".rda"))
  
  # let's get some network "statistical" properties, first.
  # How many authors in giant component? How many in the periphery? 
  
  ###################### Calculate the giant components. 
  # clusers() will calculate the maximal (weakly or strongly) connected components of a graph.
  comp <- clusters(g)
  count_components(g) # 4385 components, number of clusters found, not printing the actual clusters. 
  # f <- component_distribution(g, cumulative = FALSE, mul.size = FALSE)
  subg <- induced.subgraph(g, which(comp$membership == which.max(comp$csize)))
  subg
  number.of.authors <- vcount(subg)
  number.of.connections <- ecount(subg)
  # calculate the percent giant component
  degree.cent <- degree(subg, v = V(subg), mode = c("all"))
  percent.v.connected <- (vcount(subg) / vcount(g))*100 # number of authors in subg / total number of authors 
  percent.e.connected <- (ecount(subg) / ecount(g))*100 # number of connections in subg / total number of connections. 
  #df1$percent.v.connected <-percent.v.connected
  #df1$percent.e.connected <-percent.e.connected
  df1[nrow(df1)+1, ] <- c(year,percent.v.connected, percent.e.connected)
  
  # Interpretation: The giant connected component(s) contain(s) 40% of the authors and 60% of the links. Which means there are a majority of authors who are not 
  # in the giant component, but they do not have a majority of the links. Makes sense; they are publishing in fragments, while the giant 
  # component is, by definition, the most connect subgraph in the network. What does this mean if it changes? 
  
  
  ################# Visualize subgraph. Set attributes for nodes and edges. 
  E(subg)$weight <- 0.1
  subg <- simplify(subg, edge.attr.comb="sum")
  
  
  #cut.off <- mean(links$weight)
  #subg <- delete_edges(subg, E(net)[weight<cut.off])
  V(subg)$size <- (degree.cent*0.01)##0.01
  
  #V(subg)$size <- 0.2
  E(subg)$width <- E(subg)$weight
  V(subg)$label <- ""
  #E(subg)$color <- "green" # data submission network is seagreen3.
  V(subg)$color <- "red"
  subg
  #E(subg)$width <- E(subg)$weight/6
  #edge_attr(subg)
  
  # plot the whole network. caution, might take a few mins.
  
  start.time <- Sys.time()
  pdf(paste0("PublicationNetworkImp",i,".pdf")) 
  plot.igraph(subg, edge.arrow.size=0,edge.arrow.width=0,vertex.frame.color=adjustcolor("red", alpha.f = 0), main=paste("Publication co-authorship network", year),edge.color = adjustcolor("SkyBlue2", alpha.f = .015),vertex.color = adjustcolor("red", alpha.f = .25))
  #plot(subg, edge.color="orange", vertex.color="gray50")alpha edge color 1992:1994 0.5,1995:2000 0.1 2000:2008 0.075 : 2009:2011 0.05 2012:2015 0.025
  #vertex color 1992:1994 1,1995:2000 2001:2008 0.05: 2009:2011 0.04 2012:2015 0.025
  dev.off()
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  print(year)
}
View(df1)
