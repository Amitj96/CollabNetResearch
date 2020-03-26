install.packages("igraph")
library(igraph)

setwd("C:/Users/amitd/Desktop/FA/publication/publication")
i=1997
load(file = paste0("pub-network-graph",i,".rda"))
df = read.csv("C:/Users/amitd/Desktop/FA/sampleof100.csv")
df
neighbours <- make_ego_graph(g, order = 2, nodes = df$Authors, mindist = 0)
neighbours
