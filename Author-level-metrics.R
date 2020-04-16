
sample = read.csv("newsample.csv")
author <- data.frame(sample$Author)


### Loop for all years
for (i in 2006:2017)
{
  print(i)

  ### Loading rda graph object
  load(file = paste0("pub-network-graph-intersected",i,".rda"))
  
  
  ### New empty data frame
  authormetrics = data.frame(Author=character(100),IDInNetwork = numeric(100),Degree=numeric(100),DegCentr=numeric(100),Betweenness=numeric(100),EigenVector=numeric(100),Closeness=numeric(100),stringsAsFactors = FALSE)
  
  ### Looping over sample authors
  for (j in 1:100){
  print(j)
  author=as.character(sample$Author[j])
  #degree(g, v = V(g)[strg])
  
  ###Handling error to catch cases where author is not present in network
  
  id = tryCatch({as.numeric(V(g)[author])}, error = function(e)
  {id = "NA"})
  
  
  ###Author not present in this network, a.k.a did not publish this year
  ###Hence no metrics available for this author for this year
  if (id == "NA"){
  authormetrics$Author[j]=author
  authormetrics$IDInNetwork[j]=""
  authormetrics$Degree[j]=""
  authormetrics$DegCentr[j]=""
  authormetrics$Betweenness[j]=""
  authormetrics$EigenVector[j]=""
  authormetrics$Closeness[j]=""
  next
  }
  
  ###Metrics for authors in network
  
  ###Degree
  degree= as.numeric(degree(g, v = V(g)[author]))
  #degree= as.numeric(degNet[author])
  
  
  ###Degree Centrality (same as degree at node level??)
  degCen= as.numeric(centr_degree(g,mode="all")$res[id])
  #degCen= as.numeric(degCenNet$res[id])
  
  
  ###Betweenness
  btwn= as.numeric(centr_betw(g)$res[id])
  #btwn= as.numeric(BtwnNet$res[id])
  
  
  #Eigen vector value
  eigenvec= as.numeric(centr_eigen(g)$vector[id])
  #eigenvec= as.numeric(EigenNet$vector[id])
  
  
  ###Closeness
  closeness= as.numeric(centr_clo(g)$res[id])
  #closeness= as.numeric(CloseNet$res[id])
  
  
  ### Adding values to data frame
  authormetrics$Author[j]=author
  authormetrics$IDInNetwork[j]=id
  authormetrics$Degree[j]=degree
  authormetrics$DegCentr[j]=degCen
  authormetrics$Betweenness[j]=btwn
  authormetrics$EigenVector[j]=eigenvec
  authormetrics$Closeness[j]=closeness
  
  
  }

  name <- paste("pub-author-metric-sample1-",i, ".csv", sep = "")
  write.csv(authormetrics, name)
} 



  ######Getting degree using index
  #amit = as.data.frame(degree(g, v = V(g)[index]))
  
  ######Getting degreeCentralization using index
  #amit$DegreeCen = centr_degree(g,mode="all")$res[index]
  #centr_degree = as.data.frame(centr_degree(g,mode="all")$res[index])
  
  ######Getting Betweenness using index
  #amit$Betweeness = centr_betw(g)$res[index]
  #btwn = as.data.frame(centr_betw(g)$res[authorvector])
  
  
  ######Getting EigenValue using index
  #amit$EigenVector = centr_eigen(g)$vector[index]
  
  ######Getting Closeness using index
  #amit$Closeness = centr_clo(g)$res[index]
  #clo = centr_clo(g)
  #clo$res[5]
  
  ######Changing Column name
  #colnames(amit)[1]="Degree"
  
  
  #metric <- paste("pub-author-metric-sample-",`i,".csv")
  #name <- paste("pub-author-metric-sample-",i, ".csv", sep = "")
  #write.csv(amit, name)
#}
getwd()
