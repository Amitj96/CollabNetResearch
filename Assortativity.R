library(igraph)
library(Matrix)
getwd()
setwd("C:/Users/amitd/Desktop/FA/Assort")
networkfile = 'karate-fac_edges.txt'
metadatafile = 'karate-fac_labels.txt'
E<-list()
M<-list()

loadPartition <- function(partitionFile, zero_index)
{
  conn <- file(partitionFile,open="r")
  lines <- readLines(conn)
  for (i in 1:length(lines)){
    v<-list(as.integer(strsplit(lines[i], "\\s")[[1]])-zero_index)
    M[i]<<-v
  }
  close(conn)
  return(M)
}




load <- function(networkfile,metadatafile, zero_index)
{
  conn <- file(networkfile,open="r")
  lines <- readLines(conn)
  for (i in 1:length(lines)){
    v<-list(as.integer(strsplit(lines[i], "\\s")[[1]])-zero_index)
    E[i]<<-v
  }
  close(conn)
  M = loadPartition(metadatafile, zero_index)
  EM<-list('E'=E,'M'=M)
  return(EM)
}

E=load(networkfile,metadatafile, 1)$E
M=load(networkfile,metadatafile, 1)$M

n=length(M)
c=length(unique(unlist(M)))


localAssortF<-function(E, M, pr=seq(0, 0.9, by=0.1), undir=TRUE, missingValue=-1)
{

  n=length(M)
  ncomp = length(which(-1 != M))
  m = length(E)
  A = createA(E, m)$A
  degree = createA(E, m)$degree
  D<- diag(1/degree)
  W <- D%*%A
  W
  c=length(unique(unlist(M)))
  if(ncomp < n)
  {
    c = c- 1
  }
  
  Z = matrix(replicate(n, 0))
  Z
  Z[M == missingValue] = 1
  Z = W%*%Z / matrix(degree)

  values <- as.numeric(c(replicate(ncomp, 1)))
  yi <- c(as.integer(which(M != missingValue)))
  yj <- c(as.integer(M[M != missingValue]))
  yj
  Y <- sparseMatrix(i=yi, j=yj, x=values,index1=FALSE)[-1,]
  
  assortM <- array(dim=c(n, length(pr)))
  assortT <- array(dim=c(n))
  eij_glob <- (t(Y)%*%(A%*%Y))
  eij_glob <- eij_glob/sum(eij_glob)
  ab_glob <- sum(colSums(eij_glob)*rowSums(eij_glob))
  WY <- W%*%Y
  print("start iteration")
  
 
  
  for(i in seq(1, 34, by=1))
  {

    pis=calculateRWRrange(A, degree, i, pr, n, trans=TRUE, maxIter=1000)$Fall
    ti=calculateRWRrange(A, degree, i, pr, n, trans=TRUE, maxIter=1000)$T
    it=calculateRWRrange(A, degree, i, pr, n, trans=TRUE, maxIter=1000)$it
    for (ii in seq_along(pr)){
      pi = pis[, ii]
      YPI<-sparseMatrix(i=c(as.integer(M[M != missingValue])), j=seq(0, n-1, by=1)[M != missingValue], x=pi[M != missingValue],index1=FALSE)#[-1,]
      YPI
      trace_e<-sum(diag(YPI%*%WY))
      assortM[i ,ii] = trace_e
    }
    YPI<-sparseMatrix(i=c(as.integer(M[M != missingValue])), j=seq(0, n-1, by=1)[M != missingValue], x=ti[M != missingValue],index1=FALSE)#[-1,]
    YPI
    e_gh<-YPI%*%WY
    Z[i]<-sum(e_gh)
    e_gh<-e_gh/sum(e_gh)
    trace_e<-sum(diag(e_gh))
    assortT[i]<-trace_e
  }
    
  assortM =assortM - ab_glob
  assortM =assortM / (1.-ab_glob + 1e-200)
  
  assortT =assortT - ab_glob
  assortT =assortT / (1.-ab_glob + 1e-200)
  MTZ<-list('assortM'=assortM,'assortT'=assortT,'Z'=Z)
  return(MTZ)
}



createA <- function(E, m)
{
    x = matrix(data=NA, nrow=m, ncol=2)
    
    for(j in 1:2){
      for(i in 1:m){
        x[i,j] = E[[i]][j]
      }
    }
    g<-graph_from_edgelist(x+1, directed = FALSE)
    A<-as_adjacency_matrix(g,sparse = igraph_opt("sparsematrices"))
    A=A/2
    A
    degree <-array(colSums(A))
    AD<-list('A'=A,'degree'=degree)
    return(AD)
  
}


calculateRWRrange<- function(A, degree, i, prs, n, trans=TRUE, maxIter=1000)
{

  pr = tail(prs,1)
  D<- diag(1/degree)
  W<- D%*%A
  diff<- 1
  it<- 1
  F<- matrix(replicate(n, 0))
  Fall <- array(0,dim=c(n, length(prs)))
  F[i] = 1
  Fall[i,] = 1
  Fold = F
  T = F
  if(trans)
  { 
    W<-t(W)
  }
  
  oneminuspr = 1-pr
  while(diff > 1e-9)
  {
    F = pr*W%*%F
    F[i] =F[i]+ oneminuspr
    Fall =Fall + (F-Fold)%o%((prs/pr)^it)
    T = T + (F-Fold)/((it+1)*(pr**it))
    diff = sum((F-Fold)**2)
    it = it+1
    if(it > maxIter){
      print(i, "max iterations exceeded")
      diff=0
    }
    Fold = F
    
  }
  FIT<-list('Fall'=Fall,'T'=T,'it'=it)
  return(FIT)
}


assortT<-localAssortF(E, M, pr=seq(0, 0.9, by=0.1), undir=TRUE, missingValue=-1)$assortT
assortM<-localAssortF(E, M, pr=seq(0, 0.9, by=0.1), undir=TRUE, missingValue=-1)$assortM

