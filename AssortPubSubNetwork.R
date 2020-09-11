library(igraph)
library(Matrix)
setwd("C:/Users/amitd/Desktop/FA/publication/publication")
load("pub-network-graph2012.rda")
pubg<-g
V(pubg)$label <- 1
V(pubg)$label

setwd("C:/Users/amitd/Desktop/FA/datasubmissions/datasubmissions")
load("sub-network-graph2012.rda")
subg<-g
V(subg)$label <- 2
subg
V(subg)$label

g <- subg %u% pubg
V(g)$label_2


pub.v.index <- which(!is.na(V(g)$label_2))
sub.v.index <- which(!is.na(V(g)$label_1))
in.both.v.index <- intersect(pub.v.index, sub.v.index)

V(g)$label[pub.v.index] <- 1
V(g)$label[sub.v.index] <- 2
V(g)$label[in.both.v.index] <- 3
V(g)$namesA<-V(g)$name
g <- set.vertex.attribute(g, "name", value=1:length(V(g)$name))

V(g)
E(g)


h<-as_edgelist(g, names = TRUE)
M<-as.integer(V(g)$label)
h
M
length(M)



E<-list()
for(i in 1:nrow(h)){
  E[i]<-list(as.integer(h[i,]))
}
E



n=length(M)
c=length(unique(unlist(M)))
n
c
localAssortF<-function(E, M, pr=seq(0, 0.5, by=0.1), undir=TRUE, missingValue=-1)
{
  
  n=length(M)
  ncomp = length(which(-1 != M))
  m = length(E)
  A = createA(E, m)$A
  degree = createA(E, m)$degree
  #D<- bandSparse(n,n,0,list(rep(1/degree, n+1)))#as(diag(1/degree), "dgCMatrix") #matCSC <- as(matBaseR, "dgCMatrix") #typeof(D)#as_adjacency_matrix(1/degree,sparse = igraph_opt("sparsematrices"))
  
  D<-Diagonal(x=1/degree)
  W <- D%*%A
  
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
  
  
  
  for(i in seq(1, n, by=1))
  {
    print(i)
    pis=calculateRWRrange(A,W,D, degree, i, pr, n, trans=TRUE, maxIter=1000)$Fall
    ti=calculateRWRrange(A,W,D, degree, i, pr, n, trans=TRUE, maxIter=1000)$T
    it=calculateRWRrange(A,W,D, degree, i, pr, n, trans=TRUE, maxIter=1000)$it
     for (ii in seq_along(pr)){
       #print("forloop2")
       pi = pis[, ii]
       YPI<-sparseMatrix(i=c(as.integer(M[M != missingValue])), j=seq(0, n-1, by=1)[M != missingValue], x=pi[M != missingValue],index1=FALSE)#[-1,]
       trace_e<-sum(diag(YPI%*%WY))
       assortM[i ,ii] = trace_e
     }
    YPI<-sparseMatrix(i=c(as.integer(M[M != missingValue])), j=seq(0, n-1, by=1)[M != missingValue], x=ti[M != missingValue],index1=FALSE)#[-1,]
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
  print("Create")
  # x = matrix(data=NA, nrow=m, ncol=2)
  # 
  # for(j in 1:2){
  #   for(i in 1:m){
  #     x[i,j] = E[[i]][j]
  #   }
  # }
  #g<-graph_from_edgelist(h, directed = FALSE)
  A<-as_adjacency_matrix(g,sparse = igraph_opt("sparsematrices"))
  A
  degree <-array(colSums(A))
  AD<-list('A'=A,'degree'=degree)
  return(AD)
  
}


calculateRWRrange<- function(A,W,D, degree, i, prs, n, trans=TRUE, maxIter=1000)
{
  #print("CalRange")
  pr = tail(prs,1)
  # D= D
  # W=W
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
  F
  oneminuspr = 1-pr
  while(diff > 1e-9)
  {
    #print("whileloop")
    F = pr*W%*%F
    #print("whileloop1")
    F[i] =F[i]+ oneminuspr
    #print("whileloop1.1")
    Fall =Fall + (F-Fold)%o%((prs/pr)^it)
    #print("whileloop2")
    T = T + (F-Fold)/((it+1)*(pr**it))
    #print("whileloop3")
    diff = sum((F-Fold)**2)
    it = it+1
    if(it > maxIter){
      print("max iterations exceeded")
      diff=0
    }
    Fold = F
    #print("whileloop ends")
  }
  FIT<-list('Fall'=Fall,'T'=T,'it'=it)
  return(FIT)
}
Cstack_info()

assortT<-localAssortF(E, M, pr=seq(0, 0.5, by=0.1), undir=TRUE, missingValue=-1)$assortT
assortM<-localAssortF(E, M, pr=seq(0, 0.2, by=0.1), undir=TRUE, missingValue=-1)$assortM
 
 min(assortT)
 max(assortT)
 summary(assortT)
V(g)$assort <- assortT
summary(assortT)
PS2012 <- paste0("PubSub2012Assort.rda")
save(g, file="PS2012.rda")
getwd()
s
load("PS2002.rda")
install.packages("plotrix")
library(plotrix)
tmp <- round(rescale(V(g)$assort, c(1,255)),0)
summary(tmp)

V(g)$color <- colorRampPalette(c("blue" ,'white',"red"))(255)[tmp]

par(mar = c(0,0,0,0))
plot.igraph(g,main="Assortativity 2002",v.alpha=.50,vertex.frame.color=adjustcolor("red", alpha.f = 0),edge.color = adjustcolor("black", alpha.f = .015), vertex.size = 0.5,vertex.label=NA,edge.width=0.1)

col.labels<-c("Disassortative","Uniform Mixing","Assortative")

legend(x = "bottomleft",      
         legend = col.labels,
         pch = 10,              
         col = V(g)$color,
         bty = "n")

