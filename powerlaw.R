install.packages("poweRlaw")
library("poweRlaw")
df<-read.csv("C:/Users/amitd/Desktop/FA/Authors.csv")
df <- df[order(-df$Frequency),]
pl_m = displ$new(df$Frequency)

#####Initial xmin and scaling parameter
pl_m$getXmin()
pl_m$getPars()

#####Set some value for xmin and scaling parameter

pl_m$setXmin(2)
pl_m$setPars(2)

#####Estimate corresponding alpha for xmin

estimate_pars(pl_m)


(est_pl = estimate_xmin(pl_m))

pl_m$setXmin(est_pl)
######Plot 
plot(pl_m)
