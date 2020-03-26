df = read.csv("C:/Users/amitd/Desktop/FA/intersectedAuthors.csv")


length(colnames(df))

x <- as.numeric(gsub("X", "",colnames(df)[-1]))


M <- as.matrix(df[,2:22])
range(M)

length(x)
dim(M)

plot(x, seq(from=0, to = max(M), length.out = length(x)), type = "n",xlab = "Years", ylab = "Publications")

for (i in 1:nrow(M)) {
  lines(x, jitter(M[i, ]), col = rgb(1,0,0,alpha = .015))
}

lines(x, colMeans(M), col = "black", lwd = 2)

##########First Quartile#######

firstquartile = c()
for (i in 1:ncol(M)) {
  firstquartile[i] <- quantile(M[,i], probs = 0.25, na.rm = FALSE)
  }
lines(x, firstquartile, col = "green", lwd = 3)

###########Third Quartile######

thirdquartile = c()
for (i in 1:ncol(M)) {
  thirdquartile[i] <- quantile(M[,i], probs = 0.75, na.rm = FALSE)
}
lines(x, thirdquartile, col = "yellow", lwd = 2)

legend(1998, 20, legend=c("First Quartile", "Mean","Third Quartile"),
       col=c("green", "black","yellow"), lty=1, cex=0.8)

firstquartile
thirdquartile
summary(M[,1])


#################################################################################################################



install.packages("matrixStats")
library(matrixStats)
stdev = colSds(M)

df1 = read.csv("C:/Users/amitd/Desktop/FA/newsample.csv")
df1

length(colnames(df1))

x <- c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)

Mat <- as.matrix(df1[,2:23])
range(Mat)

length(x)
dim(Mat)

plot(x, seq(from=0, to = 21, length.out = length(x)), type = "n",xlab = "Years", ylab = "Publications")

for (i in 1:nrow(Mat)) {
  lines(x, Mat[i,-22 ], col = ifelse(Mat[i,22] <= 1,rgb(1,0,0,alpha = .2),ifelse(Mat[i,22] >= 3.285714,rgb(0.15,0.25,0.36,alpha = .2),rgb(0.22,1.00,0.35,alpha = .3))))
}


