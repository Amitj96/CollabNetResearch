install.packages("readxl")
library("readxl")
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

setwd("C:/Users/amitd/Desktop/FA")

df <- read.csv("master_patent_networkmetrics.csv") 
df1 <- read_excel("master_datasubmissions.xlsx")
df <- within(df, rm(teamsize_median))


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

##For Patent

dfcor <- cor(df)


##Correlation matrix with significance values(p-values)
dfrcorr = rcorr(as.matrix(df))
dfrcorr

##Extract values
dfcoeff = dfrcorr$r
dfp = dfrcorr$P

##Plottinng correlation matrix
corrplot(dfcoeff)

dfcorflat<- flattenCorrMatrix(dfrcorr$r, dfrcorr$P)
dfcorflat <- dfcorflat[order(-abs(dfcorflat$cor)),]

chart.Correlation(df, histogram=TRUE, pch="25")
###PerformanceAnalytics
chart.Correlation(df[, c(1,2,4,5,6,7)], histogram=TRUE, pch="25")
chart.Correlation(df[, c(8,9,10,11,12)], histogram=TRUE, pch="25")
chart.Correlation(df[, c(13,14,15,16)], histogram=TRUE, pch="25")
chart.Correlation(df[, c(17,18,19,20,21,22,23,24,24)], histogram=TRUE, pch="25")


##For Data submission

df1cor <- cor(df1)
##Correlation matrix with significance values(p-values)
df1rcorr = rcorr(as.matrix(df1))
df1rcorr

##Extract values
df1coeff = df1rcorr$r
df1p = df1rcorr$P

##Plottinng correlation matrix
corrplot(df1coeff)

df1corflat<- flattenCorrMatrix(df1rcorr$r, df1rcorr$P)
df1corflat <- df1corflat[order(-abs(df1corflat$cor)),]

###PerformanceAnalytics
chart.Correlation(df1[, c(1,2,4,5,6,7)], histogram=TRUE, pch="25")
chart.Correlation(df1[, c(8,9,10,11,12)], histogram=TRUE, pch="25")
chart.Correlation(df1[, c(13,14,15,16)], histogram=TRUE, pch="25")
chart.Correlation(df1[, c(17,18,19,20,21,22,23,24,24)], histogram=TRUE, pch="25")

chart.Correlation(df, histogram=TRUE, pch="25")
