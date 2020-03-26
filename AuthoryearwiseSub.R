setwd("C:/Users/amitd/Desktop/FA/Authors")
for (i in 1992:2018){
  print(i)
  sub <- paste("sub-", i, ".csv", sep = "")
  assign(paste("dfsub", i, sep = ""),read.csv(sub))
}
new_df <- data.frame(Author = character(0),'1992'=numeric(0),'1993'=numeric(0),'1994'=numeric(0),'1995'=numeric(0),'1996'=numeric(0),'1997'=numeric(0),'1998'=numeric(0),'1999'=numeric(0),'2000'=numeric(0),'2001'=numeric(0),'2002'=numeric(0),'2003'=numeric(0),'2004'=numeric(0),'2005'=numeric(0),'2006'=numeric(0),'2007'=numeric(0),'2008'=numeric(0),'2009'=numeric(0),'2010'=numeric(0),'2011'=numeric(0),'2012'=numeric(0),'2013'=numeric(0),'2014'=numeric(0),'2015'=numeric(0),'2016'=numeric(0),'2017'=numeric(0),'2018'=numeric(0),stringsAsFactors = FALSE)

dfnames <- c("dfsub1992","dfsub1993","dfsub1994","dfsub1995","dfsub1996","dfsub1997","dfsub1998","dfsub1999","dfsub2000","dfsub2001","dfsub2002","dfsub2003","dfsub2004","dfsub2005","dfsub2006","dfsub2007","dfsub2008","dfsub2009","dfsub2010","dfsub2011","dfsub2012","dfsub2013","dfsub2014","dfsub2015","dfsub2016","dfsub2017","dfsub2018")

df.list <- list()

for (i in 1:length(dfnames))
{
  df.list[[i]] <- get(dfnames[i])
}

mylist <- list()

for (df in df.list)
{
  for (row in 1:nrow(df)) 
  {
    name <- as.character(df[row, "rn"])
    if (name %in% mylist){
      next}
    else{
      mylist <- c(mylist,name)}
  }
}

for (i in mylist){
  if (i %in% dfsub1992$rn){
    c=which(dfsub1992$rn == i)
    deg1 <-dfsub1992[c,"degree.cent"]
  }
  else{deg1= ""}
  if (i %in% dfsub1993$rn){
    c=which(dfsub1993$rn == i)
    deg2 <-dfsub1993[c,"degree.cent"]
  }
  else{deg2= ""}
  if (i %in% dfsub1994$rn){
    c=which(dfsub1994$rn == i)
    deg3 <-dfsub1994[c,"degree.cent"]
  }
  else{deg3= ""}
  if (i %in% dfsub1995$rn){
    c=which(dfsub1995$rn == i)
    deg4 <-dfsub1995[c,"degree.cent"]
  }
  else{deg4= ""}
  if (i %in% dfsub1996$rn){
    c=which(dfsub1996$rn == i)
    deg5 <-dfsub1996[c,"degree.cent"]
  }
  else{deg5= ""}
  if (i %in% dfsub1997$rn){
    c=which(dfsub1997$rn == i)
    deg6 <-dfsub1997[c,"degree.cent"]
  }
  else{deg6= ""}
  if (i %in% dfsub1998$rn){
    c=which(dfsub1998$rn == i)
    deg7 <-dfsub1998[c,"degree.cent"]
  }
  else{deg7= ""}
  if (i %in% dfsub1999$rn){
    c=which(dfsub1999$rn == i)
    deg8 <-dfsub1999[c,"degree.cent"]
  }
  else{deg8= ""}
  if (i %in% dfsub2000$rn){
    c=which(dfsub2000$rn == i)
    deg9 <-dfsub2000[c,"degree.cent"]
  }
  else{deg9= ""}
  if (i %in% dfsub2001$rn){
    c=which(dfsub2001$rn == i)
    deg10 <-dfsub2001[c,"degree.cent"]
  }
  else{deg10= ""}
  if (i %in% dfsub2002$rn){
    c=which(dfsub2002$rn == i)
    deg11 <-dfsub2002[c,"degree.cent"]
  }
  else{deg11= ""}
  if (i %in% dfsub2003$rn){
    c=which(dfsub2003$rn == i)
    deg12 <-dfsub2003[c,"degree.cent"]
  }
  else{deg12= ""}
  if (i %in% dfsub2004$rn){
    c=which(dfsub2004$rn == i)
    deg13 <-dfsub2004[c,"degree.cent"]
  }
  else{deg13= ""}
  if (i %in% dfsub2005$rn){
    c=which(dfsub2005$rn == i)
    deg14 <-dfsub2005[c,"degree.cent"]
  }
  else{deg14= ""}
  
  if (i %in% dfsub2006$rn){
    c=which(dfsub2006$rn == i)
    deg15 <-dfsub2006[c,"degree.cent"]
  }
  else{deg15= ""}
  if (i %in% dfsub2007$rn){
    c=which(dfsub2007$rn == i)
    deg27 <-dfsub2007[c,"degree.cent"]
  }
  else{deg27= ""}
  if (i %in% dfsub2008$rn){
    c=which(dfsub2008$rn == i)
    deg16 <-dfsub2008[c,"degree.cent"]
  }
  else{deg16= ""}
  if (i %in% dfsub2009$rn){
    c=which(dfsub2009$rn == i)
    deg17 <-dfsub2009[c,"degree.cent"]
  }
  else{deg17= ""}
  if (i %in% dfsub2010$rn){
    c=which(dfsub2010$rn == i)
    deg18 <-dfsub2010[c,"degree.cent"]
  }
  else{deg18= ""}
  if (i %in% dfsub2011$rn){
    c=which(dfsub2011$rn == i)
    deg19 <-dfsub2011[c,"degree.cent"]
  }
  else{deg19= ""}
  if (i %in% dfsub2012$rn){
    c=which(dfsub2012$rn == i)
    deg20 <-dfsub2012[c,"degree.cent"]
  }
  else{deg20= ""}
  if (i %in% dfsub2013$rn){
    c=which(dfsub2013$rn == i)
    deg21 <-dfsub2013[c,"degree.cent"]
  }
  else{deg21= ""}
  if (i %in% dfsub2014$rn){
    c=which(dfsub2014$rn == i)
    deg22 <-dfsub2014[c,"degree.cent"]
  }
  else{deg22= ""}
  if (i %in% dfsub2015$rn){
    c=which(dfsub2015$rn == i)
    deg23 <-dfsub2015[c,"degree.cent"]
  }
  else{deg23= ""}
  if (i %in% dfsub2016$rn){
    c=which(dfsub2016$rn == i)
    deg24 <-dfsub2016[c,"degree.cent"]
  }
  else{deg24= ""}
  if (i %in% dfsub2017$rn){
    c=which(dfsub2017$rn == i)
    deg25 <-dfsub2017[c,"degree.cent"]
  }
  else{deg25= ""}
  if (i %in% dfsub2018$rn){
    c=which(dfsub2018$rn == i)
    deg26 <-dfsub2018[c,"degree.cent"]
  }
  else{deg26= ""}
  new_df[nrow(new_df)+1, ] <- c(i,deg1,deg2,deg3,deg4,deg5,deg6,deg7,deg8,deg9,deg10,deg11,deg12,deg13,deg14,deg15,deg27,deg16,deg17,deg18,deg19,deg19,deg20,deg21,deg22,deg23,deg24,deg25,deg26)
}

write.csv(new_df, file= "AuthorsYearwiseSub.csv", ,row.names=FALSE)
