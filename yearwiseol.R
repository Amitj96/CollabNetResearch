setwd("C:/Users/amitd/Desktop/FA/Authors")
for (i in 1992:2018){
  print(i)
  pub <- paste("pub-", i, ".csv", sep = "")
  assign(paste("dfpub", i, sep = ""),read.csv(pub))
}
new_df <- data.frame(Author = character(0),'1992'=numeric(0),'1993'=numeric(0),'1994'=numeric(0),'1995'=numeric(0),'1996'=numeric(0),'1997'=numeric(0),'1998'=numeric(0),'1999'=numeric(0),'2000'=numeric(0),'2001'=numeric(0),'2002'=numeric(0),'2003'=numeric(0),'2004'=numeric(0),'2005'=numeric(0),'2006'=numeric(0),'2007'=numeric(0),'2008'=numeric(0),'2009'=numeric(0),'2010'=numeric(0),'2011'=numeric(0),'2012'=numeric(0),'2013'=numeric(0),'2014'=numeric(0),'2015'=numeric(0),'2016'=numeric(0),'2017'=numeric(0),'2018'=numeric(0),stringsAsFactors = FALSE)

dfnames <- c("dfpub1992","dfpub1993","dfpub1994","dfpub1995","dfpub1996","dfpub1997","dfpub1998","dfpub1999","dfpub2000","dfpub2001","dfpub2002","dfpub2003","dfpub2004","dfpub2005","dfpub2006","dfpub2007","dfpub2008","dfpub2009","dfpub2010","dfpub2011","dfpub2012","dfpub2013","dfpub2014","dfpub2015","dfpub2016","dfpub2017","dfpub2018")

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
    if (i %in% dfpub1992$rn){
      c=which(dfpub1992$rn == i)
      deg1 <-dfpub1992[c,"degree.cent"]
    }
    else{deg1= ""}
  if (i %in% dfpub1993$rn){
    c=which(dfpub1993$rn == i)
    deg2 <-dfpub1993[c,"degree.cent"]
  }
  else{deg2= ""}
  if (i %in% dfpub1994$rn){
    c=which(dfpub1994$rn == i)
    deg3 <-dfpub1994[c,"degree.cent"]
  }
  else{deg3= ""}
  if (i %in% dfpub1995$rn){
    c=which(dfpub1995$rn == i)
    deg4 <-dfpub1995[c,"degree.cent"]
  }
  else{deg4= ""}
  if (i %in% dfpub1996$rn){
    c=which(dfpub1996$rn == i)
    deg5 <-dfpub1996[c,"degree.cent"]
  }
  else{deg5= ""}
  if (i %in% dfpub1997$rn){
    c=which(dfpub1997$rn == i)
    deg6 <-dfpub1997[c,"degree.cent"]
  }
  else{deg6= ""}
  if (i %in% dfpub1998$rn){
    c=which(dfpub1998$rn == i)
    deg7 <-dfpub1998[c,"degree.cent"]
  }
  else{deg7= ""}
  if (i %in% dfpub1999$rn){
    c=which(dfpub1999$rn == i)
    deg8 <-dfpub1999[c,"degree.cent"]
  }
  else{deg8= ""}
  if (i %in% dfpub2000$rn){
    c=which(dfpub2000$rn == i)
    deg9 <-dfpub2000[c,"degree.cent"]
  }
  else{deg9= ""}
  if (i %in% dfpub2001$rn){
    c=which(dfpub2001$rn == i)
    deg10 <-dfpub2001[c,"degree.cent"]
  }
  else{deg10= ""}
  if (i %in% dfpub2002$rn){
    c=which(dfpub2002$rn == i)
    deg11 <-dfpub2002[c,"degree.cent"]
  }
  else{deg11= ""}
  if (i %in% dfpub2003$rn){
    c=which(dfpub2003$rn == i)
    deg12 <-dfpub2003[c,"degree.cent"]
  }
  else{deg12= ""}
  if (i %in% dfpub2004$rn){
    c=which(dfpub2004$rn == i)
    deg13 <-dfpub2004[c,"degree.cent"]
  }
  else{deg13= ""}
  if (i %in% dfpub2005$rn){
    c=which(dfpub2005$rn == i)
    deg14 <-dfpub2005[c,"degree.cent"]
  }
  else{deg14= ""}
  
  if (i %in% dfpub2006$rn){
    c=which(dfpub2006$rn == i)
    deg15 <-dfpub2006[c,"degree.cent"]
  }
  else{deg15= ""}
  if (i %in% dfpub2007$rn){
    c=which(dfpub2007$rn == i)
    deg27 <-dfpub2007[c,"degree.cent"]
  }
  else{deg27= ""}
  if (i %in% dfpub2008$rn){
    c=which(dfpub2008$rn == i)
    deg16 <-dfpub2008[c,"degree.cent"]
  }
  else{deg16= ""}
  if (i %in% dfpub2009$rn){
    c=which(dfpub2009$rn == i)
    deg17 <-dfpub2009[c,"degree.cent"]
  }
  else{deg17= ""}
  if (i %in% dfpub2010$rn){
    c=which(dfpub2010$rn == i)
    deg18 <-dfpub2010[c,"degree.cent"]
  }
  else{deg18= ""}
  if (i %in% dfpub2011$rn){
    c=which(dfpub2011$rn == i)
    deg19 <-dfpub2011[c,"degree.cent"]
  }
  else{deg19= ""}
  if (i %in% dfpub2012$rn){
    c=which(dfpub2012$rn == i)
    deg20 <-dfpub2012[c,"degree.cent"]
  }
  else{deg20= ""}
  if (i %in% dfpub2013$rn){
    c=which(dfpub2013$rn == i)
    deg21 <-dfpub2013[c,"degree.cent"]
  }
  else{deg21= ""}
  if (i %in% dfpub2014$rn){
    c=which(dfpub2014$rn == i)
    deg22 <-dfpub2014[c,"degree.cent"]
  }
  else{deg22= ""}
  if (i %in% dfpub2015$rn){
    c=which(dfpub2015$rn == i)
    deg23 <-dfpub2015[c,"degree.cent"]
  }
  else{deg23= ""}
  if (i %in% dfpub2016$rn){
    c=which(dfpub2016$rn == i)
    deg24 <-dfpub2016[c,"degree.cent"]
  }
  else{deg24= ""}
  if (i %in% dfpub2017$rn){
    c=which(dfpub2017$rn == i)
    deg25 <-dfpub2017[c,"degree.cent"]
  }
  else{deg25= ""}
  if (i %in% dfpub2018$rn){
    c=which(dfpub2018$rn == i)
    deg26 <-dfpub2018[c,"degree.cent"]
  }
  else{deg26= ""}
  new_df[nrow(new_df)+1, ] <- c(i,deg1,deg2,deg3,deg4,deg5,deg6,deg7,deg8,deg9,deg10,deg11,deg12,deg13,deg14,deg15,deg27,deg16,deg17,deg18,deg19,deg19,deg20,deg21,deg22,deg23,deg24,deg25,deg26)
}

write.csv(new_df, file= "AuthorsYearwise.csv", ,row.names=FALSE)
