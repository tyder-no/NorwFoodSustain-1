options(encoding="UTF-8")
#
# source("ssb-json-tests-1.R")
#
#
#
#

library(httr)
# rjstat is used for converting SSB JSON -> Data frame
library(rjstat)
# jsonlite is used mainly for converting metadata 
library(jsonlite)
# Reshape is used for filtering/transforming/grouping 
library(reshape)
#
library(sqldf)
#

source("ssb-json-functions.R")

#
#   NVEs API for magasinfylling
#   curl -X GET "https://nvebiapi.nve.no/api/Magasinstatistikk/HentOffentligData" -H "accept: application/json" > nvefylling.json
#   curl -X GET "https://nvebiapi.nve.no/api/Magasinstatistikk/HentOffentligDataSisteUke" -H "accept: application/json" > nvefylling_21-52.json
#
#








query2DF <- function(jsQuery){
    fromJSON(jsQuery)
}

df2Query0 <- function(df) {
    toJSON(df)
}

df2Query <- function(df) {
    toJSON(df)
}


# Tables for testing etc

bankSelection <-
"
07459:  Alders- og kjønnsfordeling i kommuner, fylker og hele landets befolkning (K) 1986 - 2019
07984 	Sysselsatte, etter bosted, arbeidssted, kjønn, alder og næring (17 grupper, SN2007). 4. kvartal (K) 	2008 - 2018
        -> derivat av de to over? 06445 	Andel sysselsatte i befolkningen, etter bosted, kjønn og alder. 4. kvartal (K) 	2005 - 2018
     
12558 	Desilfordelt inntekt for husholdninger. Høyeste verdi, antall og prosent (K) 	2005 - 2017
06462 	Jordbruksareal, etter bruken (dekar) (K) 	1969 - 2018
12660 	Husdyr på utmarksbeite (K) 	1995 - 2018
07366 	Produktivt skogareal (dekar) (K) 	2008 - 2017
     
     
03375 	Framskrevet folkemengde per 01.01, alternativ MMMM (K) (2002-framskrivingen) 	2002 - 2020
03376 	Framskrevet folkemengde per 01.01, etter kjønn og ettårig alder i 14 alternativer (2002-framskrivingen) 	2002 - 2050
     
05903 	Framskrevet folkemengde etter kjønn og ettårig alder i 13 alternativer (2005-framskrivingen) 	2005 - 2060
05904 	Framskrevet folkemengde etter kjønn og alder i 9 alternativer (K) (2005-framskrivingen) 	2005 - 2025
     
06916 	Framskrevet folkemengde etter kjønn og ettårig alder i 14 alternativer (2008-framskrivingen) 	2008 - 2060
06917 	Framskrevet folkemengde etter kjønn og alder i 9 alternativer (K) (2008-framskrivingen) 	2008 - 2030
     
07267 	Framskrevet folkemengde etter kjønn og ettårig alder i 14 alternativer (2009-framskrivingen) 	2009 - 2060
07268 	Framskrevet folkemengde etter kjønn og alder i 9 alternativer (K) (2009-framskrivingen) 	2009 - 2030
     
08108 	Framskrevet folkemengde etter kjønn og ettårig alder i 14 alternativer (2010-framskrivingen) 	2010 - 2060
08109 	Framskrevet folkemengde etter kjønn og alder i 9 alternativer (K) (2010-framskrivingen) 	2010 - 2030
     
08824 	Framskrevet folkemengde, etter kjønn, alder, innvandringskategori og landbakgrunn, i 14 alternativer (2011-framskrivingen) 	2011 - 2100
08825 	Framskrevet folkemengde, etter kjønn og alder, i 9 alternativer (K) (2011-framskrivingen) 	2011 - 2040
     
09481 	Framskrevet folkemengde, etter kjønn, alder, innvandringskategori og landbakgrunn, i 15 alternativer (2012-framskrivingen) 	2012 - 2100
09482 	Framskrevet folkemengde etter kjønn og alder, i 9 alternativer (K) (B) (2012-framskrivingen) 	2012 - 2040
     
10212 	Framskrevet folkemengde, etter kjønn, alder, innvandringskategori og landbakgrunn, i 15 alternativer (2014-framskrivingen) 	2014 - 2100
10213 	Framskrevet folkemengde etter kjønn og alder, i 9 alternativer (K) (B) (2014-framskrivingen) 	2014 - 2040
     
11167 	Framskrevet folkemengde 1. januar, etter kjønn, alder, innvandringskategori og landbakgrunn, i 15 alternativer (2016-framskrivingen) 	2016 - 2100
11168 	Framskrevet folkemengde 1. januar, etter kjønn og alder, i 9 alternativer (K) (B) (2016-framskrivingen) 	2016 - 2040
     
11667 	Framskrevet folkemengde 1. januar, etter kjønn, alder, innvandringskategori og landbakgrunn, i 15 alternativer 	2018 - 2100
11668 	Framskrevet folkemengde 1. januar, etter kjønn og alder, i 9 alternativer (K) (B) 	2018 - 2040
"


tstTabs <- c("07459","07984","12558","06462","12660","07366","03375","03376","05903","05904","06916","06917","07267","07268",
  "08108","08109","08824","08825","09481","09482","10212","10213","11167","11168","11667","11668")

tstTabs2 <- c("06445","03013","05327")
tstTabs3 <- c("01302","01313")     # 1996, 1999-framskrivningene
tstTabs4 <- c("03014","08307","08313","12824") # Electricity data etc

# Uses API to get metadata, saves it as R data frames for further processing

readAndSaveMeta <- function(tblList) {
    for (tblNum in tblList) {
        mDf <- getRMetaDataFrame(tblNum) ;
        save(mDf,file=paste('Rd_meta/md_',tblNum,'.Rdata',sep=''))
    }
}

# Reads in metadata frames, creates search markup dfs

createAndSaveSearchMarkup <- function(tblList) {
    for (tblNum in tblList) {
        load(file=paste('Rd_meta/md_',tblNum,'.Rdata',sep=''))
        mVL <- getDFValuesAndLabels(mDf)
        save(mVL,file=paste('Rd_meta/sdf_',tblNum,'.Rdata',sep=''))
    }
}




fetchAndSave01302 <- function(){
    load(file=paste('Rd_meta/sdf_','01302','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    for (i in c(1,2)) mVL$Region$Slct[i] = 1
    for (i in seq(84,109)) mVL$Region$Slct[i] = 1
    for (i in c(111,184,213,260,291,311,362,379,390,525,575,659,686,739,807)) mVL$Region$Slct[i] = 1
    for (i in seq(496,525)) mVL$Region$Slct[i] = 1
   
    mVL$ContentsCode$Slct[1]=10
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q01302 <- createSearchFromDF(mVL)
    jd01302 <- getJSONData("01302",q01302)
    save(jd01302,file=paste('Rd_data/jd_','01302','.Rdata',sep=''))
    q01302
}

fetchAndSave01313 <- function(){
    load(file=paste('Rd_meta/sdf_','01313','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    for (i in c(1,2)) mVL$Region$Slct[i] = 1
    for (i in seq(84,109)) mVL$Region$Slct[i] = 1
    for (i in c(111,184,213,260,291,311,362,379,390,525,575,659,686,739,807)) mVL$Region$Slct[i] = 1
    for (i in seq(496,525)) mVL$Region$Slct[i] = 1
   
    mVL$ContentsCode$Slct[1]=10
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q01313 <- createSearchFromDF(mVL)
    jd01313 <- getJSONData("01313",q01313)
    save(jd01313,file=paste('Rd_data/jd_','01313','.Rdata',sep=''))
    q01313
}

fetchAndSave03375 <- function(){
    load(file=paste('Rd_meta/sdf_','03375','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    for (i in c(1,2)) mVL$Region$Slct[i] = 1
    for (i in seq(84,109)) mVL$Region$Slct[i] = 1
    for (i in c(111,184,213,260,291,311,362,379,390,525,575,659,686,739,807)) mVL$Region$Slct[i] = 1
    for (i in seq(496,525)) mVL$Region$Slct[i] = 1
   
    mVL$ContentsCode$Slct[1]=10
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q03375 <- createSearchFromDF(mVL)
    jd03375 <- getJSONData("03375",q03375)
    save(jd03375,file=paste('Rd_data/jd_','03375','.Rdata',sep=''))
    q03375
}

fetchAndSave07268 <- function(){
    load(file=paste('Rd_meta/sdf_','07268','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    for (i in c(1,2)) mVL$Region$Slct[i] = 1
    for (i in seq(84,109)) mVL$Region$Slct[i] = 1
    for (i in c(111,184,213,260,291,311,362,379,390,525,575,659,686,739,807)) mVL$Region$Slct[i] = 1
    for (i in seq(496,525)) mVL$Region$Slct[i] = 1
   
    mVL$ContentsCode$Slct[1]=1
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q07268 <- createSearchFromDF(mVL)
    jd07268 <- getJSONData("07268",q07268)
    save(jd07268,file=paste('Rd_data/jd_','07268','.Rdata',sep=''))
    q07268
}


fetchAndSave10213 <- function(){
    load(file=paste('Rd_meta/sdf_','10213','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    for (i in c(1,2)) mVL$Region$Slct[i] = 1
    for (i in seq(21,45)) mVL$Region$Slct[i] = 1
    for (i in c(65,88,115,137,152,171,187,203,230,292,330,356,380,425,450)) mVL$Region$Slct[i] = 1
    for (i in seq(265,291)) mVL$Region$Slct[i] = 1
   
    mVL$ContentsCode$Slct[1]=1
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q10213 <- createSearchFromDF(mVL)
    jd10213 <- getJSONData("10213",q10213)
    save(jd10213,file=paste('Rd_data/jd_','10213','.Rdata',sep=''))
    q10213
}


fetchAndSave06445 <- function(){
    load(file=paste('Rd_meta/sdf_','06445','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q06445 <- createSearchFromDF(mVL)
    jd06445 <- getJSONData("06445",q06445)
    save(jd06445,file=paste('Rd_data/jd_','06445','.Rdata',sep=''))
    q06445
}

fetchAndSave03013 <- function(){
    load(file=paste('Rd_meta/sdf_','03013','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$ContentsCode$Slct[1] = 1
     for (i in c(1:13)) mVL$Konsumgrp$Slct[i] <- 1
  
    q03013 <- createSearchFromDF(mVL)
    jd03013 <- getJSONData("03013",q03013)
    save(jd03013,file=paste('Rd_data/jd_','03013','.Rdata',sep=''))
}

fetchAndSave05327 <- function(){
    load(file=paste('Rd_meta/sdf_','05327','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$ContentsCode$Slct[1] = 1
    mVL$Konsumgrp$Slct[1] <- 10
  
    q05327 <- createSearchFromDF(mVL)
    jd05327 <- getJSONData("05327",q05327)
    save(jd05327,file=paste('Rd_data/jd_','05327','.Rdata',sep=''))
}


fetchAndSave07459 <- function(){
    load(file=paste('Rd_meta/sdf_','07459','.Rdata',sep='')) # get mVL
    #mVL$Tid$Slct[1] <- 10
    for (i in c(26,27,28,29,30,31,32,33)) mVL$Tid$Slct[i] <- 1
    mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    #mVL$Kjonn$Slct[1]=10
    mVL$Alder$Slct[1]=10
    
    q07459 <- createSearchFromDF(mVL)
    jd07459 <- getJSONData("07459",q07459)
    save(jd07459,file=paste('Rd_data/jd_','07459','.Rdata',sep=''))
    q07459      
}

fetchAndSave07459b <- function(){
    load(file=paste('Rd_meta/sdf_','07459','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    for (i in c(1,2)) mVL$Region$Slct[i] = 1
    for (i in seq(84,109)) mVL$Region$Slct[i] = 1
    for (i in c(111,184,213,260,291,311,362,379,390,525,575,659,686,739,807)) mVL$Region$Slct[i] = 1
    for (i in seq(496,525)) mVL$Region$Slct[i] = 1
    #for (i in c(26,27,28,29,30,31,32,33)) mVL$Tid$Slct[i] <- 1
    
    mVL$ContentsCode$Slct[1] = 10
    
    #mVL$Region$Slct[1]=10
    mVL$Kjonn$Slct[1]=10
    mVL$Alder$Slct[1]=10
    
    q07459 <- createSearchFromDF(mVL)
    jd07459b <- getJSONData("07459",q07459)
    save(jd07459b,file=paste('Rd_data/jd_','07459b','.Rdata',sep=''))
    q07459      
}




fetchAndSave07984 <- function(){
    load(file=paste('Rd_meta/sdf_','07984','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    #mVL$NACE2007$Slct[1]=10  # Can't order everything, too many cells, get 403 response
    mVL$Alder$Slct[1]=10
    mVL$Kjonn$Slct[1]=10

    q07984 <- createSearchFromDF(mVL)
    jd07984 <- getJSONData("07984",q07984)
    save(jd07984,file=paste('Rd_data/jd_','07984','.Rdata',sep=''))
}




fetchAndSave12558 <- function(){
    load(file=paste('Rd_meta/sdf_','12558','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$InntektSkatt$Slct[1] = 10


    mVL$Desiler$Slct[1] = 10
    mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    q12558 <- createSearchFromDF(mVL)
    jd12558 <- getJSONData("12558",q12558)
    save(jd12558,file=paste('Rd_data/jd_','12558','.Rdata',sep=''))
}


fetchAndSave06462 <- function(){
    load(file=paste('Rd_meta/sdf_','06462','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$VekstarDekar$Slct[1] = 10
    #mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    q06462 <- createSearchFromDF(mVL)
    jd06462 <- getJSONData("06462",q06462)
    save(jd06462,file=paste('Rd_data/jd_','06462','.Rdata',sep=''))
}


fetchAndSave12660 <- function(){
    load(file=paste('Rd_meta/sdf_','12660','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    q12660 <- createSearchFromDF(mVL)
    jd12660 <- getJSONData("12660",q12660)
    save(jd12660,file=paste('Rd_data/jd_','12660','.Rdata',sep=''))
}


fetchAndSave07366 <- function(){
    load(file=paste('Rd_meta/sdf_','07366','.Rdata',sep='')) # get mVL
    mVL$Tid$Slct[1] <- 10
    mVL$ContentsCode$Slct[1] = 10
    mVL$Region$Slct[1]=10
    q07366 <- createSearchFromDF(mVL)
    jd07366 <- getJSONData("07366",q07366)
    save(jd07366,file=paste('Rd_data/jd_','07366','.Rdata',sep=''))
}


fetchAndSave03014 <- function(){
#    load(file=paste('Rd_meta/sdf_','03014','.Rdata',sep='')) # get mVL
#    mVL$Tid$Slct[1] <- 10
#    mVL$ContentsCode$Slct[1] = 10
#    mVL$Region$Slct[1]=10
#    q03014 <- createSearchFromDF(mVL)
  q03014a <-
   ' {
  "query": [
    {
      "code": "Konsumgrp",
      "selection": {
        "filter": "vs:CoiCop2016niva1",
        "values": []
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "KpiAar"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
 q03014b <-
'{
  "query": [
    {
      "code": "Konsumgrp",
      "selection": {
        "filter": "vs:CoiCop2016niva4",
        "values": [
          "04.5.1"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "KpiAar"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
    
    jd03014 <- getJSONData("03014",q03014a)
    jd03014b <- getJSONData("03014",q03014b)
    
    save(jd03014,file=paste('Rd_data/jd_','03014','.Rdata',sep=''))
    save(jd03014b,file=paste('Rd_data/jd_','03014b','.Rdata',sep=''))
    
}


fetchAndSave08307 <- function(){
 #   load(file=paste('Rd_meta/sdf_','08307','.Rdata',sep='')) # get mVL
 #   mVL$Tid$Slct[1] <- 10
 #   mVL$ContentsCode$Slct[1] = 10
 #   mVL$Region$Slct[1]=10
 #   q08307 <- createSearchFromDF(mVL)
   q08307 <-
   '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }'

    
    jd08307 <- getJSONData("08307",q08307)
    save(jd08307,file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
}



fetchAndSave08313 <- function(){
 #   load(file=paste('Rd_meta/sdf_','08313','.Rdata',sep='')) # get mVL
 #   mVL$Tid$Slct[1] <- 10
 #   mVL$ContentsCode$Slct[1] = 10
 #   mVL$Region$Slct[1]=10
 #   q08313 <- createSearchFromDF(mVL)
    q08313 <-
   '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }'

    
    jd08313 <- getJSONData("08313",q08313)
    save(jd08313,file=paste('Rd_data/jd_','08313','.Rdata',sep=''))
}



fetchAndSave12824 <- function(){
   # load(file=paste('Rd_meta/sdf_','12824','.Rdata',sep='')) # get mVL
   # mVL$Tid$Slct[1] <- 10
   # mVL$ContentsCode$Slct[1] = 10
   # mVL$Region$Slct[1]=10
   # q12824 <- createSearchFromDF(mVL)
  q12824 <-
   '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }'

    
    jd12824 <- getJSONData("12824",q12824)
    save(jd12824,file=paste('Rd_data/jd_','12824','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))
}


massage06462 <- function(){
   load(file=paste('Rd_data/jd_','06462','.Rdata',sep=''))
   jd06462$ContentsCode <- NULL
   jd06462$ContentsCode <-  jd06462$VekstarDekar
   jd06462$VekstarDekar <- NULL ;
   jd06462
                      
}

massage12558 <- function(){
    load(file=paste('Rd_data/jd_','12558','.Rdata',sep=''))
    jd12558a <- jd12558[jd12558$ContentsCode=='VerdiDesil',]
    jd12558a$ContentsCode <- NULL
    jd12558a$ContentsCode <-  jd12558a$Desiler
    jd12558a$Desiler <- NULL ;
    jd12558a
    
}

massage07984 <- function(){
    load(file=paste('Rd_data/jd_','07984','.Rdata',sep=''))
    jd07984a <- jd07984[jd07984$ContentsCode=='Sysselsatte',]
    jd07984a$ContentsCode <- NULL
    jd07984a$ContentsCode <-  jd07984a$Alder
    jd07984a$Alder <- NULL ;
    jd07984a
    
}


massage07459 <- function(){
    load(file=paste('Rd_data/jd_','07459','.Rdata',sep=''))
    jd07459$ContentsCode <- NULL
    jd07459$ContentsCode <-  jd07459$Alder
    jd07459$Alder <- NULL ;
    jd07459
    
}

massage07459b <- function(){
    load(file=paste('Rd_data/jd_','07459b','.Rdata',sep=''))
    jd07459b$ContentsCode <- NULL
    jd07459b$ContentsCode <-  jd07459b$Alder
    jd07459b$Alder <- NULL ;
    jd07459b
    
}


massage03013 <- function(){
    load(file=paste('Rd_data/jd_','03013','.Rdata',sep=''))
    jd03013$ContentsCode <- NULL
    jd03013$ContentsCode <-  jd03013$Konsumgrp
    jd03013$Konsumgrp <- NULL ;
    jd03013
    
}

massage05327 <- function(){
    load(file=paste('Rd_data/jd_','05327','.Rdata',sep=''))
    jd05327$ContentsCode <- NULL
    jd05327$ContentsCode <-  jd05327$Konsumgrp
    jd05327$Konsumgrp <- NULL ;
    jd05327
    
}


massage01302 <- function(){
    load(file=paste('Rd_data/jd_','01302','.Rdata',sep=''))
    jd01302$ContentsCode <- NULL
    jd01302$ContentsCode <-  jd01302$Alder
    jd01302$Alder <- NULL ;
    jd01302
    
}

massage01313 <- function(){
    load(file=paste('Rd_data/jd_','01313','.Rdata',sep=''))
    jd01313$ContentsCode <- NULL
    jd01313$ContentsCode <-  jd01313$Alder
    jd01313$Alder <- NULL ;
    jd01313
    
}

massage03375 <- function(){
    load(file=paste('Rd_data/jd_','03375','.Rdata',sep=''))
    jd03375$ContentsCode <- NULL
    jd03375$ContentsCode <-  jd03375$Alder
    jd03375$Alder <- NULL ;
    jd03375
    
}

massage07268 <- function(){
    load(file=paste('Rd_data/jd_','07268','.Rdata',sep=''))
    jd07268$ContentsCode <- NULL
    jd07268$ContentsCode <-  jd07268$Alder
    jd07268$Alder <- NULL ;
    jd07268
    
}



castOnContentsCode12558 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region","InntektSkatt"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid+InntektSkatt~ContentsCode)
    cD

}

castOnContentsCode07984 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region","Kjonn"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid+Kjonn~ContentsCode)
    cD

}

castOnContentsCode03013 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode"))
    mY$eV <- NULL ;
    cD <- cast(mY,Tid~ContentsCode)
    cD

}


castOnContentsCode05327 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode"))
    mY$eV <- NULL ;
    cD <- cast(mY,Tid~ContentsCode)
    cD

}

castOnContentsCodeKjonn <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region","Kjonn"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid+Kjonn~ContentsCode)
    cD

}

castOnContentsCode <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid~ContentsCode)
    cD

}


dsJoinTid <- function(ds1,ds2) {

    ds2$T2 <- ds2$Tid ; ds2$Tid <- NULL ; 
    ds3 <- sqldf("SELECT d1.*, d2.* FROM ds1 d1 LEFT JOIN ds2 d2 ON d1.Tid = d2.T2 ")
    ds3$T2 <- NULL ;
    ds3

}    


dsJoinRegionTid <- function(ds1,ds2) {

    ds2$T2 <- ds2$Tid ; ds2$Tid <- NULL ;  ds2$R2 <- ds2$Region ; ds2$Region <- NULL ;
    ds3 <- sqldf("SELECT d1.*, d2.* FROM ds1 d1 LEFT JOIN ds2 d2 ON d1.Region = d2.R2 AND d1.Tid = d2.T2 WHERE d1.Region>'0'")
    ds3$T2 <- NULL ; ds3$R2 <- NULL ;
    ds3

}    



#    readAndSaveMeta(tstTabs)
#    createAndSaveSearchMarkup(tstTabs)
#    readAndSaveMeta(tstTabs2)
#    createAndSaveSearchMarkup(tstTabs2)

#    readAndSaveMeta(tstTabs4)
#    createAndSaveSearchMarkup(tstTabs4)



#md12660=load(file=paste('Rd_meta/md_','12660','.Rdata',sep=''))
#md07366=getValuesAndLabels("07366")

#fetchAndSave12660()
#fetchAndSave07366()
#fetchAndSave06462()
#fetchAndSave12558()
#fetchAndSave07984()
#q0 <- fetchAndSave07459()
#q0 <- fetchAndSave07459b()


#q0 <- fetchAndSave06445()
#q0 <- fetchAndSave03013()
#q0 <- fetchAndSave05327()




# Befolkningsframskrivninger
#q0 <- fetchAndSave01302()
#q0 <- fetchAndSave01313()
#q0 <- fetchAndSave03375()
#q0 <- fetchAndSave07268()
#q0 <- fetchAndSave10213()








#jd07366
#load(file=paste('Rd_data/jd_','07366','.Rdata',sep=''))

#jd12660
#load(file=paste('Rd_data/jd_','12660','.Rdata',sep=''))

#jd07366[0:100,]


# load(file=paste('Rd_data/jd_','06462','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07366','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','12660','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','12558','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07984','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07459','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','06445','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','03013','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','05327','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','01302','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','01313','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','03375','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07268','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','10213','.Rdata',sep=''))



#ds3 <- dsJoinRegionTid(castOnContentsCode(jd07366),castOnContentsCode(jd12660))
#ds4 <- dsJoinRegionTid(castOnContentsCode(jd07366),castOnContentsCode(massage06462()))
#ds5 <- dsJoinRegionTid(castOnContentsCode(jd07366),castOnContentsCode12558(massage12558()))
#ds6 <- dsJoinRegionTid(castOnContentsCode(jd07366),castOnContentsCode07984(massage07984()))
#ds7 <- dsJoinRegionTid(castOnContentsCode(jd07366),castOnContentsCode(massage07459()))

#ds8 <-  dsJoinTid(castOnContentsCode03013(massage03013()),castOnContentsCode05327(massage05327()))

#b2019 <- castOnContentsCodeKjonn(massage07459b())


#kpiDs <- castOnContentsCode03013(massage03013())
#kpiJAEDs <- castOnContentsCode05327(massage05327())

#f96 <- castOnContentsCodeKjonn(massage01302()) 
#f99 <- castOnContentsCodeKjonn(massage01313()) 

fetchAndSave03014 <- function(){
#    load(file=paste('Rd_meta/sdf_','03014','.Rdata',sep='')) # get mVL
#    mVL$Tid$Slct[1] <- 10
#    mVL$ContentsCode$Slct[1] = 10
#    mVL$Region$Slct[1]=10
#    q03014 <- createSearchFromDF(mVL)
  q03014a <-
   ' {
  "query": [
    {
      "code": "Konsumgrp",
      "selection": {
        "filter": "vs:CoiCop2016niva1",
        "values": []
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "KpiAar"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'
 q03014b <-
'{
  "query": [
    {
      "code": "Konsumgrp",
      "selection": {
        "filter": "vs:CoiCop2016niva4",
        "values": [
          "04.5.1"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "KpiAar"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'
    
    jd03014a <- getJSONData("03014",q03014a)
    jd03014b <- getJSONData("03014",q03014b)
    
    save(jd03014a,file=paste('Rd_data/jd_','03014a','.Rdata',sep=''))
    save(jd03014b,file=paste('Rd_data/jd_','03014b','.Rdata',sep=''))
    # load(file=paste('Rd_data/jd_','03014','.Rdata',sep=''))
    # load(file=paste('Rd_data/jd_','03014b','.Rdata',sep=''))
    
    
}


fetchAndSave08307 <- function(){
 #   load(file=paste('Rd_meta/sdf_','08307','.Rdata',sep='')) # get mVL
 #   mVL$Tid$Slct[1] <- 10
 #   mVL$ContentsCode$Slct[1] = 10
 #   mVL$Region$Slct[1]=10
 #   q08307 <- createSearchFromDF(mVL)
   q08307 <-
   '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }'

    
    jd08307 <- getJSONData("08307",q08307)
    save(jd08307,file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
     # load(file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
}



fetchAndSave08313 <- function(){
 #   load(file=paste('Rd_meta/sdf_','08313','.Rdata',sep='')) # get mVL
 #   mVL$Tid$Slct[1] <- 10
 #   mVL$ContentsCode$Slct[1] = 10
 #   mVL$Region$Slct[1]=10
 #   q08313 <- createSearchFromDF(mVL)
    q08313 <-
   '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }'

    
    jd08313 <- getJSONData("08313",q08313)
    save(jd08313,file=paste('Rd_data/jd_','08313','.Rdata',sep=''))
 # load(file=paste('Rd_data/jd_','08313','.Rdata',sep=''))
    
}



fetchAndSave12824 <- function(){
   # load(file=paste('Rd_meta/sdf_','12824','.Rdata',sep='')) # get mVL
   # mVL$Tid$Slct[1] <- 10
   # mVL$ContentsCode$Slct[1] = 10
   # mVL$Region$Slct[1]=10
   # q12824 <- createSearchFromDF(mVL)
  q12824 <-
   '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }'

    
    jd12824 <- getJSONData("12824",q12824)
    save(jd12824,file=paste('Rd_data/jd_','12824','.Rdata',sep=''))

     # load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))
}

   # load(file=paste('Rd_data/jd_','03014','.Rdata',sep=''))
   # load(file=paste('Rd_data/jd_','03014b','.Rdata',sep=''))
   # load(file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
   # load(file=paste('Rd_data/jd_','08313','.Rdata',sep=''))
   # load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))


massage06462 <- function(){
   load(file=paste('Rd_data/jd_','06462','.Rdata',sep=''))
   jd06462$ContentsCode <- NULL
   jd06462$ContentsCode <-  jd06462$VekstarDekar
   jd06462$VekstarDekar <- NULL ;
   jd06462
                      
}

massage12558 <- function(){
    load(file=paste('Rd_data/jd_','12558','.Rdata',sep=''))
    jd12558a <- jd12558[jd12558$ContentsCode=='VerdiDesil',]
    jd12558a$ContentsCode <- NULL
    jd12558a$ContentsCode <-  jd12558a$Desiler
    jd12558a$Desiler <- NULL ;
    jd12558a
    
}

massage07984 <- function(){
    load(file=paste('Rd_data/jd_','07984','.Rdata',sep=''))
    jd07984a <- jd07984[jd07984$ContentsCode=='Sysselsatte',]
    jd07984a$ContentsCode <- NULL
    jd07984a$ContentsCode <-  jd07984a$Alder
    jd07984a$Alder <- NULL ;
    jd07984a
    
}


massage07459 <- function(){
    load(file=paste('Rd_data/jd_','07459','.Rdata',sep=''))
    jd07459$ContentsCode <- NULL
    jd07459$ContentsCode <-  jd07459$Alder
    jd07459$Alder <- NULL ;
    jd07459
    
}

massage07459b <- function(){
    load(file=paste('Rd_data/jd_','07459b','.Rdata',sep=''))
    jd07459b$ContentsCode <- NULL
    jd07459b$ContentsCode <-  jd07459b$Alder
    jd07459b$Alder <- NULL ;
    jd07459b
    
}


massage03013 <- function(){
    load(file=paste('Rd_data/jd_','03013','.Rdata',sep=''))
    jd03013$ContentsCode <- NULL
    jd03013$ContentsCode <-  jd03013$Konsumgrp
    jd03013$Konsumgrp <- NULL ;
    jd03013
    
}

massage05327 <- function(){
    load(file=paste('Rd_data/jd_','05327','.Rdata',sep=''))
    jd05327$ContentsCode <- NULL
    jd05327$ContentsCode <-  jd05327$Konsumgrp
    jd05327$Konsumgrp <- NULL ;
    jd05327
    
}


massage01302 <- function(){
    load(file=paste('Rd_data/jd_','01302','.Rdata',sep=''))
    jd01302$ContentsCode <- NULL
    jd01302$ContentsCode <-  jd01302$Alder
    jd01302$Alder <- NULL ;
    jd01302
    
}

massage01313 <- function(){
    load(file=paste('Rd_data/jd_','01313','.Rdata',sep=''))
    jd01313$ContentsCode <- NULL
    jd01313$ContentsCode <-  jd01313$Alder
    jd01313$Alder <- NULL ;
    jd01313
    
}

massage03375 <- function(){
    load(file=paste('Rd_data/jd_','03375','.Rdata',sep=''))
    jd03375$ContentsCode <- NULL
    jd03375$ContentsCode <-  jd03375$Alder
    jd03375$Alder <- NULL ;
    jd03375
    
}

massage07268 <- function(){
    load(file=paste('Rd_data/jd_','07268','.Rdata',sep=''))
    jd07268$ContentsCode <- NULL
    jd07268$ContentsCode <-  jd07268$Alder
    jd07268$Alder <- NULL ;
    jd07268
    
}



castOnContentsCode12558 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region","InntektSkatt"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid+InntektSkatt~ContentsCode)
    cD

}

castOnContentsCode07984 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region","Kjonn"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid+Kjonn~ContentsCode)
    cD

}

castOnContentsCode03013 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode"))
    mY$eV <- NULL ;
    cD <- cast(mY,Tid~ContentsCode)
    cD

}


castOnContentsCode05327 <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode"))
    mY$eV <- NULL ;
    cD <- cast(mY,Tid~ContentsCode)
    cD

}

castOnContentsCodeKjonn <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region","Kjonn"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid+Kjonn~ContentsCode)
    cD

}

castOnContentsCode <- function(dS) {
    dS$eV <- dS$value ; dS$value <- NULL ;
    mY <- melt(dS,id=c("Tid","ContentsCode","Region"))
    mY$eV <- NULL ;
    cD <- cast(mY,Region+Tid~ContentsCode)
    cD

}


dsJoinTid <- function(ds1,ds2) {

    ds2$T2 <- ds2$Tid ; ds2$Tid <- NULL ; 
    ds3 <- sqldf("SELECT d1.*, d2.* FROM ds1 d1 LEFT JOIN ds2 d2 ON d1.Tid = d2.T2 ")
    ds3$T2 <- NULL ;
    ds3

}    


dsJoinRegionTid <- function(ds1,ds2) {

    ds2$T2 <- ds2$Tid ; ds2$Tid <- NULL ;  ds2$R2 <- ds2$Region ; ds2$Region <- NULL ;
    ds3 <- sqldf("SELECT d1.*, d2.* FROM ds1 d1 LEFT JOIN ds2 d2 ON d1.Region = d2.R2 AND d1.Tid = d2.T2 WHERE d1.Region>'0'")
    ds3$T2 <- NULL ; ds3$R2 <- NULL ;
    ds3

}    



#    readAndSaveMeta(tstTabs)
#    createAndSaveSearchMarkup(tstTabs)
#    readAndSaveMeta(tstTabs2)
#    createAndSaveSearchMarkup(tstTabs2)

#    readAndSaveMeta(tstTabs4)
#    createAndSaveSearchMarkup(tstTabs4)



#md12660=load(file=paste('Rd_meta/md_','12660','.Rdata',sep=''))
#md07366=getValuesAndLabels("07366")

#fetchAndSave12660()
#fetchAndSave07366()
#fetchAndSave06462()
#fetchAndSave12558()
#fetchAndSave07984()
#q0 <- fetchAndSave07459()
#q0 <- fetchAndSave07459b()


#q0 <- fetchAndSave06445()
#q0 <- fetchAndSave03013()
#q0 <- fetchAndSave05327()




# Befolkningsframskrivninger
#q0 <- fetchAndSave01302()
#q0 <- fetchAndSave01313()
#q0 <- fetchAndSave03375()
#q0 <- fetchAndSave07268()
#q0 <- fetchAndSave10213()








#jd07366
#load(file=paste('Rd_data/jd_','07366','.Rdata',sep=''))

#jd12660
#load(file=paste('Rd_data/jd_','12660','.Rdata',sep=''))

#jd07366[0:100,]


# load(file=paste('Rd_data/jd_','06462','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07366','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','12660','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','12558','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07984','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07459','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','06445','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','03013','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','05327','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','01302','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','01313','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','03375','.Rdata',sep=''))
# load(file=paste('Rd_data/jd_','07268','.Rdata',sep=''))
                                        # load(file=paste('Rd_data/jd_','10213','.Rdata',sep=''))

fetchElectric <- function() {

    fetchAndSave03014()
    fetchAndSave08307()
    fetchAndSave08313()
    fetchAndSave12824() 

}

mkGraphData <- function(downLoad=0) {

    if (downLoad==1) fetchElectric() ;

    load(file=paste('Rd_data/jd_','03014','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','03014b','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','08313','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))

    
}
    
lagDataSerier <- function(downLoad=0) {

    
    if (downLoad==1) fetchElectric() ;
   
    load(file=paste('Rd_data/jd_','03014','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','03014b','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','08313','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))
    

    # Fig 1
    jd08307Bruttoforbruk <-  jd08307[jd08307$ContentsCode=='Bruttoforbruk',]
    Bruttoforbruk <- jd08307Bruttoforbruk$value 
    ProdTotal <-  jd08307[jd08307$ContentsCode=='ProdTotal',]$value
    Eksport <-  jd08307[jd08307$ContentsCode=='Eksport',]$value
    Import <-  jd08307[jd08307$ContentsCode=='Import',]$value
    NettoImp <- ifelse(Eksport-Import<0,1,0)
    Balanse <- Eksport-Import
    Tid1 <- as.numeric(jd08307Bruttoforbruk$Tid)
    fig1Tab<- cbind.data.frame(Tid1,cbind(ProdTotal,Bruttoforbruk,Eksport,Import,Balanse)/1000,NettoImp)

    #Fig 2
    NettoImp79 <- c(fig1Tab[fig1Tab$Tid1>1978,]$NettoImp,0,0) # Ikke import i 2020,2021
    Tid2 <- as.numeric(jd03014$Tid)
    indeksTot <-jd03014$value
    indeksEl <- jd03014b$value
    realPrisEl <- jd03014b$value/jd03014$value*100
    fig2Tab <- cbind(Tid2,indeksTot,indeksEl,realPrisEl,NettoImp79)

    #Fig 3
    jd12824b <- jd12824[jd12824$Tid>'2018M11',]
    jd12824bVind <- jd12824b[jd12824b$Produk2=='01.03',]
    Tid3 <- jd12824bVind$Tid
    Vind <- as.numeric(jd12824bVind$value) 
    Varme <- as.numeric( jd12824b[jd12824b$Produk2=='01.02',]$value )
    Vann <-  as.numeric(jd12824b[jd12824b$Produk2=='01.01',]$value)
    Eksport <- as.numeric( jd12824b[jd12824b$Produk2=='03',]$value)
    Import <-  as.numeric(jd12824b[jd12824b$Produk2=='02',]$value)
    Balanse <- Eksport-Import
    fig3Tab <- cbind.data.frame(Tid3,cbind(Vann,Vind,Varme,Eksport,Import,Balanse)/1000000)
    list(fig1=fig1Tab,fig2=fig2Tab,fig3=fig3Tab)

}


lagEksportFiler <- function(downLoad=0) {

    dFrm <- lagDataSerier(downLoad=downLoad) ;
    write.csv(dFrm$fig1, "kraft_fig1.csv") ;
    write.csv(dFrm$fig2, "kraft_fig2.csv") ;
    write.csv(dFrm$fig3, "kraft_fig3.csv") ;


}
