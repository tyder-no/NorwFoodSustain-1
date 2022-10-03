options(encoding="UTF-8")
#
# source("ssb-json-fetch-1.R")
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
#                                        #
# 8,16,17,18,21,22,24,25,27,28,29,30,31,32,33,34,35,36,37,38,42,43,44,45,46,48,51,52,53,54,62,63,64,66,67,69,71,72,73,75,76,77
# 19,20,26,40,41,47,49,50
# 19,20



sbSNoAgr1 <-
"
Jordbruksbedrifter (gardsbruk) og personlege brukarar (gardbrukarar)

05988 Jordbruksbedrifter, etter brukartype (F) 1999 - 2021
05971 Jordbruksbedrifter, etter brukartype og jordbruksareal i drift 1999 - 2021
05972 Jordbruksbedrifter, etter kjønn på referanseperson og brukartype 1999 - 2021
05976 Personlege brukarar, etter kjønn og gjennomsnittsalder (F) 1999 - 2021
05977 Personlege brukarar, etter kjønn, gjennomsnittsalder og jordbruksareal i drift 1999 - 2021
05974 Personlege brukarar, etter alder (F) 1999 - 2021
05975 Personlege brukarar, etter alder og jordbruksareal i drift 1999 - 2020
12659 Jordbruksbedrifter og jordbruksareal, etter driftsform (F) 2010 - 2020

Jordbruksareal

05982 Jordbruksareal, etter bruken (dekar) 1969 - 2021
11506 Jordbruksareal, etter bruken (dekar) (F) 1969 - 2021
06462 Jordbruksareal for utvalde vekstar (dekar) (K) 1969 - 2020
04496 Jordbruksareal i drift (dekar) (F) 1969 - 2021
04500 Jordbruksareal per jordbruksbedrift (dekar) (F) 1969 - 2021
05983 Jordbruksareal, etter bruken og bruksstorleik (dekar) 2000 - 2020
03312 Jordbruksbedrifter (F) 1969 - 2021
11582 Jordbruksbedrifter i alt og jordbruksbedrifter med forskjellige jordbruksvekstar (F) 1969 - 2021
08646 Jordbruksbedrifter i alt og jordbruksbedrifter med utvalde vekstar (K) 1969 - 2020
03313 Jordbruksbedrifter, etter jordbruksareal i drift (F) 1969 - 2021
05981 Jordbruksbedrifter med ymse vekstar og gjennomsnittleg areal, etter jordbruksareal i drift 2000 - 2021
05980 Jordbruksbedrifter, etter vekstar og gjennomsnittleg areal (F) 2000 - 2021
12658 Jordleige (K) 1969 - 2019

Husdyr

11507 Husdyr, etter husdyrslag (F) 1969 - 2021
05984 Husdyr, etter husdyrslag 1969 - 2021
06447 Husdyr, etter utvalde husdyrslag (K) 1969 - 2020
05985 Husdyr per jordbruksbedrift, etter husdyrslag (F) 2000 - 2021
05986 Husdyr per jordbruksbedrift, etter husdyrslag og jordbruksareal i drift 2000 - 2021
12660 Husdyr på utmarksbeite (K) 1995 - 2020

Jordbruksbedrifter med ymse husdyrslag

05979 Jordbruksbedrifter med ymse husdyrslag, etter jordbruksareal i drift 2000 - 2021
11583 Jordbruksbedrifter med forskjellige husdyr (F) 1969 - 2021
06459 Jordbruksbedrifter med utvalde husdyrslag (K) 1969 - 2020

Økologisk jordbruk

12661 Jordbruksbedrifter med økologisk drift, godkjent økologisk jordbruksareal og jordbruksareal under omlegging til økologisk drift (karens) (F) 1995 - 2020
12662 Godkjent økologisk jordbruksareal, etter bruken 1995 - 2020
12663 Økologisk husdyrhald 1999 - 2020
"

# Lists of tables

agrTabs1 <- c("05988","05971","05972","05976","05977","05974","05975","12659")
agrTabs2 <- c("05982","11506","06462","04496","04500","05983","03312","11582","08646","03313","05981","05980","12658")
agrTabs3 <-c("11507","05984","06447","05985","05986","12660")
agrTabs4 <-c("05979","11583","06459")
agrTabs5 <-c("12661","12662","12663")


#https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Skordar/JO0601J01.px

j0601query0 <-
'{
  "query": [
    {
      "code": "Län",
      "selection": {
        "filter": "item",
        "values": [
          "26"
        ]
      }
    },
    {
      "code": "Variabel",
      "selection": {
        "filter": "item",
        "values": [
          "2",
          "3"
        ]
      }
    },
    {
      "code": "Tabelluppgift",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "År",
      "selection": {
        "filter": "item",
        "values": [
          "35",
          "36",
          "37",
          "38",
          "39",
          "40",
          "41",
          "42",
          "43",
          "44",
          "45",
          "46",
          "47",
          "48",
          "49",
          "50",
          "51",
          "52",
          "53",
          "54",
          "55"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'


query2DF <- function(jsQuery){
    fromJSON(jsQuery)
}

df2Query0 <- function(df) {
    toJSON(df)
}

df2Query <- function(df) {
    toJSON(df)
}


basicSelection <-
"
07459:  Alders- og kjønnsfordeling i kommuner, fylker og hele landets befolkning (K) 1986 - 2019
07984 	Sysselsatte, etter bosted, arbeidssted, kjønn, alder og næring (17 grupper, SN2007). 4. kvartal (K) 	2008 - 2018
        -> derivat av de to over? 06445 	Andel sysselsatte i befolkningen, etter bosted, kjønn og alder. 4. kvartal (K) 	2005 - 2018
     
12558 	Desilfordelt inntekt for husholdninger. Høyeste verdi, antall og prosent (K) 	2005 - 2017
06462 	Jordbruksareal, etter bruken (dekar) (K) 	1969 - 2018
12660 	Husdyr på utmarksbeite (K) 	1995 - 2018
07366 	Produktivt skogareal (dekar) (K) 	2008 - 2017
"
 




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


tstTabs1 <- c("07459","07984","12558","06462","12660","07366","03375","03376","05903","05904","06916","06917","07267","07268",
  "08108","08109","08824","08825","09481","09482","10212","10213","11167","11168","11667","11668")

tstTabs2 <- c("06445","03013","05327")
tstTabs3 <- c("01302","01313")     # 1996, 1999-framskrivningene
tstTabs4 <- c("03014","08307","08313","12824") # Electricity data etc


# Using Hadley Wickham's excel read function here...
# library("readxl")
# fct <- read_excel('~/Downloads/Norwegian_Food_Composition_Table_2021.xlsx',sheet='Foods')
# fct_refs <- read_excel('~/Downloads/Norwegian_Food_Composition_Table_2021.xlsx',sheet='References')


# Uses API to get metadata, saves it as R data frames for further processing

readAndSaveMeta <- function(tblList) {
    for (tblNum in tblList) {
        print(tblNum)
        X <- getRMetaDataFrame(tblNum) ;
        saveRDS(X,file=paste('Rd_meta/md_',tblNum,'.Rds',sep=''))
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


fetchMetaData <- function() {

   #readAndSaveMeta(tstTabs1) ;
   readAndSaveMeta(tstTabs2) ;
   readAndSaveMeta(tstTabs3) ;
   readAndSaveMeta(tstTabs4) ;


    }



getBasicQuery <- function() {

    '{
    "query": [],
    "response": {
      "format": "json-stat"
     }
   }' 

}

# Uses API to get metadata for a list of tables, saves it as R data frames for further processing

fetchAndSaveMeta <- function(tblList,prfx='sbSNo') {

    getMDfName <- function(tblNum) {paste0('M_',prfx,tblNum)}
    for (tblNum in tblList) {
        print(tblNum)
        mDf <-  getMDfName(tblNum) ;
        X <- getRMetaDataFrame(tblNum) ;
        saveRDS(X,file=paste('Rd_meta/',mDf,'.Rds',sep=''))
    }
    
    
}

# Uses API to get data, saves it as R data frames for further processing

fetchAndSaveTable <- function(tblId,prfx='sbSNo',jsQuery=NULL){
   # load(file=paste('Rd_meta/sdf_','12824','.Rdata',sep='')) # get mVL
   # mVL$Tid$Slct[1] <- 10
   # mVL$ContentsCode$Slct[1] = 10
   # mVL$Region$Slct[1]=10
   # q12824 <- createSearchFromDF(mVL)

    
   getDfName <- function() {paste0(prfx,tblId)}
    jDf <- getDfName()
    if (is.null(jsQuery))  jsQuery <- getBasicQuery() ; 
    
                                        #assign(jDf,getJSONData(tableId,getBasicQuery()))
   X <- getJSONData(tblId,jsQuery)
   saveRDS(X,file=paste('Rd_data/',jDf,'.Rds',sep=''))
   X
     # load(file=paste('Rd_data/',prfx,tableId,'.Rdata',sep=''))
}

loadTable <- function(tblId,prfx='sbSNo'){
   # load(file=paste('Rd_meta/sdf_','12824','.Rdata',sep='')) # get mVL
   # mVL$Tid$Slct[1] <- 10
   # mVL$ContentsCode$Slct[1] = 10
   # mVL$Region$Slct[1]=10
   # q12824 <- createSearchFromDF(mVL)
   #getDfName <- function() {paste0(prfx,tableId)}
   #jDf <- getDfName() 
   #assign(jDf,getJSONData(tableId,getBasicQuery()))
   #save(list=jDf,file=paste('Rd_data/',jDf,'.Rdata',sep=''))
    dfNm <- paste0(prfx,tblId) ;
    readRDS(file=paste('Rd_data/',dfNm,'.Rds',sep=''))
  
    
}

pgWriteTable <- function(df,tblNm,con=NULL) {

    dbWriteTable(con, tblNm,value = df, append = FALSE, row.names = FALSE)
  
}

loadAndWriteTable <- function(tblId,prfx='sbSNo',con){

  X <- loadTable(tblId,prfx=prfx) ;
  pgWriteTable(X,paste0(prfx,tblId),con=con)  
  X
}
    
# Fetches a list of tables

fetchAndSaveTableList <- function(tblList,prfx='sbSNo') {
   
    for (tblNum in tblList) {
       fetchAndSaveTable(tblNum,prfx=prfx)
    }
    
}

loadTableList <- function(tblList,prfx='sbSNo') {
   
    for (tblNum in tblList) {
       loadTable(tblNum,prfx=prfx)
    }
    
}

fetchAndSaveAgrMeta <- function(prfx='sbSNo') {

    # agrTabLists <- c("agrTabs1","agrTabs2","agrTabs3","agrTabs4","agrTabs5")
    # Concatenate lists into one
    agrTabList <- c(agrTabs1,agrTabs2,agrTabs3,agrTabs4,agrTabs5)
    fetchAndSaveMeta(agrTabList,prfx=prfx)
    
     
}

fetchAndSaveAgrData <- function(prfx='sbSNo') {

    #agrTabLists <- c("agrTabs1","agrTabs2","agrTabs3","agrTabs4","agrTabs5")
    agrTabList <- c(agrTabs1,agrTabs2,agrTabs3,agrTabs4,agrTabs5)
    fetchAndSaveTableList(agrTabList,prfx=prfx)
   
     
}
