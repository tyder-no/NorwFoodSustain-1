options(encoding="UTF-8")
#
# source("ssb-json-tests-1.R")
# source("kraft_2021_1.R")
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

source("ssb-json-tests-1.R")



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


plotMagExp2021 <- function() {

    mag172021 <- c(37,47.5,66.1,69.5,66.7,62.9,71.4,65.5,55.9)   # Monthly from April
    mag17Med <- c(31.3,45.3,67.9,74.9,81.1,82.9,81.4,77.3,68.0)  # Median

    dFr <- lagDataSerier()
    monStat <- dFr$fig3 
    apr2021Bal <- c(monStat[29:36,]$Balanse,1.65) ; cumApr2021Bal <- cumsum(apr2021Bal) ;
    
    magKap <- 87.2 ; magMedApr <- mag17Med[1] ; mon17 <- c('Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Des')
    mag2021Rel <- (mag172021[1:9]- magMedApr)/100*magKap ;
    magMedRel  <- (mag17Med[1:9]- magMedApr)/100*magKap ;
    X11(width=10,height=9)
    plot(cumsum(apr2021Bal),ylim=c(0,45),xlab='Måned',ylab='TWh',xaxt="n")
    axis(1,at=1:9,labels=mon17)
    points(cumsum(apr2021Bal),type='l',col=1,lty=2,lwd=4)
    points((mag172021[1:9]- magMedApr)/100*magKap,type='l',col=2,lwd=3,lty=2)
    points((mag17Med[1:9]- magMedApr)/100*magKap,type='l',col=4,lwd=3,lty=2)
    grid()
    legend(1,45,legend=c('Kumulativ nettoeksport','Fylling 2021, rel','Median fylling, rel'),lty=c(2,2,2),lwd=c(4,3,3),col=c(1,2,4))
    dev.copy2eps(device=x11,file='magasin_eksport_2021_1.eps') ;

    
    tmpF <- cbind.data.frame(mon17,apr2021Bal,  mag172021 ,mag17Med, cumApr2021Bal,  mag2021Rel, magMedRel)
    list(plData=tmpF)
    write.csv(tmpF, "magasin_fig1.csv") ;
}
