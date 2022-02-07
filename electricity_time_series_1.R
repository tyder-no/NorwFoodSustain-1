# source("pension_lifetimes_1.R")
# source("pop_projections_1.R")
# source("electricity_time_series_1.R")
#
#
#
#
#
# source("ssb-json-tests-1.R")




#f96 <- castOnContentsCodeKjonn(massage01302()) 
#f99 <- castOnContentsCodeKjonn(massage01313()) 
#b2019 <- castOnContentsCodeKjonn(massage07459b())


#f96[100:150,c(1:2,17:19)]
#f96[100:150,c(1:2,20:23)]
#f96[100:150,c(1:2,24:48)]
#f96[100:150,c(1:2,49:70)]
#f96[100:150,c(1:2,71:83)]
#f96[100:150,c(1:2,84:93)]
#f96[100:150,c(1:2,94:103)]


ageGroup11 <- function(ds,prefix='grp') {

    stpts <- c(4,5,10,17,20,24,49,71,84,94)  
    endpts <- c(4,9,16,19,23,48,70,83,93,103)  
    agenms <-c('0','1_5','6_12','13_15','16_19','20_44','45_66','67_79','80_89','90p','Tot')
    colnms <-paste(prefix,'_',agenms,sep='')
    grpds <- ds[,c(1:3)] ; cn0 <-colnames(grpds) ; colnms <- c(cn0,colnms) ; 
    for (i in (1:length(stpts))) {         print(endpts[i])
        if (endpts[i]>stpts[i])         newCol <- rowSums(ds[,stpts[i]:endpts[i]])
        else newCol <- ds[,stpts[i]]
        
        grpds <- cbind(grpds,newCol) 
    }
    newCol <- rowSums(grpds[,4:(3+length(endpts))])
     grpds <- cbind(grpds,newCol)
                                        #assign(paste('f96_','90p',sep=''),rowSums(f96[,94:103]))
    colnames(grpds) <- colnms 
    grpds
}

genderAdd <- function(ds,genderCol=3) {

  mDs <- ds[ds[genderCol]==1,4:14] ;
  fDs <- ds[ds[genderCol]==2,4:14] ;
  rDs <- mDs + fDs ;
  cbind(ds[ds[genderCol]==1,1:2],rDs)

}

#intervs <- c(4,c(5:9),10:16,17:19,20:23,24:48,49:70,71:83,84:93,94:103)

mkfg96 <- function() {
    f96 <- castOnContentsCodeKjonn(massage01302())
    gds <- ageGroup11(f96,prefix='f96')
    ads <- genderAdd(gds)
    ads

}


mkfg99 <- function() {
    f99 <- castOnContentsCodeKjonn(massage01313())
    gds <- ageGroup11(f99,prefix='f99')
    ads <- genderAdd(gds)
    ads

}


mkfg02 <- function() {
    f02 <- castOnContentsCodeKjonn(massage03375())
    gds <- ageGroup11(f02,prefix='f02')
    ads <- genderAdd(gds)
    ads

}

mkfg09 <- function() {
    f09 <- castOnContentsCodeKjonn(massage07268())
    gds <- ageGroup11(f09,prefix='f09')
    ads <- genderAdd(gds)
    ads

}



mkbg19 <- function() {
    b19 <- castOnContentsCodeKjonn(massage07459b())
    gds <- ageGroup11(b19,prefix='b19')
    ads <- genderAdd(gds)
    ads

}


#fg96 <mkfg96()
#fg99 <- mkfg99()
#fg02 <- mkfg02()
#fg09 <- mkfg09()



#bg19 <- mkbg19()


#bg19$Region[bg19$Region=='0'] <- '0A'
#fg96$Region[fg96$Region=='0'] <- '0A'
#fg02$Region[fg02$Region=='0'] <- '0A'
#fg09$Region[fg09$Region=='0'] <- '0A'



# bg19fg96fg02 <- dsJoinRegionTid(bg19fg96,fg02)
# bg19fg96fg02fg09 <- dsJoinRegionTid(bg19fg96fg02,fg09)

# fg96bg19 <- dsJoinRegionTid(fg96,bg19)
# bg19fg96 <- dsJoinRegionTid(bg19,fg96)
# bf221 <- bg19fg96[bg19fg96$Region=='0221',]
#bf0 <- bg19fg96[bg19fg96$Region=='0',]

#bf0221 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='0221',]

#plot(bf0221$Tid,bf0221$b19_Tot,col=3,ylim=c(11000,17000))
#points(bf0221$Tid,bf0221$f96_Tot,col=2)

#X11()
#plot(bf0221$Tid,bf0221$b19_90p,col=3,ylim=c(0,150))
#points(bf0221$Tid,bf0221$f96_90p,col=2)

#X11()
#plot(bf0221$Tid,bf0221$b19_6_12,col=3,ylim=c(0,1500))
#points(bf0221$Tid,bf0221$f96_6_12,col=2)

#bf00 <- bg19fg96[bg19fg96$Region=='0A',]
#bf01 <- bg19fg96[bg19fg96$Region=='01',]
#bf02 <- bg19fg96[bg19fg96$Region=='02',]

#bf03 <- bg19fg96[bg19fg96$Region=='03',]

#bf000 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='0A',]
#bf001 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='01',]
#bf002 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='02',]

#bf003 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='03',]
#bf014 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='14',]
#bf015 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='15',]
#bf016 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='16',]

#bf017 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='17',]
#bf018 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='18',]

#bf019 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='19',]
#bf020 <- bg19fg96fg02fg09[bg19fg96fg02fg09$Region=='20',]





#bfpanel(bf0221,mainTit='Aurskog-Høland, alle',plotFnm='fpanel_ah.png')

#bfpanel(bf000,ylim1=c(4000000,5500000),ylim2=c(200000,600000),ylim3=c(40000,70000),ylim4=c(5000,45000))
#bfpanel(bf002,ylim1=c(400000,650000),ylim2=c(20000,60000),ylim3=c(4000,7000),ylim4=c(500,4500))
#bfpanel(bf001,ylim1=c(200000,300000),ylim2=c(8000,26000),ylim3=c(1500,3500),ylim4=c(500,2500))
#bfpanel(bf003,ylim1=c(400000,700000),ylim2=c(20000,60000),ylim3=c(5000,11000),ylim4=c(1000,6000),mainTit='Oslo, personer',plotFnm='fpanel_oslo.png')
#bfpanel(bf014,ylim1=c(100000,120000),ylim2=c(7000,12000),ylim3=c(1000,1600),ylim4=c(500,1500))
#bfpanel(bf018,ylim1=c(200000,260000),ylim2=c(17000,25000),ylim3=c(1800,3600),ylim4=c(500,2500))
#
# bfpanel(bf003,ylim1=c(400000,700000),ylim2=c(20000,60000),ylim3=c(5000,11000),ylim4=c(1000,6000),mainTit='Oslo, alle',plotFnm='fpanel_oslo.png')
#bfpanel(bf014,ylim1=c(100000,120000),ylim2=c(7000,12000),ylim3=c(1000,1600),ylim4=c(500,1500),mainTit='Sogn og fjordane, alle',plotFnm='fpanel_sof.png')


bfpanel <- function(dfc,ylim1=c(11000,17000),ylim2=c(800,1600),ylim3=c(100,200),ylim4=c(0,150),mainTit='Hele landet, alle',plotFnm='fpanel_no.png') { 

    
    pkernel <- function() {
        par(mfrow=c(2,2))
        plot(dfc$Tid,dfc$b19_Tot,col=3,ylim=ylim1,type='l',lwd=3,xlab='Tid',ylab='Personer',main=mainTit)
        points(dfc$Tid,dfc$f96_Tot,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_Tot,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_Tot,col=2,type='l',lty=3,lwd=2)
        legend(1986,ylim1[2],0.72,legend=c('Faktisk','F1996','F2002','F2009'),col=c(3,2,2,2),lwd=c(3,2,2,2),lty=c(1,1,2,3))


        plot(dfc$Tid,dfc$b19_6_12,col=3,ylim=ylim2,type='l',lwd=3,xlab='Tid',ylab='Personer',main='Barneskolealder 6-12 år')
        points(dfc$Tid,dfc$f96_6_12,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_6_12,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_6_12,col=2,type='l',lty=3,lwd=2)

        plot(dfc$Tid,dfc$b19_0,col=3,ylim=ylim3,type='l',lwd=3,xlab='Tid',ylab='Personer',main='Fødte')
        points(dfc$Tid,dfc$f96_0,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_0,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_0,col=2,type='l',lty=3,lwd=2)

        plot(dfc$Tid,dfc$b19_90p,col=3,ylim=ylim4,type='l',lwd=3,xlab='Tid',ylab='Personer',main='90 år og over')
        points(dfc$Tid,dfc$f96_90p,col=2,type='l',lwd=2)
        points(dfc$Tid,dfc$f02_90p,col=2,type='l',lty=2,lwd=2)
        points(dfc$Tid,dfc$f09_90p,col=2,type='l',lty=3,lwd=2)
    }
    graphics.off() 
    X11(width=12,height=10)
    pkernel()

    png(plotFnm,width = 1400, height = 1000)
    par(mfrow=c(2,2))
    pkernel()
    dev.off()
    
}


#load(file='Rd_data/bg19fg96fg02fg09.Rdata')
#attach(bg19fg96fg02fg09)

#bbrd19 <- (b19_0+b19_1_5+b19_6_12+b19_13_15+b19_16_19+b19_67_79+b19_80_89+b19_90p)/(b19_20_44+b19_45_66)
#fbrd96 <- (f96_0+f96_1_5+f96_6_12+f96_13_15+f96_16_19+f96_67_79+f96_80_89+f96_90p)/(f96_20_44+f96_45_66)
#fbrd02 <- (f02_0+f02_1_5+f02_6_12+f02_13_15+f02_16_19+f02_67_79+f02_80_89+f02_90p)/(f02_20_44+f02_45_66)
#fbrd09 <- (f09_0+f09_1_5+f09_6_12+f09_13_15+f09_16_19+f09_67_79+f09_80_89+f09_90p)/(f09_20_44+f09_45_66)


pltbrdn <- function() {
  
    plot(Tid[Region=='0A'],bbrd19[Region=='0A'],ylim=c(0.6,0.72),col=3,type='l',lwd=4,xlab='År',ylab='Byrde',main='Forsørgelsesbyrde (= Utenfor/Inne i arbeidsstyrken)')
    points(Tid[Region=='0A'],fbrd96[Region=='0A'],col=2,type='l',lwd=2)
    points(Tid[Region=='0A'],fbrd02[Region=='0A'],col=2,type='l',lwd=2,lty=2)
    points(Tid[Region=='0A'],fbrd09[Region=='0A'],col=2,type='l',lwd=2,lty=3)
    legend(2005,0.72,legend=c('Faktisk','F1996','F2002','F2009'),col=c(3,2,2,2),lwd=c(4,2,2,2),lty=c(1,1,2,3))
}

pltbrdndeviation <- function() {
    devIndex <- function(fb) { (fb/bb19 -1)*100 }
    
    
    Td <- Tid[Region=='0A'] ; bb19 <- bbrd19[Region=='0A'] ;
    fb96 <- fbrd96[Region=='0A'] ; fb02 <- fbrd02[Region=='0A'] ; fb09 <- fbrd09[Region=='0A'] ;
    plot(Td,devIndex(fb96),ylim=c(-2,5),xlim=c(1996,2019),col=2,type='l',lwd=3,xlab='År',ylab='Avviksindeks',main='Indeks: Prosentvis feil forsørgelsesbyrde')
    points(Td,devIndex(fb02),col=2,type='l',lwd=3,lty=2)
    points(Td,devIndex(fb09),col=2,type='l',lwd=3,lty=3)
    abline(a=0,b=0)
    legend(1996,5,legend=c('F1996','F2002','F2009'),col=c(2,2,2),lwd=c(3,3,3),lty=c(1,2,3))
}

pltboth <- function() {

    X11(width=14,height=10)
    par(mfrow=c(1,2))
    pltbrdn()
    pltbrdndeviation()
    
    png('fbyrde.png',width = 1400, height = 1000)
    par(mfrow=c(1,2))
    pltbrdn()
    pltbrdndeviation()
    dev.off()
    
}


elFetct2021 <- function() {

    fetchAndSave08307()
    fetchAndSave08313a()
    fetchAndSave03014b()
    fetchAndSave03014a() 
 

}


elPlots2021 <- function() {



    plotProdTot <- function() {
       plot(jd08307p$Tid,jd08307pV/1000,yaxt='n',type='l',col=4,xlim=c(1960,2020), ylim=c(0,150),lwd=3,xlab='År',ylab='TWh',main='Produksjon og forbruk')
       axis(2,at=seq(0,150,25))
       points(jd08307e$Tid,jd08307cV/1000,type='l',col=2,lwd=3,lty=2) 
       legend(1960,142,bg='white',legend=c('Total kraftproduksjon','Forbruk'),col=c(4,2),lwd=c(3,2),lty=c(1,2))
       
       grid()  

    }

    plotProdWaterWind <- function() {
       plot(jd08307Tid,jd08307pLV/1000,yaxt='n',type='l',col=4,xlim=c(2008,2020), ylim=c(115,155),lwd=4,xlab='År',ylab='TWh',main='Produksjon og forbruk')
       axis(2,at=seq(0,150,25))
   
       points(jd08307Tid,jd08307waLV/1000,type='l',col=3,lwd=3,lty=1)
       points(jd08307Tid,(jd08307waLV+jd08307hLV)/1000,type='l',col=5,lwd=2,lty=1)
       points(jd08307Tid,jd08307cLV/1000,type='l',col=2,lwd=5,lty=1)
       
       legend(2008,155,bg='white',legend=c('Vann+Vind+Varme','Vann+Varme','Vannkraft','Forbruk'),col=c(4,5,3,2),lwd=c(4,2,3,5),lty=c(1,1,1,1))
       
       grid()  

    }
 
    
    plotImpExp <- function() {
       plot(jd08307e$Tid,jd08307eV/1000,type='l',col=3,lty=2,xlim=c(1960,2020),ylim=c(-10,23),lwd=2,xlab='År',ylab='TWh',main='Balanse')
       points(jd08307i$Tid,jd08307iV/1000,type='l',col=2,lwd=2,lty=2)
       points(jd08307i$Tid,(jd08307eV-jd08307iV)/1000,type='l',col=1,lwd=3,lty=1)
       
     #  points(jd08307i$Tid,jd08307e$value-jd08307i$value,type='l',col=4,lwd=2,lty=2)
        legend(1960,22,bg='white',legend=c('Eksport','Import','Balanse, årlig'),col=c(3,2,1),lwd=c(2,2,3),lty=c(2,2,1))
       abline(0,0)
       grid()  

    }

    barPlotBalance <- function() {
     #   a <- barplot(eBalance,col= ifelse(eBalance < 0,"red","blue"), xaxt = "n", yaxt = "n")
                                        #a <- barplot(eBalance)
        par(mgp=c(2.3,0.7,0),mar=c(5,5,5,2)+0.1) 
        eB <- c((jd08307eV-jd08307iV)/1000,20,17.5)
         a <-    barplot(eB1,names=kpiTid,col= ifelse(eB1 < 0,"red","blue"),ylab="TWh",cex.lab=1.5,cex.axis=1.2,cex=1.3 ) 
         
    }

    
    plotRealPrice <- function() {

        par(mgp=c(2.3,0.7,0),mar=c(5,5,5,2)+0.1)
        plot(jd03014a$Tid,jd03014b$value/jd03014a$value*100,col=2,type='n',lwd=3,xlab='År',ylab='Indeks,2015=100',main='',cex.axis=1.3,cex.lab=1.7)  # Real price
        grid()
        abline(100,0,lty=2)
        points(jd03014a$Tid,jd03014b$value/jd03014a$value*100,col=2,type='l',lwd=4,)  # Real price
        points(jd08313a$Tid,jd08313a$value/jd08313a[15,]$value*100,col=4,type='l',lwd=4,lty=3) # Consumption
    
       # eB1 <- eBalance[eBalance[1,]>=jd03014a[1,]$Tid]
        eB1 <- eBalance[eBalance[,1]>=1979,]
      
        
        impMark <- ifelse(eB1[,2]<0,jd03014b$value/jd03014a$value*100,NA)
        points(eB1[,1],impMark,col=2,lwd=3,cex=2.2)
   
        legend(1980,170,bg='white',legend=c('Realpris','Forbruk/innbygger'),col=c(2,4),lwd=c(3,3),lty=c(1,3),cex=1.5)
       
        
    }

    plotScatSurplusPrice <- function()  {
        rEDf <- realElDf[realElDf$kpiTid>2004,]
        par(mgp=c(2.3,1,0),mar=c(5,5,5,2)+0.1)
        plot(rEDf$eB1,rEDf$realEl,xlab='Nettoeksport, TWh',ylab='Realpris forbruker, 2015=100',pch=16,col=2,cex=2.6,cex.lab=1.4,type='n')
        text(rEDf$eB1[13:17]-2,rEDf$realEl[13:17],labels=rEDf$kpiTid[13:17],cex=1.5)
        grid()
        points(rEDf$eB1,rEDf$realEl,pch=16,col=2,cex=2.6,cex.lab=1.4)
        rEDf[17,4] <- NA
        rEDf[14,4] <- NA

        xM1 <- lm(rEDf$realEl ~ rEDf$eB1)
        abline(coef(xM1)[1],coef(xM1)[2],lty=2)


    }


    
    cv <- function(x) { sd(x)/mean(x) }

   
    load(file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','08313a','.Rdata',sep='')) 
    load(file=paste('Rd_data/jd_','03014b','.Rdata',sep='')) 
    load(file=paste('Rd_data/jd_','03014a','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))

   
    
    jd08307iV <- jd08307[jd08307$ContentsCode=='Import',]$value
    jd08307eV <- jd08307[jd08307$ContentsCode=='Eksport',]$value
    jd08307pV <- jd08307[jd08307$ContentsCode=='ProdTotal',]$value
    jd08307cV <- jd08307[jd08307$ContentsCode=='Bruttoforbruk',]$value
    jd08307waV <- jd08307[jd08307$ContentsCode=='VannKraft',]$value
    jd08307wiV <- jd08307[jd08307$ContentsCode=='VindKraft',]$value
    jd08307hV <- jd08307[jd08307$ContentsCode=='VarmeKraft',]$value

    jd08307i <- jd08307[jd08307$ContentsCode=='Import',]
    jd08307e <- jd08307[jd08307$ContentsCode=='Eksport',]
    jd08307p <- jd08307[jd08307$ContentsCode=='ProdTotal',]
    jd08307c <- jd08307[jd08307$ContentsCode=='Bruttoforbruk',]
    jd08307wa <- jd08307[jd08307$ContentsCode=='VannKraft',]
    jd08307wi <- jd08307[jd08307$ContentsCode=='VindKraft',]
    jd08307h <- jd08307[jd08307$ContentsCode=='VarmeKraft',]


  # Data for 2020

    watSer <- jd12824[jd12824$Produk2=='01.01',]  
    heatSer <- jd12824[jd12824$Produk2=='01.02',]  
    windSer <- jd12824[jd12824$Produk2=='01.03',]  
    expSer <- jd12824[jd12824$Produk2=='03',]  
    impSer <- jd12824[jd12824$Produk2=='02',]  
    consSer <- jd12824[jd12824$Produk2=='04',]  

    watSer20 <- watSer[watSer$Tid>'2019M12'&watSer$Tid<'2021M01',]$value  ;  watSer20Sum <- sum( watSer20)/1000 ;
    heatSer20 <- heatSer[heatSer$Tid>'2019M12'&heatSer$Tid<'2021M01',]$value  ;  heatSer20Sum <- sum( heatSer20)/1000 ;
    windSer20 <- windSer[windSer$Tid>'2019M12'&windSer$Tid<'2021M01',]$value   ;  windSer20Sum <- sum( windSer20)/1000 ;
    consSer20 <- consSer[consSer$Tid>'2019M12'&consSer$Tid<'2021M01',]$value  ;  consSer20Sum <- sum( consSer20)/1000 ;
    
    
    
    jd08307iL <- jd08307i[jd08307i$Tid>2007,] ; jd08307iLV <- jd08307iL$value ;
    jd08307eL <- jd08307e[jd08307e$Tid>2007,]  ; jd08307eLV <- jd08307eL$value ;
    jd08307pL <- jd08307p[jd08307p$Tid>2007,]  ; jd08307pLV <- c(jd08307pL$value, watSer20Sum+ windSer20Sum+heatSer20Sum) ;
    jd08307cL <- jd08307c[jd08307c$Tid>2007,]   ; jd08307cLV <- c(jd08307cL$value,consSer20Sum) ;
    jd08307waL <- jd08307wa[jd08307wa$Tid>2007,]  ; jd08307waLV <- c(jd08307waL$value, watSer20Sum) ;
    jd08307wiL <- jd08307wi[jd08307wi$Tid>2007,]   ; jd08307wiLV <- c(jd08307wiL$value, windSer20Sum) ;
    jd08307hL <- jd08307h[jd08307h$Tid>2007,]      ; jd08307hLV <- c(jd08307hL$value, heatSer20Sum) ;
    jd08307Tid <- c(jd08307e[jd08307e$Tid>2007,]$Tid,'2020')
 
    
    eBalance <-cbind( as.numeric(jd08307e$Tid), as.numeric(jd08307e$value - jd08307i$value))  

    
    p1980v <- jd08307p[jd08307p$Tid>1979&jd08307p$Tid<1990,]$value
    p1980t <- jd08307p[jd08307p$Tid>1979&jd08307p$Tid<1990,]$Tid
    
    cv1980 <- cv( p1980v)

# 0.09862509
 p2010 <- jd08307p[jd08307p$Tid>2009&jd08307p$Tid<2020,]$value
 cv( p2010)
# 0.06677229

    kpiTid <- jd03014a$Tid
    kpiTot <- jd03014a$value
    kpiEl <- jd03014b$value
    realEl <- kpiEl/kpiTot*100
   # eB1 <- eBalance[eBalance[,1]>=1979,]
    eB1 <- c(eBalance[eBalance[,1]>=1979,2]/1000,20,17.5)
   
    realElDf <- cbind.data.frame(kpiTid,kpiTot,kpiEl,realEl,eB1)
     

    
    graphics.off()
    X11(width=10,height=10) ; par(mfrow=c(2,1))
    plotProdTot()
    plotImpExp()
    dev.copy2eps(device=x11,file='virkel_nor_f1.eps') ; 
    
    X11(width=10,height=7)
    plotRealPrice()
    #dev.copy2eps(device=x11,file='virkel_nor_f2.eps') ; 
    postscript('virkel_nor_f1.eps',width=11,height=8,onefile=TRUE, horizontal=TRUE) ; 
    plotRealPrice() ;
    dev.off() ;

    pdf('virkel_nor_f1.pdf',width=11,height=8) ; 
    plotRealPrice() ;
    dev.off() ;

    
    X11(width=10,height=7)
    plotProdWaterWind()
    dev.copy2eps(device=x11,file='vind_vann_nor_f1.eps') ; 
  
    X11(width=11,height=9)
    plotScatSurplusPrice()
    #dev.copy2eps(device=x11,file='overskudd_pris_f1.eps') ; 

    pdf('overskudd_pris_f1.pdf',width=10,height=8)
    plotScatSurplusPrice()
    dev.off()

    postscript('overskudd_pris_f1.eps',width=10,height=8,onefile=TRUE, horizontal=TRUE)
    plotScatSurplusPrice()
    dev.off()

    svg('overskudd_pris_f1.svg',width=10,height=8)
    plotScatSurplusPrice()
    dev.off() 
    
    png('overskudd_pris_f1.png')
    plotScatSurplusPrice()
    dev.off()
    

    
    rEDf <- realElDf[realElDf$kpiTid>2004,]
    write.csv(realElDf, "overskudd_pris_f1.csv") ;
    write.csv(rEDf, "overskudd_pris_2005-2021.csv") ;


    X11() ;
    barPlotBalance()

    pdf('barplot_overskudd.pdf',width=11,height=8)
    barPlotBalance()
    dev.off()
 
    postscript('barplot_overskudd.eps',width=11,height=8,onefile=TRUE, horizontal=TRUE)
    barPlotBalance()
    dev.off()
    
    
   list(rD1=realElDf,rD2=rEDf)
}



lifeTimePlots2021 <- function() {

    
    plotLongSer <- function(){

          
         plot(lTid, eLtM00,type='l',ylim=c(0,100),lwd=2,col=4)
         points(lTid, 70+eLtM70,type='l',col=4,lwd=2,lty=2)
         points(lTid, 80+eLtM80,type='l',col=4,lwd=2,lty=3)
         points(lTid, eLtF00,type='l',lwd=2,col=2)
         points(lTid, 70+eLtF70,type='l',col=2,lwd=2,lty=2)
         points(lTid, 80+eLtF80,type='l',col=2,lwd=2,lty=3)
         grid()
        
    }


    lTid <- seq(1850,2020,by=5)
    load(file=paste('Rd_data/jd_','05862','.Rdata',sep=''))  
    load(file=paste('Rd_data/jd_','05375','.Rdata',sep=''))  

    jd05862M <- jd05862[jd05862$Kjonn==1&jd05862$Tid>'1841-1850',]
    jd05862F <- jd05862[jd05862$Kjonn==2&jd05862$Tid>'1841-1850',]
    
    eLtM00 <- jd05862M[jd05862M$Alder2=='00',]$value
    eLtM70 <- jd05862M[jd05862M$Alder2==70,]$value
    eLtM80 <- jd05862M[jd05862M$Alder2==80,]$value
    eLtF00 <- jd05862F[jd05862F$Alder2=='00',]$value
    eLtF70 <- jd05862F[jd05862F$Alder2==70,]$value
    eLtF80 <- jd05862F[jd05862F$Alder2==80,]$value


    X11()
    plotLongSer() 


}


plotWindData <- function() {

    load(file=paste('Rd_data/jd_','12824','.Rdata',sep=''))
    jd12824 <- jd12824[jd12824$Tid>'2018M11',] 
    watSer <- jd12824[jd12824$Produk2=='01.01',]  
    heatSer <- jd12824[jd12824$Produk2=='01.02',]  
    windSer <- jd12824[jd12824$Produk2=='01.03',]  
    expSer <- jd12824[jd12824$Produk2=='03',]  
    impSer <- jd12824[jd12824$Produk2=='02',]  
    consSer <- jd12824[jd12824$Produk2=='04',]  

     windSer2 <- jd12824[jd12824$Produk2=='01.03'&jd12824$Tid>'2020M11',] 
    
    whSer <- heatSer$value + watSer$value
    monthSer <- watSer$Tid
    monthSer2 <- windSer2$Tid

    mS <- c("", "2019-1", "", "", "2019-4", "", "","2019-7", "", "", "2019-10", "", "",
            "2020-1", "", "", "2020-4", "", "","2020-7", "", "", "2020-10", "", "",
            "2021-1", "", "", "2021-4", "", "","2021-7", "", "", "2021-10", "")
     
    
  #  graphics.off()
    
  #  X11()
  #  plot(consSer$value,type='l',col=1,lwd=2)
  #  points(whSer,type='l',col=3,lwd=2)
  #  points(whSer+ windSer$value,type='l',col=4,lwd=2)

  #  X11()
  #  plot(impSer$value,type='l',col=1,lwd=2)
  #  points(expSer$value,type='l',col=3,lwd=2)
  #  points(windSer$value,type='l',col=4,lwd=2)  

    X11(width=10,height=10) ; par(mfrow=c(2,1))
  # 1:
    plot((expSer$value-impSer$value)/1e6,type='l',col=1,lwd=2,xlab="",ylab="TWh",main="Balanse, månedlig",xaxt='n')
 #   axis(1, at=1:36,lab=mS,las=1)
    axis(1, at=1:36,lab=mS,las=2)
 #   points(expSer$value,type='l',col=3,lwd=2)
    points(windSer$value/1e6,type='l',col=4,lwd=2)
    abline(0,0)
    legend(0.3,2.8,legend=c('Balanse','Vindkraft'),col=c(1,4),lwd=c(2,2),cex=0.85)
    grid()
    
 # 2:
    barplot(windSer2$value/1e6,names.arg=c("Des", "Jan", "Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov"),ylab="TWh",main="Vindkraft produksjon 2020-21",col=4)
    dev.copy2eps(device=x11,file='virkel_nor_f3.eps') ;



}
