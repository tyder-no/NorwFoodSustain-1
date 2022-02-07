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
        plot(jd08307e$Tid,jd08307p$value/1000,yaxt='n',type='l',col=4,xlim=c(1960,2020), ylim=c(0,150),lwd=2,xlab='År',ylab='TWh',main='Produksjon')
        axis(2,at=seq(0,150,25))
       legend(1960,142,legend=c('Total kraftproduksjon TWh'),col=c(4),lwd=c(2),lty=c(1))
       
       grid()  

    }

    
    plotImpExp <- function() {
       plot(jd08307e$Tid,jd08307e$value/1000,type='l',col=3,xlim=c(1960,2020),lwd=2,xlab='År',ylab='TWh',main='Balanse')
       points(jd08307i$Tid,jd08307i$value/1000,type='l',col=2,lwd=2,lty=2)
     #  points(jd08307i$Tid,jd08307e$value-jd08307i$value,type='l',col=4,lwd=2,lty=2)
        legend(1960,22,legend=c('Eksport','Import'),col=c(3,2),lwd=c(2,2),lty=c(1,2))
       
       grid()  

    }

    plotRealPrice <- function() {
    
        plot(jd03014a$Tid,jd03014b$value/jd03014a$value*100,col=2,type='l',lwd=2,xlab='År',ylab='Indeks,2015=100',main='Realpris og forbruk')  # Real price
        points(jd08313a$Tid,jd08313a$value/jd08313a[15,]$value*100,col=4,type='l',lwd=2,lty=2) # Consumption
        grid()
       # eB1 <- eBalance[eBalance[1,]>=jd03014a[1,]$Tid]
        eB1 <- eBalance[eBalance[,1]>=1979,]
      
        
        impMark <- ifelse(eB1[,2]<0,jd03014b$value/jd03014a$value*100,NA)
        points(eB1[,1],impMark,col=2,lwd=2)
        legend(1980,140,legend=c('Realpris','Forbruk/innbygger'),col=c(2,4),lwd=c(2,2),lty=c(1,2))
       
        
    }


    cv <- function(x) { sd(x)/mean(x) }

   
    load(file=paste('Rd_data/jd_','08307','.Rdata',sep=''))
    load(file=paste('Rd_data/jd_','08313a','.Rdata',sep='')) 
    load(file=paste('Rd_data/jd_','03014b','.Rdata',sep='')) 
    load(file=paste('Rd_data/jd_','03014a','.Rdata',sep=''))
    
    jd08307i <- jd08307[jd08307$ContentsCode=='Import',]
    jd08307e <- jd08307[jd08307$ContentsCode=='Eksport',]
    jd08307p <- jd08307[jd08307$ContentsCode=='ProdTotal',]

    eBalance <-cbind( as.numeric(jd08307e$Tid), as.numeric(jd08307e$value - jd08307i$value))  

    p1980v <- jd08307p[jd08307p$Tid>1979&jd08307p$Tid<1990,]$value
    p1980t <- jd08307p[jd08307p$Tid>1979&jd08307p$Tid<1990,]$Tid
    
    cv1980 <- cv( p1980)

# 0.09862509
 p2010 <- jd08307p[jd08307p$Tid>2009&jd08307p$Tid<2020,]$value
 cv( p2010)
# 0.06677229

    
    graphics.off()
    X11(width=10,height=10) ; par(mfrow=c(2,1))
    plotProdTot()
    plotImpExp()

    X11(width=10,height=7)
    plotRealPrice()


  # X11(width=10,height=7)
  #  plotProdTot()
  
    
    


}



lifeTimePlots2021 <- function() {

    
    plotLongSer <- function(){

          
         plot(lTid, eLtM00,type='l',ylim=c(0,100),lwd=2,col=4)
         points(lTid, 60+eLtM60,type='l',col=4,lwd=2,lty=2)
    #     points(lTid, 70+eLtM70,type='l',col=4,lwd=2,lty=2)
         points(lTid, 80+eLtM80,type='l',col=4,lwd=2,lty=3)
         points(lTid, eLtF00,type='l',lwd=2,col=2)
         points(lTid, 60+eLtF60,type='l',col=2,lwd=2,lty=2)
     #    points(lTid, 70+eLtF70,type='l',col=2,lwd=2,lty=2)
          points(lTid, 80+eLtF80,type='l',col=2,lwd=2,lty=3)
         grid()
        
    }

    plotShortSer <- function(){

          
         plot(lTids, eLtM00s,type='l',ylim=c(70,100),lwd=2,col=4)
         points(lTids, 67+eLtM67s,type='l',col=4,lwd=2,lty=2)
         points(lTids, 80+eLtM80s,type='l',col=4,lwd=2,lty=3)
         points(lTids, 90+eLtM90s,type='l',col=4,lwd=2,lty=4)
        
         points(lTids, eLtF00s,type='l',lwd=2,col=2)
         points(lTids, 67+eLtF67s,type='l',col=2,lwd=2,lty=2)
        points(lTids, 80+eLtF80s,type='l',col=2,lwd=2,lty=3)
        points(lTids, 90+eLtF90s,type='l',col=2,lwd=2,lty=4)
  
        
         grid()
        
    }


    plotLTrends <- function() {

      trYrs <- c(60,67,70,75,80,85,90,95) ; trM <- numeric(length(trYrs)) ; trF <- numeric(length(trYrs)) ;   
        
      l60Ms <- lm(eLtM60s~lTids) ;  trM[1] <- coef(l60Ms)[2] ;
      l67Ms <- lm(eLtM67s~lTids) ;  trM[2] <- coef(l67Ms)[2] ;
      l70Ms <- lm(eLtM70s~lTids) ;  trM[3] <- coef(l70Ms)[2] ;
      l75Ms <- lm(eLtM75s~lTids) ;  trM[4] <- coef(l75Ms)[2] ;
      l80Ms <- lm(eLtM80s~lTids) ;  trM[5] <- coef(l80Ms)[2] ;
      l85Ms <- lm(eLtM85s~lTids) ;  trM[6] <- coef(l85Ms)[2] ;
      l90Ms <- lm(eLtM90s~lTids) ;  trM[7] <- coef(l90Ms)[2] ;
      l95Ms <- lm(eLtM95s~lTids) ;  trM[8] <- coef(l95Ms)[2] ;

      l60Fs <- lm(eLtF60s~lTids) ;  trF[1] <- coef(l60Fs)[2] ; 
      l67Fs <- lm(eLtF67s~lTids) ;  trF[2] <- coef(l67Fs)[2] ;
      l70Fs <- lm(eLtF70s~lTids) ;  trF[3] <- coef(l70Fs)[2] ;
      l75Fs <- lm(eLtF75s~lTids) ;  trF[4] <- coef(l75Fs)[2] ;
      l80Fs <- lm(eLtF80s~lTids) ;  trF[5] <- coef(l80Fs)[2] ;
      l85Fs <- lm(eLtF85s~lTids) ;  trF[6] <- coef(l85Fs)[2] ;
      l90Fs <- lm(eLtF90s~lTids) ;  trF[7] <- coef(l90Fs)[2] ;
      l95Fs <- lm(eLtF95s~lTids) ;  trF[8] <- coef(l95Fs)[2] ;

      plot(trYrs, 20*trM,ylim=c(0,4),lwd=2,col=4)     
      points(trYrs, 20*trF,lwd=2,col=2)     
      points(trYrs, 20*trF,type='l',lwd=1,col=2,lty=2)     
      points(trYrs, 20*trM,type='l',lwd=1,col=4,lty=2)     
   

        
    }

    plotExpect200 <- function(){

        dF00 <- diff20(eCF00) ;  dM00 <- diff20(eCM00) ;
        dF67 <- diff20(eCF67) ;  dM67 <- diff20(eCM67) ;
        dF90 <- diff20(eCF90) ;  dM90 <- diff20(eCM90) ;
        dM80 <- diff20(eCM80) ;  dF80 <- diff20(eCF80) ;
        
        plot(1920:2100,dM67,type='l',col=4,lwd=3,ylim=c(0,4),xlim=c(1920,2100),xlab='Fødselsår',ylab='20 års endring')
        points(1920:2100,dF67,type='l',col=2,lwd=3)
       # points(1920:2100,dF90,type='l',col=2,lty=2,lwd=2)
       # points(1920:2100,dM90,type='l',col=4,lty=2,lwd=2)
       # points(1920:2100,dM80,type='l',col=4,lty=3,lwd=2)
       # points(1920:2100,dF80,type='l',col=2,lty=3,lwd=2)
            
         grid()
        
    }

    plotCohort200 <- function(){

       
        
        plot(1900:2100,eCM00,type='l',col=4,lwd=3,ylim=c(50,100),xlim=c(1900,2100),xlab='Fødselsår',ylab='Forventet levealder, kohort')
        points(1900:2100,eCF00,type='l',col=2,lwd=3)
        points(1900:2100,60+eCF60,type='l',col=2,lwd=2,lty=2)
        points(1900:2100,60+eCM60,type='l',col=4,lwd=2,lty=2)
        points(1900:2100,80+eCF80,type='l',col=2,lwd=2,lty=2)
        points(1900:2100,80+eCM80,type='l',col=4,lwd=2,lty=2)
        points(1900:2100,90+eCF90,type='l',col=2,lwd=2,lty=3)
        points(1900:2100,90+eCM90,type='l',col=4,lwd=2,lty=3)
 
        
       # points(1920:2100,dF90,type='l',col=2,lty=2,lwd=2)
       # points(1920:2100,dM90,type='l',col=4,lty=2,lwd=2)
       # points(1920:2100,dM80,type='l',col=4,lty=3,lwd=2)
       # points(1920:2100,dF80,type='l',col=2,lty=3,lwd=2)
            
         grid()
        
    }
    
    diff20 <- function(x) {
      xs <- numeric(20) ;  xe <- numeric(20) ; lx <- length(x) ;
      x1 <- c(x,xe) ; x2 <- c(xs,x) ;
      y <- x1-x2

     f21 <- rep(1/21,21)
     y_sym <- filter(y[21:lx], f21, sides=2)
      
      y[21:lx]
     }     

    
    
    lTid <- seq(1850,2020,by=5)
    lTids <- seq(1998,2020,by=1)
    
    load(file=paste('Rd_data/jd_','05862','.Rdata',sep=''))  
    load(file=paste('Rd_data/jd_','05375','.Rdata',sep=''))  
    load(file=paste('Rd_data/jd_','12889','.Rdata',sep=''))

    jd12889M <- jd12889[jd12889$Kjonn==1,]
    jd12889F <- jd12889[jd12889$Kjonn==2,]

    eCM00 <- jd12889M[jd12889M$Alder=='000',]$value
    eCF00 <- jd12889F[jd12889F$Alder=='000',]$value
    eCM60 <- jd12889M[jd12889M$Alder=='060',]$value
    eCF60 <- jd12889F[jd12889F$Alder=='060',]$value
    eCM67 <- jd12889M[jd12889M$Alder=='067',]$value
    eCF67 <- jd12889F[jd12889F$Alder=='067',]$value
    eCM80 <- jd12889M[jd12889M$Alder=='080',]$value
    eCF80 <- jd12889F[jd12889F$Alder=='080',]$value
    eCM90 <- jd12889M[jd12889M$Alder=='090',]$value
    eCF90 <- jd12889F[jd12889F$Alder=='090',]$value
  

    
    jd05862M <- jd05862[jd05862$Kjonn==1&jd05862$Tid>'1841-1850',]
    jd05862F <- jd05862[jd05862$Kjonn==2&jd05862$Tid>'1841-1850',]
    
    eLtM00 <- jd05862M[jd05862M$Alder2=='00',]$value
    eLtM60 <- jd05862M[jd05862M$Alder2==60,]$value
    eLtM70 <- jd05862M[jd05862M$Alder2==70,]$value
    eLtM80 <- jd05862M[jd05862M$Alder2==80,]$value
    eLtF00 <- jd05862F[jd05862F$Alder2=='00',]$value
    eLtF60 <- jd05862F[jd05862F$Alder2==60,]$value
    eLtF70 <- jd05862F[jd05862F$Alder2==70,]$value
    eLtF80 <- jd05862F[jd05862F$Alder2==80,]$value

    jd05375M <- jd05375[jd05375$Kjonn==1&jd05375$Tid>1997,]
    jd05375F <- jd05375[jd05375$Kjonn==2&jd05375$Tid>1997,]

    eLtM00s <- jd05375M[jd05375M$Alder=='000',]$value
    eLtM60s <- jd05375M[jd05375M$Alder=='060',]$value
    eLtM67s <- jd05375M[jd05375M$Alder=='067',]$value
    eLtM70s <- jd05375M[jd05375M$Alder=='070',]$value
    eLtM75s <- jd05375M[jd05375M$Alder=='075',]$value
    eLtM80s <- jd05375M[jd05375M$Alder=='080',]$value
    eLtM85s <- jd05375M[jd05375M$Alder=='085',]$value
    eLtM90s <- jd05375M[jd05375M$Alder=='090',]$value
    eLtM95s <- jd05375M[jd05375M$Alder=='095',]$value

    eLtF00s <- jd05375F[jd05375F$Alder=='000',]$value
    eLtF60s <- jd05375F[jd05375F$Alder=='060',]$value
    eLtF67s <- jd05375F[jd05375F$Alder=='067',]$value
    eLtF70s <- jd05375F[jd05375F$Alder=='070',]$value
    eLtF75s <- jd05375F[jd05375F$Alder=='075',]$value
    eLtF80s <- jd05375F[jd05375F$Alder=='080',]$value
    eLtF85s <- jd05375F[jd05375F$Alder=='085',]$value
    eLtF90s <- jd05375F[jd05375F$Alder=='090',]$value
    eLtF95s <- jd05375F[jd05375F$Alder=='095',]$value


    
    graphics.off()
    
    X11()
    plotLongSer() 

    X11()
    plotShortSer()
    
    X11()
    plotLTrends() 

    X11()
    plotExpect200() 

    X11()
    plotCohort200()
    
    
}
