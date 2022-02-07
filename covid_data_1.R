#  source("covid_data_1.R")
#
#
#
#
ageGrp <- c(0,10,20,30,40,50,60,70,80,90)
nInf2151 <- c(49804,85073,60771,60180,56413,37838,18738,9280,3552,1136)
nInfS2151 <- c(49208,187515,221731,232176,237455,200402,100976,46641,26766,11709)
nDead2151 <-c(1,2,3,6,17,57,139,312,440,328)
nDeadS2151 <-c(3,14,21,51,129,398,1091,3443,6209,3946)

dRate2151 <- nDead2151/nInf2151

# Source Reindriftsforvaltningen 2019
 swtRakk <- c(32.5, 33.0, 32.4, 34.2, 35.6, 36.3, 35.8, 37.3, 34.9) # Weight simler > 2yrs
 cwtRakk <- c(22.2, 22.1, 21.1, 22.0, 20.7, 23.3, 23.7, 20.2, 21.7) # Weight calves
 yrsRakk <-2011:2019
 typRakk <-c(1,1,1,0,2,2,2,2,2,3)  # Before,construction,running,last year

nRakk <- c(3404,3974,3829,3755,3707,3930,3717,3855,3743,3686) # Number of animals at spring



dRateEst <- function(nDead=nDead2151,nInf=nInf2151) {

   dRate = nDead/nInf    
   #X11()
   plot(ageGrp,log(dRate))
   summary(dR0 <- lm(log(dRate)~ageGrp))

   c( coef(dR0)[1], coef(dR0)[2])
}



hRateEst <- function(cf=c(3.9e-5,0.117),iV=nInf2151,aV=ageGrp) {
 
            
         iV*(cf[1]*exp(cf[2]*ageGrp))

    }
   
testRakk <- function() {

    cwtRakk1 <- cwtRakk[typRakk==1]
    cwtRakk2 <- cwtRakk[typRakk==2]
    cres <- t.test(cwtRakk1,cwtRakk2)

    swtRakk1 <- swtRakk[typRakk==1]
    swtRakk2 <- swtRakk[typRakk==2]
    sres <- t.test(swtRakk1,swtRakk2)

    nRakk1 <- nRakk[typRakk==1]
    nRakk2 <- nRakk[typRakk==2]
    nres <- t.test(nRakk1,nRakk2)

    graphics.off()
    
    #X11(width=10,height=10)
    plot(yrsRakk,swtRakk,ylim=c(20,40),col=2,xlab='År',ylab='Slaktevekt, kg',pch=16,cex=2.1,cex.lab=1.5)
    
    points(yrsRakk,cwtRakk,col=4,pch=16,cex=2.1)
    points(yrsRakk,cwtRakk,col=4,type='l',lty=2)
    points(yrsRakk,swtRakk,col=2,type='l',lty=2)
    abline(v=2014)
    grid()
    legend(2011,40,legend=c('Simler >2år','Kalver'),col=c(2,4),pch=c(16,16),cex=2.1)

    #dev.copy2eps(device=x11,file='berlevaag_1.eps') ; 

    
    list(calves=cres,simler=sres,numR=nres)
  

}

