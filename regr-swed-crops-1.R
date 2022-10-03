# source('regr-swed-crops-1.R')
#
#
#
#
#
#

library(sqldf)

#Region	Latitude	City
#10	56.2	Karlskrona 
#20	60.7	Falun 
#9	57.6	Visby 
#21	60.7	Gävle 
#13	56.9	Halmstad 
#23	62.2	Östersund 
#6	57.7	Jönköping 
#8	57.1	Kalmar 
#7	56.8	Växjö 
#25	65.7	Luleå 
#12	55.4	Malmö 
#1	59.3	Stockholm 
#4	59	Nyköping 
#3	59.9	Uppsala 
#17	59.4	Karlstad 
#24	63.9	Umeå 
#22	62.7	Härnösand 
#19	59.6	Västerås 
#14	57.9	Göteborg 
#18	59.1	Örebro 
#5	58.4	Linköping 


# Basic preparation, done 'globally': Read latitude table, crops table, define grain crops
sLat <- read.table('swe-reg.dat',header=T)
sjv06010Df <- readRDS(file=paste0('Rd_data/','sjv06010','.Rds'))
grainCodes <- c('0','1','2','3','4','5')


# getSweGrainsYear: Get Swedish grains for given year
# returns a data frame with
# Uses sjv06010Df, downloaded complete table
# Region Crop Variable ValueType Year Value
# Variable==0, ValueType==0 

getSweGrainsYear <- function(gYr=1967) {
   # Pick year,  correct variable combination and grain yields
   jDf <- sjv06010Df[(sjv06010Df$Year==gYr)&(sjv06010Df$Variable==0) & (sjv06010Df$ValueType==0)& (sjv06010Df$Crop %in% c('0','1','2','3','4','5')),]
   # Select and rename
   jDfR <- as.data.frame(cbind(jDf[,1],jDf[,2],jDf[,5],jDf[,6])) 
   names(jDfR) <- c("Region","Crop","Year","Yield")
   # Return data frame
   jDfR 
    
}

#  getSweGrainsRegionMax: Max yield per region
#  Processes year data frame with grains to pick the largest
#  Returns a data frame with region, year and max crop

getSweGrainsRegionMax <- function(dF) {
    # Purge NAs    
    dFD <- dF[!is.na(dF$Yield),]
    # Use SQL max() to get max, group by to do it on regions
    dF0 <-  sqldf('select dFD.Region, dFD.Year, dFD.Crop, max(dFD.Yield) as maxYield from dFD group by dFD.Region')
    dF0
 }


#  getSweGrainsRegionMax: Max yield per region
#  Processes 2 series of 5-year data frames, using rbind to assemble yer data into one data frame
#  Computes region averages with SQL avg() + group by
#  Left joins latitude data, return list with the two data frames

getSweGrainsRegionsAveMax <- function() {

    # 1967-71 first period 
    # Using lapply, first create a list of year-grains data frames, then using max on that list 
    yRs0 <- c(1967,1968,1969,1970,1971)
    dF0 <- lapply(yRs0,function(yR){getSweGrainsYear(gYr=yR)})
    dFM0 <- lapply(dF0,function(dF){getSweGrainsRegionMax(dF)})

    # 2016-2020 
    yRs1 <- c(2016,2017,2018,2019,2020)
    dF1 <- lapply(yRs1,function(yR){getSweGrainsYear(gYr=yR)})
    dFM1 <- lapply(dF1,function(dF){ getSweGrainsRegionMax(dF)})

    # Combining year data frames by rbind
    l0 <- dFM0[[1]] ;
    for (i in 2:5) l0 <- rbind(l0,dFM0[[i]]) ;
    l1 <- dFM1[[1]] ;
    for (i in 2:5) l1 <- rbind(l1,dFM1[[i]]) ;

    # Using SQL avg()
    yAve10 <- sqldf('select l1.Region, avg(l1.maxYield) as avgMaxYield from l1 group by l1.Region')  
    yAve00 <- sqldf('select l0.Region, avg(l0.maxYield) as avgMaxYield from l0 group by l0.Region')  
    # Left join latitude data
    yAve0 <- sqldf('select yAve00.*, sLat.Latitude from yAve00 left join sLat on (yAve00.Region=sLat.region)')
    yAve1 <- sqldf('select yAve10.*, sLat.Latitude from yAve10 left join sLat on (yAve10.Region=sLat.region)')
       
 #   list(dFM0=dFM0,dFM1=dFM1,yAve0=yAve0,yAve1=yAve1)
     list(yAve0=yAve0,yAve1=yAve1)
   
}
    


#   plotSwedLatitGrain 
#   Parameters:
#   y0,y1: The regional data to plot
#   xlim,ylim: Plot ranges, latitude and kg/ha yield
#   cCol,mCol,
#

plotSwedLatitGrain <- function(y0,y1,cCol=2,mCol=4,nCol=6,totCol=3,fFact=1e09,xLim=c(54,66),yLim=c(0,8000),cexG=1.5) {
 #> plot(y1$Latitude,y1$avgMaxYield,col=2,pch=16,ylim=c(0,8000))
 #> points(y0$Latitude,y0$avgMaxYield,col=4,pch=16)

    mkPlot <- function(a0,b0,a1,b1,regEq0='y =  41.476  -  0.595 * x',regEq1='y =  41.476  -  0.595 * x') {
        #1
        op <- par(mar=c(6,7,4,2)+0.1)
        plot(y1$Latitude,y1$avgMaxYield,type='n',main='',xlab='',ylab='',xlim=xLim,ylim=yLim,bty='n',xaxt='n',yaxt='n',axes=FALSE,cex.main=cexG,cex.lab=cexG,cex.axis=cexG)
        points(y1$Latitude,y1$avgMaxYield,pch=19, col=cCol, cex=1.5*cexG)
        points(y0$Latitude,y0$avgMaxYield,pch=19, col=mCol, cex=1.5*cexG)
        
        title(ylab='Grain yield per hectar, kilo',cex.lab=cexG,line=4.5)
        title(xlab='Latitude, degrees',cex.lab=cexG,line=4.5)
        abline(a1,b1,col=cCol)
        abline(a0,b0,col=mCol)
        
        text(60,461,regEq0,cex=cexG)
        text(60,1,regEq1,cex=cexG)

        axis(side=1,at=seq(xLim[1],xLim[2],1),lwd=2,mgp=c(0.5*cexG,1.1*cexG,0),cex.axis=cexG)
                                        # rug(x = seq(1865,2015,10), ticksize = -0.01, side = 1,cex.axis=cexG)
        
        axis(side=2,at=seq(yLim[1],yLim[2],1000),lwd=2,mgp=c(1*cexG,1.3*cexG,1),cex.axis=cexG)
        par(op)
        
    }

    greg1 <- lm(y1$avgMaxYield ~ y1$Latitude)
    greg0 <- lm(y0$avgMaxYield ~ y0$Latitude)

    regEq0 <- paste('y0 = ',round(greg0$coef[1],0),' - ',round(-greg0$coef[2],0),'* x,   r^2 =',round(summary(greg0)$adj.r.squared,2))
    print(regEq0)
 
    regEq1 <- paste('y1 = ',round(greg1$coef[1],0),' - ',round(-greg1$coef[2],0),'* x,   r^2 =',round(summary(greg1)$adj.r.squared,2))
    print(regEq1)
    
    graphics.off()
    X11(width=10,height=8)
    mkPlot(greg0$coef[1],greg0$coef[2],greg1$coef[1],greg1$coef[2],regEq0=regEq0,regEq1=regEq1)

  # EPS graphics
    setEPS()
    postscript('yield_latitude_sweden.eps',width=11,height=8)
    mkPlot(greg0$coef[1],greg0$coef[2],greg1$coef[1],greg1$coef[2],regEq0=regEq0,regEq1=regEq1)
    dev.off()

 # SVG  
    svg('yield_latitude_sweden.svg',width=11,height=8) ;
    mkPlot(greg0$coef[1],greg0$coef[2],greg1$coef[1],greg1$coef[2],regEq0=regEq0,regEq1=regEq1)
    dev.off() ;

 # png  
    png('yield_latitude_sweden.png',width=11,height=8) ;
    mkPlot(greg0$coef[1],greg0$coef[2],greg1$coef[1],greg1$coef[2],regEq0=regEq0,regEq1=regEq1)
    dev.off() ;
    
    
}

# readAndProcessGrainData - The process of preparing and plotting
#
#

readAndProcessGrainData <- function() {

    regT <- getSweGrainsRegionsAveMax()
    y0 <- regT$yAve0
    y1 <- regT$yAve1

    plotSwedLatitGrain(y0,y1)
     

}    

# jDf <- readRDS(file=paste0('Rd_data/','sjv06010','.Rds'))
# SweWW <- jDf[jDf$Region=='0'&jDf$Crop=='0'&jDf$Variable=='0',]
# plot(SweWW$Year,SweWW$Value,type='l')
# regSweWW <- jDf[jDf$Crop=='0'&jDf$Variable=='0',]
# regSweWW67 <- regSweWW[regSweWW$Year==1967,] 



















