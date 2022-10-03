# source("import-csv-pdf-1.R")
#
# routines for handling CSV files ripped from pdf - Start with Excalibur/Camelot
# ripped CSV files
#
#

# 2021
# 8,16,17,18,21,22,24,25,27,28,29,30,31,32,33,34,35,36,37,38,42,43,44,45,46,48,51,52,53,54,62,63,64,66,67,69,71,72,73,75,76,77
# 19,20,26,40,41,47,49,50

# 2019
# 8,16,17,18,22,23,24,25,27,28,29,30,31,32,33,34,35,36,37,38,39,42,43,44,45,46,48,57,58,59,61,62,64,66,67,68,70,71,72,73,78,79,80,81,83,84,85,86,88,90
# 19,20,26,40,41,47,49


# replDecCommas - CSV import often has decimal comma, and when comma is also used as delimiter,
# it may be as easy to import 'as is'
#


replDecCommas <- function(cDf) {

    cDfNm <- names(cDf) ;    nCol <- length(cDfNm) ;
    cDfNm <- gsub('\\.','',cDfNm) ;     # Remove periods
    cDfNm <- gsub('\\\\','',cDfNm) ; # Does not work for removing \n
   # t0 <-lapply(cDfNm,function(x){x <- paste(x,collapse='\n')})     

    
  #  cDf0 <- cDf[,2:nCol] ; # Simple tables with just one text column first
    cDf1 <- cDf[,1] ;
    cDf1 <- gsub('\\\n','',cDf1) ; # Does work for removing \n 
    cDfN0 <- as.data.frame( lapply(cDf[,2:nCol],function(x){as.numeric(gsub(',','.',x))})) # Using lapply to convert column-wise, then cast
    cDfN <- cbind.data.frame(cDf1,cDfN0) ; names(cDfN) <- cDfNm ;

    print(cDfN)
    cDfN
    
}

#
# Handling file lists ls *.csv > ls-csv.txt
# Reading in as a table, processing one at a time
#
#

readProcessTblList <- function(tblDir='~/Documents/test2/',prfx='UNK2019_',fLnm='ls-csv.txt') {

    tblDir <- '~/Documents/test2/' ; fLnm <- 'ls-csv.txt' ;
    fL <- read.table(paste0(tblDir,fLnm))
    
    for (fnm0 in fL$V1) {
        
        fnm <- paste0(tblDir,fnm0) ; sN <- fnm ; # Serial ID
        df0 <- read.csv(fnm) ;
        df1 <- replDecCommas(df0) ;
        sN0 <- sub("(.+)(page-)([0-9]+)(.+)([0-9]+)(.+)","p\\3_\\5",sN,perl=TRUE) # ID for data frame
        saveRDS(df1,file=paste0('Rd_data/',prfx,sN0,'.Rds'))
    
    }
    


}



