#source("adaptFoodCompTable.R")

# 20220208
#


# Create acceptable identifier names for table, incl unique ref id's

patchColNames <- function(cNm) {

    cNm1 <- gsub(':','U',cNm)
    cNm2 <- gsub(' ','_',cNm1)
    cNm3 <- gsub('-','_',cNm2)
    cNm4 <- gsub('+','',cNm3)
    cNm4 <- gsub('Mono+Di','MonoDi',cNm4)
    cNm4 <- gsub('\\(','',cNm4)
    nmArr5 <- gsub('\\)','',cNm4)
    for (i in 1:116) if (nmArr5[i]=='Ref') nmArr5[i] <- paste0('Ref_',nmArr5[i-1]) ;
    nmArr5
}


adaptFoodCompTable <- function(tblNm='FCT_2021_0') {

    dfFCT <- readRDS(paste0('./Rd_data/',tblNm,'.Rds')) ;
    nmArr <- dfFCT[2,] ;
    # Setup acceptable names
    tabNames <- patchColNames(nmArr) ;
    tabNames[61] <- 'MonoDi' ;
    tabNames[62] <- 'Ref_MonoDi' ;
    
    names(dfFCT) <- tabNames
    # Filter food items lines
    foodItms <- grep("[0-9][0-9]\\.[0-9][0-9][0-9]",dfFCT[,1])
    dfFCT1 <- dfFCT[foodItms,] ;

    # Split in data and reference table

    refCols <- grep('Ref_',names(dfFCT1))
    dfRef <- dfFCT1[,refCols]

    dfDat <- dfFCT1[,-grep('Ref_',names(dfFCT1))]
    # Take ID component
    dfDatID <-dfDat[,1:2]
    # Taka data part and convert to numeric with lapply
    dfDatD <-dfDat[,3:59] 
    dfDatD[] <- lapply(dfDatD, function(x) as.numeric(as.character(x)))
    # Join ID parts on data and refs. cbind.data.frame preserves data types in cbind
    dfDat <- cbind.data.frame(dfDatID,dfDatD)
    dfRef <- cbind.data.frame(dfDatID,dfRef)
    
    # Save
    saveRDS(dfDat,paste0('./Rd_data/',tblNm,'_dat','.Rds')) ;
    saveRDS(dfRef,paste0('./Rd_data/',tblNm,'_ref','.Rds')) ;
    
}

# Reading in and saving to PostgreSQL tables. Now it is in fact correct...
#> dfFCT <- readRDS(paste0('./Rd_data/','FCT_2021_0','_dat','.Rds'))
#> dbWriteTable(con, "FCT2021_dat",value = dfFCT, append = TRUE, row.names = FALSE)
#[1] TRUE
#> dfRef <- readRDS(paste0('./Rd_data/','FCT_2021_0','_ref','.Rds'))
#> dbWriteTable(con, "FCT2021_ref",value = dfRef, append = TRUE, row.names = FALSE)
#[1] TRUE
# Handling the reference sheet
# library("readxl")
# fct_refs <- read_excel('~/Downloads/Norwegian_Food_Composition_Table_2021.xlsx',sheet='References')
# saveRDS(fct_refs,paste0('./Rd_data/','FCT_2021','_reflist','.Rds'))


# sjv06010Df <- readRDS(file=paste0('Rd_data/','sjv06010','.Rds'))
