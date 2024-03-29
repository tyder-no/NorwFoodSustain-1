# source("pension_lifetimes_1.R")
# source("pop_projections_1.R")
# source("electricity_time_series_1.R")
#
#
#
#
#
# source("ssb-json-tests-1.R")



# Current way to save as .pdf or .eps files
#  mgp: Margins to axis label, tick labelse and tickmarks mar: Margins around plot    par(mgp=c(2.3,1,0),mar=c(5,5,5,2)+0.1)
#  mfrow: Array dimensions of multiplot. par(mfrow=c(2,3)) : 2x3 array of plots  
#  plot(type='n') : Just create correct fram, fill in with data afterwards, using points(x,y, ...)
#  cex: Scaling constant  cex.lab: Scaling axis labels 
#  


# Handling of food comp table. Illustrates a number of techniques and problems.
# gsub: Substitute throughout an array
# grep: Regular expressions on an array -grep: Pick those not matching
# lapply(dfDatD, function(x) as.numeric(as.character(x))): Cast all columns to numeric
# cbind.data.frame: Join blocks with text and number columns, keep their data types
# read_excel('~/Downloads/Norwegian_Food_Composition_Table_2021.xlsx',sheet='References'): Using library readxl
# readRDS: dfFCT <- readRDS(paste0('./Rd_data/','FCT_2021_0','_dat','.Rds')) The functional way to read df
# dbWriteTable(con, "FCT2021_dat",value = dfFCT, append = TRUE, row.names = FALSE): Store table as PostgreSQL table
# 
# fct_refs <- read_excel('~/Downloads/Norwegian_Food_Composition_Table_2021.xlsx',sheet='References'): Read reference sheet
# saveRDS(fct_refs,paste0('./Rd_data/','FCT_2021','_reflist','.Rds')): Save reference sheet







adaptFoodCompTable <- function(tblNm='FCT_2021_0') {

    dfFCT <- readRDS(paste0('./Rd_data/',tblNm,'.Rds')) ;
    nmArr <- dfFCT[2,] ;
    # Setup acceptable names
    tabNames <- patchColNames(nmArr) ;
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

