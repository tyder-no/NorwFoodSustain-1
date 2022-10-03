#source("computeOnFoodCompTable.R")

# 20220208
#
library(sqldf)

# Create acceptable identifier names for table, incl unique ref id's


dfFCT <- readRDS(paste0('./Rd_data/','FCT_2021_0','_dat','.Rds'))

selectItems <- function(itmList,dfFCT) {

   sdF <- sqldf('select * from dfFCT where dfFCT.FoodId in (select itmList.FoodId from itmList) order by FoodId ') ;

}

testSelect <- function(dfFCT) {

    tstItm <- as.data.frame(rbind(c("01.247",100),c("03.017",100),c("05.016",100)))
    names(tstItm) <- c("FoodId","Wt")
    sFCT <- selectItems(tstItm,dfFCT)

}


# nutrCompute 
# N: Nutrients rpovided  A: Table item list I: Intake by food item 
# N = t(A) %*% I

nutrCompute <- function(itmList,dfFCT) {

    sFCT <- selectItems(tstItm,dfFCT)  
    dsFCT <- as.matrix(sFCT[,3:length(names(dfFCT))])
    wItm <- as.vector(as.numeric(itmList$Wt))
    nutrList <- t(dsFCT) %*% wItm
    nutrList

}

# nutrReverseCompute 
# N (nutrList): Nutrients provided  A: Table item list I: Intake by food item 
# N = t(A) %*% I  => A %*% N = A %*% t(A) %*% I => I =  Inv(A %*% t(A)) %*% A %*% N

nutrReverseCompute <- function(nutrList,itmList,dfFCT) {

    sFCT <- selectItems(tstItm,dfFCT)  
    dsFCT <- as.matrix(sFCT[,3:length(names(dfFCT))])
    dsInv <- solve(dsFCT %*% t(dsFCT))
 #   nItm <- as.vector(as.numeric(nutrList))
    nItm <- as.vector((nutrList))
    
    iItm <- dsInv %*% dsFCT %*% nItm
    iItm

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
