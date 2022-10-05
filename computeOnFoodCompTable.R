#source("computeOnFoodCompTable.R")

# 20220208
#
library(sqldf)

# Create acceptable identifier names for table, incl unique ref id's

selectItems <- function(itmList,dfFCT) {

   sdF <- sqldf('select * from dfFCT where dfFCT.FoodId in (select itmList.FoodId from itmList) order by FoodId ') ;

}

getTstItm <- function(nItm=3) {

    tstItm <- as.data.frame(rbind(c("01.247",100),c("03.017",100),c("05.016",100)))
    names(tstItm) <- c("FoodId","Wt")
    tstItm

}    


getSelectItm <- function(dfFCT,sItm) {

    sFCT <- selectItems(sItm,dfFCT)
    sFCT 
}


# nutrComputeSum - Matrix multiplication giving vector of totals for nutrient intake 
# N: Nutrients provided, vector  A: Table item list I: Intake by food item 
# N = t(A) %*% I

nutrComputeSum <- function(dfFCT,itmList) {

    sFCT <- selectItems(itmList,dfFCT)  
    dsFCT <- as.matrix(sFCT[,3:length(names(dfFCT))])
    wItm <- as.vector(as.numeric(itmList$Wt))
    nutrList <- t(dsFCT) %*% wItm
    nutrList

}


# nutrComputeItems - Matrix multiplication giving matrix for nutrient intake 
# N: Nutrients provided, matrix  A: Table item list I: Intake by food item 
# N = t(A * I)

nutrComputeItems <- function(dfFCT,itmList) {

    sFCT <- selectItems(itmList,dfFCT)  
    dsFCT <- as.matrix(sFCT[,3:length(names(dfFCT))])
    wItm <- as.vector(as.numeric(itmList$Wt))
    nutrMtrx <- t(dsFCT * wItm)
    nutrMtrx

}


# nutrReverseCompute 
# N (nutrList): Nutrients provided  A: Table item list I: Intake by food item 
# N = t(A) %*% I  => A %*% N = A %*% t(A) %*% I => I =  Inv(A %*% t(A)) %*% A %*% N

nutrReverseCompute <- function(dfFCT,nutrList,itmList) {

    sFCT <- selectItems(itmList,dfFCT)  
    dsFCT <- as.matrix(sFCT[,3:length(names(dfFCT))])
    dsInv <- solve(dsFCT %*% t(dsFCT))
 #   nItm <- as.vector(as.numeric(nutrList))
    nItm <- as.vector((nutrList))
    
    iItm <- dsInv %*% dsFCT %*% nItm
    iItm

}




tstSQLSum <- function() {

    id <- c("01","02","01","02")
    a <- cbind(c(1,2,3,4),c(0,1,1,0),c(1,0,1,2))
    df <- data.frame(id,a)
   # df <- as.data.frame(t(rbind(id,a)))
    ids <- sqldf('select distinct df.id from df')
    vId <- lapply(ids$id,function(x){sqldf(paste0('select * from df where df.id = "',x,'"'))})
   # s0 <- colSums(subset(vId[[1]],select=-id))
    sId <- lapply(vId,function(x){colSums(subset(x,select=-id))})
    lA <- list() ; lA$id <-  as.vector(ids)
    lA$dat <- as.data.frame(do.call(rbind,sId))







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

dfFCT <- readRDS(paste0('./Rd_data/','FCT_2021_0','_dat','.Rds'))

#nutrComputeSum(dfFCT,getTstItm())
#nutrComputeItems(dfFCT,getTstItm())

# sjv06010Df <- readRDS(file=paste0('Rd_data/','sjv06010','.Rds'))
