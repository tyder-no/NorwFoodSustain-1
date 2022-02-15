# Test session RPostgreSQL 20220206
#
# source("PostgresTst.R")

library("RPostgreSQL")
# 
pg <- dbDriver("PostgreSQL")
#
con <- dbConnect(pg, dbname = "r_db1",host = "172.17.0.1", port = 5432,user = "postgres", password = "postgres")
#
#data(mtcars)
#df <- data.frame(carname = rownames(mtcars),mtcars,row.names = NULL)
##df$carname <- as.character(df$carname)
#
#dbWriteTable(con, "cartable",value = df, append = TRUE, row.names = FALSE)
                                        #

#dbWriteTable(con, "sbSNo05982",value = sbSNo05982, append = TRUE, row.names = FALSE)
#dbWriteTable(con, "sbSNo05982",value = sbSNo05982, append = FALSE, row.names = FALSE)


#df_postgres <- dbGetQuery(con, "SELECT * from cartable")
#df_pg_05982 <- dbGetQuery(con, "SELECT * from sbSNo05982  WHERE VekstarDekar='01'") # Segfault!
#df_pg_05982 <- dbGetQuery(con, "SELECT * from sbSNo05982")  # Segfault!


# df_pg_05982 <- dbGetQuery(con, 'SELECT * from "sbSNo05982"')  # Fine
# df_pg_05982 <- dbGetQuery(con, 'SELECT * from "sbSNo05982" WHERE "VekstarDekar"=\'01\'') # Fine
# dbGetQuery(con, 'SELECT value from "sbSNo05982" WHERE "VekstarDekar"=\'01\'') # Fine
# dbGetQuery(con, 'SELECT "Tid",value from "sbSNo05982" WHERE "VekstarDekar"=\'01\'') # Fine

#
#dbDisconnect(con)
#
#UnloadDriver(pg)
#
