# Test session RPostgreSQL 20220206
#
#

library("RPostgreSQL")
# 
pg <- dbDriver("PostgreSQL")
#
con <- dbConnect(pg, dbname = "r_db1",host = "172.17.0.1", port = 5432,
                 user = "postgres", password = "postgres")
#
data(mtcars)
df <- data.frame(carname = rownames(mtcars),mtcars,row.names = NULL)
df$carname <- as.character(df$carname)
#
dbWriteTable(con, "cartable",value = df, append = TRUE, row.names = FALSE)
#
df_postgres <- dbGetQuery(con, "SELECT * from cartable")
#
dbDisconnect(con)
#
dbUnloadDriver(pg)
#
