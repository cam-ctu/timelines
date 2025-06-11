library("DBI")
library("RSQLite")
mydb <- dbConnect(RSQLite::SQLite(), "my-db")


dbWriteTable(mydb, "mtcars", mtcars)
dbWriteTable(mydb, "iris", iris)
dbListTables(mydb)

dblsReadOnly(mydb)

dbDisconnect(mydb)
dbGetQuery(mydb, 'select * from iris;')
