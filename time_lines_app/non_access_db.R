library("DBI")
library("RSQLite")
file=paste0(normalizePath(getwd()),"/my-db2")
# really doesn't like to work on the network drive....
mydb <- dbConnect(RSQLite::SQLite(), dbname="~/Documents/my-db")

mtcars
dbWriteTable(mydb, "mtcars", mtcars)
dbWriteTable(mydb, "iris", iris)
dbListTables(mydb)
dbReadTable(mydb, "mtcars")
dblsReadOnly(mydb)

dbDisconnect(mydb)
dbGetQuery(mydb, 'select * from iris;')
