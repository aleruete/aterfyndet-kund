library(DBI)
library(dplyr)
library(stringr)
library(glue)

consql = dbConnect(RSQLite::SQLite(),dbname = 'data/aterfyndet.db')

# clients <- dbGetQuery(conn = conpg, "SELECT * FROM kund")
# stock <- dbGetQuery(conn = conpg, "SELECT * FROM lager")

# dbWriteTable(conn = consql, name = "categories", value = category)
# dbWriteTable(conn = consql, name = "clients", value = clients)
# dbWriteTable(conn = consql, name = "stock", value = stock)
# # dbListTables(conn = consql)

# #> [1] "Tbl_test"
# dbDisconnect(con)
# 
# dir(".")
# #>[1] "test.db"

# clientsclean <- read.csv("data/kundlista_clean.csv", fileEncoding = "UTF-8")

clients <- dbGetQuery(conn = consql, "SELECT * FROM clients")

clients$adress <- str_replace(clients$adress, pattern = clients$ort, "")
clients$adress <- trimws(clients$adress, which = "both")

# dbExecute(consql, glue("UPDATE clients
#                             SET adress = TRIM(REPLACE(adress, ort, ''));"))
