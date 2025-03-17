#### Get the latest
library(tidyverse)
# library(DBI)
# library(RSQLite)
# remotes::install_github("karthik/rdrop2")
library(rdrop2)
drop_auth(rdstoken = "./token.rds")

dropboxpath <- "inventory/"

dropfiles <- drop_dir(path = dropboxpath) |> 
  filter(.tag == "file", 
         grepl(".db", name))
nfiles <- nrow(dropfiles)
stopifnot("No .db in folder" = nfiles > 0)

for (j in seq(nfiles)) {
  dest_file <- paste0("data/", dropfiles$name[j])
  drop_download(path = dropfiles$path_display[j],
                local_path = dest_file,
                overwrite = TRUE) #dtoken = token
}  
# consql <- dbConnect(RSQLite::SQLite(), 
#                     dbname = 'data/aterfyndet.db')
# 
# categories <- dbReadTable(consql, "categories")
# stock <- dbReadTable(consql, "stock")

