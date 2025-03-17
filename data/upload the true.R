library(rdrop2)
dropboxpath <- "inventory/"
# token <- readRDS("./token.rds")
# drop_auth(rdstoken = "./token.rds")

drop_upload("data/aterfyndet.db",
            path = dropboxpath, 
            mode = "overwrite")
