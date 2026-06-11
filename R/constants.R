# dropbox
dropboxpath <- "/inventory"

### temporary dirs
td <- file.path(paste0(tempdir(), "/Aterfyndet"))
dir.create(td, showWarnings = FALSE)
td2 <- file.path(paste0(tempdir(), "/Aterfyndet/Printer"))
dir.create(td2, showWarnings = FALSE)
td3 <- file.path(paste0(tempdir(), "/Aterfyndet/Ticket"))
dir.create(td3, showWarnings = FALSE)

## UI contants
widthSide <- 250
initialDateFilter <- as.Date(c("2024-12-01", format(Sys.Date(), "%Y-%m-%d")))

### Hash
keysecret <- Sys.getenv("pepper")
