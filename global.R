v <- numeric_version("0.5.0")
production <- TRUE
# sudo su - -c "R -e \"install.packages('any_package', repos='https://cran.rstudio.com/')\""
library(shiny)
conflicted::conflict_prefer_all("DT", "shiny", quiet = TRUE)
suppressPackageStartupMessages(library(bs4Dash))
conflicted::conflict_prefer_all("bs4Dash", "shiny", quiet = TRUE)
conflicted::conflicts_prefer("bs4Dash"::box, .quiet = TRUE)
suppressPackageStartupMessages(library(shinyjs))
library(shinyalert)
library(shinyWidgets)
library(fresh)
# library(waiter)

library(DT)
suppressPackageStartupMessages(library(tidyverse))
conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
library(lubridate)
library(httr)
library(jsonlite)
library(glue)
# library(safer)

# library(quarto)
# library(knitr)

library(DBI)
library(RSQLite)
# remotes::install_github("karthik/rdrop2")
library(rdrop2)
drop_auth(rdstoken = "./token.rds")


### Create the theme
bs4dashTheme <- create_theme(
  theme = "default", #https://bootswatch.com/
  bs4dash_vars(
    # navbar_white = "#75b8d1",
    # "gray-dark" = "#000000",
    navbar_light_color = "#FFF", #"#4e4429",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#bec5cb"
  ),
  bs4dash_layout(
    main_bg = "#cdd1d5"
  ),
  bs4dash_status(
    primary = "#F48635",
    info = "#000000",
    danger = "#BF616A",
    light = "#fff",
    dark = "#000000",
    success = "#DF357B"
  ),
  bs4dash_color(
    gray_900 = "#454D55"
  )
)