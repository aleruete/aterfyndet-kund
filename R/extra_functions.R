isValidEmail <- function(x) {
  x <- as.character(x)
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", x, ignore.case = TRUE)
}

# Load UI content from a file
load_ui_content <- function(file) {
  source(file, local = TRUE)$value
}