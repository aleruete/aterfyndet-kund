dbx_token <- function() {
  token <- Sys.getenv("DROPBOX_ACCESS_TOKEN")
  if (!nzchar(token)) stop("Set DROPBOX_ACCESS_TOKEN in .Renviron or environment.")
  token
}

dbx_api <- function(endpoint, body = list()) {
  request(paste0("https://api.dropboxapi.com/2/", endpoint)) |>
    req_headers(Authorization = paste("Bearer", dbx_token())) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform() |>
    dbx_check()
}

dbx_content <- function(endpoint, args, body = NULL, content_type = "application/octet-stream") {
  req <- request(paste0("https://content.dropboxapi.com/2/", endpoint)) |>
    req_headers(
      Authorization = paste("Bearer", dbx_token()),
      "Dropbox-API-Arg" = toJSON(args, auto_unbox = TRUE),
      "Content-Type" = content_type
    )
  
  if (!is.null(body)) req <- req |> req_body_raw(body)
  
  req |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform() |>
    dbx_check()
}

dbx_check <- function(resp) {
  if (resp_status(resp) >= 400) {
    msg <- tryCatch(resp_body_string(resp), error = function(e) "")
    stop("Dropbox API error [", resp_status(resp), "]: ", msg, call. = FALSE)
  }
  resp
}

drop_account <- function() {
  dbx_api("users/get_current_account") |>
    resp_body_json(simplifyVector = TRUE)
}

drop_metadata <- function(path) {
  dbx_api("files/get_metadata", list(path = path)) |>
    resp_body_json(simplifyVector = TRUE)
}

drop_dir <- function(path = "", recursive = FALSE) {
  first <- dbx_api(
    "files/list_folder",
    list(path = path, recursive = recursive)
  ) |>
    resp_body_json(simplifyVector = TRUE)
  
  entries <- first$entries
  cursor <- first$cursor
  has_more <- isTRUE(first$has_more)
  
  while (has_more) {
    next_page <- dbx_api(
      "files/list_folder/continue",
      list(cursor = cursor)
    ) |>
      resp_body_json(simplifyVector = TRUE)
    
    entries <- dplyr::bind_rows(entries, next_page$entries)
    cursor <- next_page$cursor
    has_more <- isTRUE(next_page$has_more)
  }
  
  if (is.null(entries) || length(entries) == 0) {
    return(tibble::tibble())
  }
  
  tibble::as_tibble(entries)
}

drop_upload <- function(file, path = NULL, mode = "overwrite") {
  if (is.null(path)) {
    path <- paste0("/", basename(file))
  }
  
  raw <- readBin(file, what = "raw", n = file.info(file)$size)
  
  dbx_content(
    "files/upload",
    args = list(
      path = path,
      mode = mode,
      autorename = FALSE,
      mute = FALSE,
      strict_conflict = FALSE
    ),
    body = raw
  ) |>
    resp_body_json(simplifyVector = TRUE)
}

drop_download <- function(path, local_path = basename(path), overwrite = TRUE) {
  if (file.exists(local_path) && !overwrite) {
    stop("File exists and overwrite = FALSE: ", local_path)
  }
  
  resp <- dbx_content(
    "files/download",
    args = list(path = path),
    body = NULL,
    content_type = ""
  )
  
  writeBin(resp_body_raw(resp), local_path)
  invisible(local_path)
}

drop_delete <- function(path) {
  dbx_api("files/delete_v2", list(path = path)) |>
    resp_body_json(simplifyVector = TRUE)
}

drop_create_folder <- function(path, autorename = FALSE) {
  dbx_api(
    "files/create_folder_v2",
    list(path = path, autorename = autorename)
  ) |>
    resp_body_json(simplifyVector = TRUE)
}

drop_exists <- function(path) {
  tryCatch({
    drop_metadata(path)
    TRUE
  }, error = function(e) FALSE)
}

drop_read_csv <- function(path, ...) {
  tmp <- tempfile(fileext = ".csv")
  drop_download(path, tmp)
  read.csv(tmp, ...)
}

drop_write_csv <- function(x, path, ...) {
  tmp <- tempfile(fileext = ".csv")
  write.csv(x, tmp, row.names = FALSE, ...)
  drop_upload(tmp, path)
}