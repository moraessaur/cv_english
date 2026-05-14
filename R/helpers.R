library(tools)
library(readxl)
library(dplyr)

get_stems <- function(path, pattern = "\\.md$") {
  if (!dir.exists(path)) {
    return(character(0))
  }

  files <- list.files(
    path,
    pattern = pattern,
    full.names = FALSE
  )

  file_path_sans_ext(files)
}

get_md_files <- function(path) {
  if (!dir.exists(path)) {
    return(character(0))
  }

  list.files(
    path,
    pattern = "\\.md$",
    full.names = FALSE
  )
}

read_md_file <- function(path) {
  if (!file.exists(path)) {
    return("")
  }

  paste(
    readLines(path, warn = FALSE),
    collapse = "\n"
  )
}

safe_md_name <- function(x) {
  x <- trimws(x)

  if (!nzchar(x)) {
    return("")
  }

  x <- gsub("\\.md$", "", x)
  x <- gsub("[^A-Za-z0-9_-]", "_", x)

  paste0(x, ".md")
}

get_role_choices <- function(workbook_path = "data/cv_main.xlsx") {

  if (!file.exists(workbook_path)) {
    return(c(
      "1 - Melhor Envio | Data Scientist" = 1,
      "2 - PicPay | Senior Data Analyst" = 2,
      "3 - Carrefour | Senior Data Scientist" = 3,
      "4 - Ria Money Transfer | Senior Data Scientist" = 4,
      "5 - CNCFlora | Data Scientist" = 5
    ))
  }

  roles <- readxl::read_excel(
    workbook_path,
    sheet = "roles"
  )

  labels <- paste0(
    roles$role_id,
    " - ",
    roles$company,
    " | ",
    roles$title,
    " (",
    roles$start,
    "-",
    roles$end,
    ")"
  )

  stats::setNames(
    roles$role_id,
    labels
  )
}

get_recent_html_renders <- function(
    paths = c("../renders", "renders", "scripts"),
    n = 20
) {
  existing_paths <- paths[dir.exists(paths)]

  if (length(existing_paths) == 0) {
    return(character(0))
  }

  files <- unlist(lapply(existing_paths, function(path) {
    list.files(
      path,
      pattern = "\\.html$",
      full.names = TRUE,
      recursive = TRUE
    )
  }))

  files <- unique(files)

  if (length(files) == 0) {
    return(character(0))
  }

  files <- files[file.exists(files)]

  mtimes <- file.info(files)$mtime
  files <- files[order(mtimes, decreasing = TRUE)]

  head(files, n)
}

html_render_choices <- function(files) {
  if (length(files) == 0) {
    return(character(0))
  }

  mtimes <- format(
    file.info(files)$mtime,
    "%Y-%m-%d %H:%M"
  )

  labels <- paste0(
    basename(files),
    " — ",
    mtimes,
    " — ",
    dirname(files)
  )

  stats::setNames(files, labels)
}

html_file_to_resource_path <- function(path) {
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)

  parent_renders <- normalizePath("../renders", winslash = "/", mustWork = FALSE)
  local_renders <- normalizePath("renders", winslash = "/", mustWork = FALSE)
  scripts_dir <- normalizePath("scripts", winslash = "/", mustWork = FALSE)

  if (startsWith(path_norm, parent_renders)) {
    rel <- sub(paste0("^", parent_renders, "/?"), "", path_norm)
    return(file.path("renders_parent", rel))
  }

  if (startsWith(path_norm, local_renders)) {
    rel <- sub(paste0("^", local_renders, "/?"), "", path_norm)
    return(file.path("renders_local", rel))
  }

  if (startsWith(path_norm, scripts_dir)) {
    rel <- sub(paste0("^", scripts_dir, "/?"), "", path_norm)
    return(file.path("scripts_static", rel))
  }

  path
}

`%||%` <- function(x, y) {
  if (is.null(x) || identical(x, "")) y else x
}