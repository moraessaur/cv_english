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

`%||%` <- function(x, y) {
  if (is.null(x) || identical(x, "")) y else x
}