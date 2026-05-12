library(tools)

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

`%||%` <- function(x, y) {
  if (is.null(x) || identical(x, "")) y else x
}