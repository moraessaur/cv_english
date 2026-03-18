render_cv <- function(payload, variant, output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  file_stem <- variant$output$file_stem %||% "cv_output"
  output_file <- paste0(file_stem, ".html")

  rmarkdown::render(
    input = "templates/cv/cv_base.Rmd",
    output_file = output_file,
    output_dir = output_dir,
    params = list(payload = payload),
    envir = new.env(parent = globalenv())
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}