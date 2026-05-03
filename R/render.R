render_cv_from_sheet <- function(
  data_location,
  input_file = "cv.rmd",
  output_file = "cv.html",
  pdf_mode = TRUE,
  envir = new.env(parent = globalenv())
) {
  if (missing(data_location) || !nzchar(data_location)) {
    stop("You must provide a non-empty data_location.")
  }

  rmarkdown::render(
    input = input_file,
    output_file = output_file,
    output_format = "pagedown::html_resume",
    params = list(
      data_location = data_location,
      pdf_mode = pdf_mode
    ),
    envir = envir
  )
}