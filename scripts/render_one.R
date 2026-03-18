args <- commandArgs(trailingOnly = TRUE)

variant_path <- if (length(args) >= 1) {
  args[1]
} else {
  "variants/english/ml_engineer.yml"
}

data_location <- "https://docs.google.com/spreadsheets/d/1mIF3aDXQJOPzfPJqOmbdY4QMsa_asKxsjb7AieAB7xA/edit?usp=sharing"

source("R/load_variant.R")
source("R/load_cv_data.R")
source("R/build_cv_payload.R")
source("R/apply_variant_rules.R")
source("R/render_cv.R")

variant <- load_variant(variant_path)

cv_data <- load_cv_data(
  data_location = data_location,
  pdf_mode = FALSE,
  sheet_is_publicly_readable = TRUE
)

payload <- build_cv_payload(cv_data, variant)
payload <- apply_variant_rules(payload, variant)

render_cv(payload, variant, output_dir = "output")