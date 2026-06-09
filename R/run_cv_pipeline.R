library(tidyverse)
library(readxl)
library(writexl)
library(googledrive)
library(pagedown)

source("R/gpt_functions.R")
source("R/utils.R")
source("R/pipeline_functions.R")
source("R/render.R")
source("R/job_rec_descriptions_functions.R")
source("R/pipeline_helpers.R")

run_cv_pipeline <- function(
    cfg,
    max_categories = 3,
    use_expertise = TRUE
) {

  drive_auth()

  drive_download(
    file = as_id(
      "1XoA9TXpG0P2LSh2jJ-lWGiy8zGNNdb81_wgnaxH-S4A"
    ),
    path = cfg$workbook_path,
    type = "xlsx",
    overwrite = TRUE
  )

  cat("\n=== CV generation guidance ===\n")
  cat("Global CV guidance:", if (!is.null(cfg$cv_context) && nzchar(trimws(cfg$cv_context))) "yes" else "no", "\n")
  cat("Role guidance:", if (!is.null(cfg$role_context) && nzchar(trimws(cfg$role_context))) "yes" else "no", "\n")
  cat("Skills stack guidance:", if (!is.null(cfg$skills_context) && nzchar(trimws(cfg$skills_context))) "yes" else "no", "\n")

  language_skills <- read_excel(
    cfg$source_file,
    sheet = "language_skills"
  )

  text_blocks <- read_excel(
    cfg$source_file,
    sheet = "text_blocks"
  )

  contact_info <- read_excel(
    cfg$source_file,
    sheet = "contact_info"
  )

  entries <- generate_cv_entries(

    workbook_path = cfg$workbook_path,

    role_variant = cfg$role_variant,

    academic_variant = cfg$academic_variant,

    role_max_bullets = cfg$role_max_bullets,

    academic_max_bullets = cfg$academic_max_bullets,

    selected_role_ids = cfg$selected_role_ids,

    job_description = cfg$job_description,

    recruiter_message = cfg$recruiter_message,

    cv_context = cfg$cv_context,

    role_context = cfg$role_context,

    skills_context = cfg$skills_context,

    max_categories = max_categories,

    use_expertise = use_expertise
  )

  entries <- entries |>
    mutate(loc = institution) |>
    mutate(institution = NA)

  entries <- entries |>
    mutate(across(
      where(is.character),
      ~ str_replace_all(.x, "</?abbr[^>]*>", "")
    ))

  cv_render <- list(
    entries = entries,
    language_skills = language_skills,
    text_blocks = text_blocks,
    contact_info = contact_info
  )

  write_xlsx(
    cv_render,
    cfg$render_xlsx_path
  )

  file <- drive_upload(
    media = cfg$render_xlsx_path,
    path = cfg$drive_sheet_folder,
    name = cfg$render_sheet_name,
    type = "spreadsheet"
  )

  drive_share(
    file,
    role = "reader",
    type = "anyone"
  )

  file <- drive_get(as_id(file$id))

  link <- file$drive_resource[[1]]$webViewLink

  render_cv_from_sheet(
    data_location = link,
    input_file = cfg$input_file,
    output_file = cfg$html_out,
    pdf_mode = cfg$pdf_mode
  )

  render_base_dir <- dirname(cfg$input_file)

  html_path <- normalizePath(
    file.path(render_base_dir, cfg$html_out),
    mustWork = FALSE
  )

  pdf_path <- normalizePath(
    file.path(render_base_dir, cfg$pdf_out),
    mustWork = FALSE
  )

  if (cfg$pdf_mode) {

    pagedown::chrome_print(
      input = html_path,
      output = pdf_path,
      timeout = 120
    )

  }

  list(
    html_path = html_path,
    pdf_path = if (cfg$pdf_mode) pdf_path else NULL
  )
}
