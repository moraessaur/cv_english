library(tidyverse)
library(readxl)
library(readr)
library(gptr)
library(httr2)
library(jsonlite)
library(dplyr)
library(writexl)
library(glue)
library(uuid)
library(googledrive)

source("R/gpt_functions.R")
source("R/utils.R")
source("R/pipeline_functions.R")
source("R/render.R")
source("R/job_rec_descriptions_functions.R")
source("R/pipeline_helpers.R")


# =========================
# CONFIG
# =========================

library(jsonlite)

config_path <- Sys.getenv(
  "CV_PIPELINE_CONFIG",
  unset = "configs/cv_pipeline_config.json"
)

if (!file.exists(config_path)) {
  stop("Config file not found: ", config_path)
}

raw_config <- jsonlite::fromJSON(
  config_path,
  simplifyVector = FALSE
)

p <- raw_config$parameters

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

cfg <- make_pipeline_config(
  file_stem = p$file_stem,
  job_description_stem = p$job_description_stem %||% NULL,
  recruiter_message_stem = p$recruiter_message_stem %||% NULL,
  optional_obs = p$optional_obs %||% NULL,
  cv_context = p$cv_context %||% NULL,
  role_context = p$role_context %||% NULL,
  skills_context = p$skills_context %||% NULL,
  role_variant = p$role_variant,
  academic_variant = p$academic_variant,
  role_max_bullets = p$role_max_bullets,
  academic_max_bullets = p$academic_max_bullets,
  selected_role_ids = unlist(p$selected_role_ids),
  source_file = p$source_file,
  workbook_path = p$workbook_path,
  render_xlsx_path = p$render_xlsx_path,
  render_output_dir = p$render_output_dir,
  drive_sheet_folder = p$drive_sheet_folder,
  input_file = p$input_file,
  pdf_mode = p$pdf_mode
)

# =========================
# DOWNLOAD MASTER FROM GOOGLE DRIVE
# =========================

drive_auth()

drive_download(
  file = as_id(p$master_drive_file_id),
  path = cfg$workbook_path,
  type = "xlsx",
  overwrite = TRUE
)


# =========================
# LOAD STATIC SHEETS
# =========================

language_skills <- read_excel(cfg$source_file, sheet = "language_skills")
text_blocks     <- read_excel(cfg$source_file, sheet = "text_blocks")
contact_info    <- read_excel(cfg$source_file, sheet = "contact_info")


# =========================
# GENERATE ENTRIES
# =========================

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
  max_categories = p$max_categories,
  use_expertise = p$use_expertise
  )

entries <- entries |>
  mutate(loc = institution) |>
  mutate(institution = NA)


# =========================
# DEBUG: CHECK FOR HTML TAGS THAT CAN BREAK PAGEDOWN
# =========================

cat("\n=== Checking generated entries for <abbr> tags ===\n")

entries |>
  select(starts_with("description_")) |>
  summarise(across(
    everything(),
    ~ any(str_detect(.x, "<abbr|</abbr"), na.rm = TRUE)
  )) |>
  print()


# =========================
# SANITIZE GENERATED TEXT BEFORE RENDERING
# =========================

entries <- entries |>
  mutate(across(
    where(is.character),
    ~ str_replace_all(.x, "</?abbr[^>]*>", "")
  ))


# =========================
# DEBUG: CHECK GENERATED SECTIONS
# =========================

cat("\n=== Sections generated ===\n")
print(entries |> count(section))

cat("\n=== Skills stack preview ===\n")
print(
  entries |>
    filter(section == "skills_stack") |>
    select(title, starts_with("description_"))
)


# =========================
# BUILD RENDER WORKBOOK
# =========================

cv_render <- list(
  entries = entries,
  language_skills = language_skills,
  text_blocks = text_blocks,
  contact_info = contact_info
)

write_xlsx(cv_render, cfg$render_xlsx_path)


# =========================
# UPLOAD RENDER SHEET TO DRIVE
# =========================

drive_auth()

file <- drive_upload(
  media = cfg$render_xlsx_path,
  path = cfg$drive_sheet_folder,
  name = cfg$render_sheet_name,
  type = "spreadsheet"
)

drive_share(file, role = "reader", type = "anyone")

file <- drive_get(as_id(file$id))
link <- file$drive_resource[[1]]$webViewLink


# =========================
# RENDER HTML CV
# =========================

render_cv_from_sheet(
  data_location = link,
  input_file = cfg$input_file,
  output_file = cfg$html_out,
  pdf_mode = cfg$pdf_mode
)


# =========================
# NORMALIZE RENDER OUTPUT PATHS
# =========================

render_base_dir <- dirname(cfg$input_file)

html_path <- normalizePath(
  file.path(render_base_dir, cfg$html_out),
  mustWork = FALSE
)

pdf_path <- normalizePath(
  file.path(render_base_dir, cfg$pdf_out),
  mustWork = FALSE
)

cat("HTML path:", html_path, "\n")
cat("HTML exists:", file.exists(html_path), "\n")


# =========================
# UPLOAD HTML TO DRIVE
# =========================

if (!file.exists(html_path)) {
  stop("Rendered HTML not found at: ", html_path)
}

html_file <- drive_upload(
  media = html_path,
  path = p$html_drive_folder,
  name = basename(html_path),
  type = "text/html"
)

drive_share(html_file, role = "reader", type = "anyone")


# =========================
# GENERATE PDF FROM HTML + UPLOAD
# =========================

if (cfg$pdf_mode) {
  pdf_result <- tryCatch({

    pagedown::chrome_print(
      input = html_path,
      output = pdf_path,
      timeout = 120
    )

    if (!file.exists(pdf_path)) {
      stop("Rendered PDF not found at: ", pdf_path)
    }

    pdf_file <- drive_upload(
      media = pdf_path,
      path = p$pdf_drive_folder,
      name = basename(pdf_path),
      type = "application/pdf"
    )

    drive_share(pdf_file, role = "reader", type = "anyone")

    cat("PDF uploaded successfully:", basename(pdf_path), "\n")

    TRUE

  }, error = function(e) {
    cat("\nPDF generation failed, but HTML was created successfully.\n")
    cat("Reason:", conditionMessage(e), "\n")
    FALSE
  })
}


# =========================
# FINAL LOGS
# =========================

cat("Rendered HTML:", cfg$html_out, "\n")
cat("Planned PDF path:", cfg$pdf_out, "\n")
cat("Job description path:", cfg$job_description_path %||% "NULL", "\n")
cat("Recruiter message path:", cfg$recruiter_message_path %||% "NULL", "\n")
