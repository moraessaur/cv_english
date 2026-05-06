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

cfg <- make_pipeline_config(
  file_stem = "telus",
  job_description_stem = "telus",
  recruiter_message_stem = NULL,
  optional_obs = NULL,
  role_variant = "retail",
  academic_variant = "mlops_heavy",
  role_max_bullets = 4,
  academic_max_bullets = 3,
  selected_role_ids = c(4, 3, 2),
  source_file = "data/cv_new_reworked.xlsx",
  workbook_path = "data/cv_main.xlsx",
  render_xlsx_path = "data/cv_render_new.xlsx",
  render_output_dir = "../renders",
  drive_sheet_folder = "mimic_tear/renders/sheets",
  input_file = "scripts/cv.rmd",
  pdf_mode = TRUE
)


# =========================
# DOWNLOAD MASTER FROM GOOGLE DRIVE
# =========================

drive_auth()

drive_download(
  file = as_id("1XoA9TXpG0P2LSh2jJ-lWGiy8zGNNdb81_wgnaxH-S4A"),
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
  recruiter_message = cfg$recruiter_message
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
  path = "mimic_tear/renders/cvs/html",
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
      path = "mimic_tear/renders/cvs/pdf",
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