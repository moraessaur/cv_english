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
source("R/new_mvp_functions.R")
source("R/render.R")
source("R/job_rec_descriptions_functions.R")
source("R/pipeline_helpers.R")



# =========================
# CONFIG
# =========================


cfg <- make_pipeline_config(
  file_stem = "nestle",
  job_description_stem = "nestle",
  recruiter_message_stem =  NULL,
  optional_obs = NULL,
  role_variant = "retail",
  academic_variant = "mlops_heavy", # academic skills & tech stack
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

cat("Rendered HTML:", cfg$html_out, "\n")
cat("Planned PDF path:", cfg$pdf_out, "\n")
cat("Job description path:", cfg$job_description_path %||% "NULL", "\n")
cat("Recruiter message path:", cfg$recruiter_message_path %||% "NULL", "\n")