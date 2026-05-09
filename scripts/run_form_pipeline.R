library(tidyverse)
library(readxl)
library(readr)
library(googledrive)

source("R/pipeline_functions.R")
source("R/gpt_functions.R")
source("R/utils.R")
source("R/pipeline_helpers.R")
source("R/form_answers.R")

# =========================
# CONFIG
# =========================

# Choose one:
#mode <- "summaries"
mode <- "question"

question <- readr::read_file("prompts/forms/questions/riachuelo.md")

cfg <- make_pipeline_config(
  file_stem = "riachuelo",
  job_description_stem = "riachuelo",
  recruiter_message_stem = NULL,
  source_file = "data/cv_main.xlsx",
  workbook_path = "data/cv_main.xlsx"
)

selected_role_ids <- c(4, 3, 2, 1)

# =========================
# DOWNLOAD MASTER FROM DRIVE
# =========================

drive_auth()

drive_download(
  file = as_id("1XoA9TXpG0P2LSh2jJ-lWGiy8zGNNdb81_wgnaxH-S4A"),
  path = cfg$workbook_path,
  type = "xlsx",
  overwrite = TRUE
)

output_dir <- file.path("output/forms", cfg$job_string)

# =========================
# RUN MODE
# =========================

if (mode == "summaries") {

  role_summaries <- generate_all_role_summaries(
    workbook_path = cfg$workbook_path,
    selected_role_ids = selected_role_ids,
    job_description = cfg$job_description,
    recruiter_message = cfg$recruiter_message,
    output_dir = output_dir,
    file_stem = cfg$job_string
  )

  cat("\n=== Combined summaries written to ===\n")
  cat(role_summaries$output_path[[1]], "\n")

} else if (mode == "question") {

  question_answer <- generate_form_answer(
    workbook_path = cfg$workbook_path,
    question = question,
    selected_role_ids = selected_role_ids,
    job_description = cfg$job_description,
    recruiter_message = cfg$recruiter_message,
    output_dir = output_dir,
    file_stem = cfg$job_string
  )

  cat("\n=== Question answer written to ===\n")
  cat(question_answer$output_path[[1]], "\n")

} else {
  stop("Invalid mode. Use 'summaries' or 'question'.")
}