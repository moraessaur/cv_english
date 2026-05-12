library(tidyverse)
library(readxl)
library(readr)
library(googledrive)

source("R/pipeline_functions.R")
source("R/gpt_functions.R")
source("R/utils.R")
source("R/pipeline_helpers.R")
source("R/form_answers.R")

run_form_pipeline_app <- function(
    mode = c("question", "summaries"),
    file_stem,
    job_description_stem,
    recruiter_message_stem = NULL,
    question_stem = NULL,
    selected_role_ids = c(4, 3, 2, 1),
    source_file = "data/cv_main.xlsx",
    workbook_path = "data/cv_main.xlsx",
    master_drive_file_id = "1XoA9TXpG0P2LSh2jJ-lWGiy8zGNNdb81_wgnaxH-S4A"
) {

  mode <- match.arg(mode)

  cfg <- make_pipeline_config(
    file_stem = file_stem,
    job_description_stem = job_description_stem,
    recruiter_message_stem = recruiter_message_stem,
    source_file = source_file,
    workbook_path = workbook_path
  )

  cat("\n=== Form pipeline config ===\n")
  cat("Mode:", mode, "\n")
  cat("File stem:", file_stem, "\n")
  cat("Job description stem:", job_description_stem, "\n")
  cat("Recruiter message stem:", recruiter_message_stem %||% "NULL", "\n")
  cat("Selected role IDs:", paste(selected_role_ids, collapse = ", "), "\n")

  drive_auth()

  drive_download(
    file = as_id(master_drive_file_id),
    path = cfg$workbook_path,
    type = "xlsx",
    overwrite = TRUE
  )

  output_dir <- file.path("output/forms", cfg$job_string)

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

    return(list(
      mode = mode,
      output_path = role_summaries$output_path[[1]],
      content = role_summaries$content[[1]]
    ))
  }

  if (mode == "question") {

    if (is.null(question_stem) || !nzchar(question_stem)) {
      stop("question_stem is required when mode = 'question'.")
    }

    question_path <- file.path(
      "prompts/forms/questions",
      paste0(question_stem, ".md")
    )

    if (!file.exists(question_path)) {
      stop("Question file not found: ", question_path)
    }

    question <- readr::read_file(question_path)

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

    return(list(
      mode = mode,
      output_path = question_answer$output_path[[1]],
      question = question_answer$question[[1]],
      content = question_answer$answer[[1]]
    ))
  }

  stop("Invalid mode. Use 'summaries' or 'question'.")
}