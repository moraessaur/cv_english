library(tidyverse)
library(readxl)
library(readr)
library(googledrive)

source("R/pipeline_functions.R")
source("R/gpt_functions.R")
source("R/utils.R")
source("R/pipeline_helpers.R")
source("R/form_answers.R")

resolve_form_question <- function(
    question_stem = NULL,
    question_text = NULL,
    question_folder = "prompts/forms/questions"
) {
  if (!is.null(question_text) && nzchar(trimws(question_text))) {
    return(question_text)
  }

  if (is.null(question_stem) || !nzchar(question_stem)) {
    stop("Either pasted question text or a question file is required.")
  }

  question_path <- file.path(
    question_folder,
    paste0(question_stem, ".md")
  )

  if (!file.exists(question_path)) {
    stop("Question file not found: ", question_path)
  }

  readr::read_file(question_path)
}

generate_form_answer_preview <- function(
    workbook_path,
    question,
    selected_role_ids = NULL,
    job_description = NULL,
    recruiter_message = NULL,
    additional_guidance = NULL,
    prompt_path = "prompts/forms/question_answer.md",
    model = "gpt-4.1-mini",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    min_words = 120,
    max_words = 220
) {
  context <- load_form_context(workbook_path)

  roles <- context$roles |>
    mutate(role_id = as.numeric(role_id))

  if (!is.null(selected_role_ids)) {
    roles <- roles |>
      filter(role_id %in% as.numeric(selected_role_ids))
  }

  if (nrow(roles) == 0) {
    stop("No roles selected. Check selected_role_ids and roles sheet.")
  }

  role_contexts <- purrr::map_chr(
    roles$role_id,
    ~ build_role_context(context, .x)
  ) |>
    paste(collapse = "\n\n--- ROLE BREAK ---\n\n")

  global_context <- build_global_context(context)
  base_prompt <- readr::read_file(prompt_path)

  jd_text <- if (!is.null(job_description)) {
    paste0("\n\nJOB DESCRIPTION:\n", job_description)
  } else {
    ""
  }

  recruiter_text <- if (!is.null(recruiter_message)) {
    paste0("\n\nRECRUITER MESSAGE:\n", recruiter_message)
  } else {
    ""
  }

  guidance_text <- if (!is.null(additional_guidance) && nzchar(trimws(additional_guidance))) {
    paste0("\n\nADDITIONAL ANSWER GUIDANCE:\n", additional_guidance)
  } else {
    ""
  }

  prompt <- paste0(
    base_prompt,
    "\n\n---\n\n",
    "QUESTION:\n",
    question,
    "\n\nWORD LIMIT:\n",
    min_words, " to ", max_words, " words.\n\n",
    "ROLE CONTEXTS:\n",
    role_contexts,
    "\n\nGLOBAL CANDIDATE CONTEXT:\n",
    global_context,
    jd_text,
    recruiter_text,
    guidance_text,
    "\n\n---\n\n",
    "Task:\n",
    "Write the strongest possible answer to the question.\n",
    "Use first person.\n",
    "Use only the provided facts.\n",
    "Follow the additional answer guidance when provided.\n"
  )

  call_openai_text(
    prompt = prompt,
    model = model,
    api_key = api_key
  )
}

generate_role_summaries_preview <- function(
    workbook_path,
    selected_role_ids = NULL,
    job_description = NULL,
    recruiter_message = NULL,
    additional_guidance = NULL,
    model = "gpt-4.1-mini",
    api_key = Sys.getenv("OPENAI_API_KEY")
) {
  context <- load_form_context(workbook_path)

  roles <- context$roles |>
    mutate(role_id = as.numeric(role_id))

  cat("\n=== Available roles in roles sheet ===\n")
  print(roles |> select(role_id, company, title))

  if (!is.null(selected_role_ids)) {
    selected_role_ids <- as.numeric(selected_role_ids)

    cat("\n=== Selected role IDs ===\n")
    print(selected_role_ids)

    roles <- roles |>
      filter(role_id %in% selected_role_ids)
  }

  cat("\n=== Roles selected for form summaries ===\n")
  print(roles |> select(role_id, company, title))

  if (nrow(roles) == 0) {
    stop("No roles selected. Check selected_role_ids and roles sheet.")
  }

  output_sections <- list()

  for (i in seq_len(nrow(roles))) {
    role_id <- roles$role_id[[i]]

    role_title <- if ("title" %in% names(roles)) {
      roles$title[[i]]
    } else {
      paste0("role_", role_id)
    }

    company <- if ("company" %in% names(roles)) {
      roles$company[[i]]
    } else {
      paste0("company_", role_id)
    }

    summary <- generate_role_summary(
      context = context,
      role_id = role_id,
      job_description = job_description,
      recruiter_message = recruiter_message,
      model = model,
      api_key = api_key
    )

    if (!is.null(additional_guidance) && nzchar(trimws(additional_guidance))) {
      summary <- paste0(
        summary,
        "\n\nAdditional guidance considered:\n",
        additional_guidance
      )
    }

    output_sections[[as.character(role_id)]] <- paste0(
      "==================================================\n",
      "COMPANY: ", company, "\n",
      "ROLE: ", role_title, "\n",
      "==================================================\n\n",
      summary,
      "\n\n"
    )
  }

  paste(output_sections, collapse = "\n")
}

run_form_pipeline_preview <- function(
    mode = c("question", "summaries"),
    file_stem,
    job_description_stem,
    recruiter_message_stem = NULL,
    question_stem = NULL,
    question_text = NULL,
    additional_guidance = NULL,
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

  cat("\n=== Form pipeline preview config ===\n")
  cat("Mode:", mode, "\n")
  cat("File stem:", file_stem, "\n")
  cat("Job description stem:", job_description_stem, "\n")
  cat("Recruiter message stem:", recruiter_message_stem %||% "NULL", "\n")
  cat("Selected role IDs:", paste(selected_role_ids, collapse = ", "), "\n")

  if (!is.null(additional_guidance) && nzchar(trimws(additional_guidance))) {
    cat("Additional guidance: yes\n")
  } else {
    cat("Additional guidance: no\n")
  }

  drive_auth()

  drive_download(
    file = as_id(master_drive_file_id),
    path = cfg$workbook_path,
    type = "xlsx",
    overwrite = TRUE
  )

  if (mode == "summaries") {
    content <- generate_role_summaries_preview(
      workbook_path = cfg$workbook_path,
      selected_role_ids = selected_role_ids,
      job_description = cfg$job_description,
      recruiter_message = cfg$recruiter_message,
      additional_guidance = additional_guidance
    )

    return(list(
      mode = mode,
      file_stem = cfg$job_string,
      question = NULL,
      content = content
    ))
  }

  question <- resolve_form_question(
    question_stem = question_stem,
    question_text = question_text
  )

  content <- generate_form_answer_preview(
    workbook_path = cfg$workbook_path,
    question = question,
    selected_role_ids = selected_role_ids,
    job_description = cfg$job_description,
    recruiter_message = cfg$recruiter_message,
    additional_guidance = additional_guidance
  )

  list(
    mode = mode,
    file_stem = cfg$job_string,
    question = question,
    content = content
  )
}

save_form_preview_to_file <- function(
    content,
    file_stem = "application",
    mode = "question",
    output_dir = "output/forms/manual_saves"
) {
  if (is.null(content) || !nzchar(trimws(content))) {
    stop("No content to save.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  out_path <- file.path(
    output_dir,
    paste0(file_stem, "_", mode, "_", timestamp, ".txt")
  )

  writeLines(content, out_path, useBytes = TRUE)

  out_path
}