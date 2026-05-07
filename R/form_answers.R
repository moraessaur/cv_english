library(tidyverse)
library(readxl)
library(readr)
library(glue)
library(stringr)

safe_read_sheet <- function(workbook_path, sheet) {
  if (!(sheet %in% readxl::excel_sheets(workbook_path))) {
    return(tibble())
  }

  readxl::read_excel(workbook_path, sheet = sheet)
}

collapse_row_text <- function(df) {
  if (nrow(df) == 0) {
    return("")
  }

  df |>
    mutate(across(everything(), as.character)) |>
    tidyr::unite("text", everything(), sep = " | ", na.rm = TRUE) |>
    pull(text) |>
    paste(collapse = "\n")
}

load_form_context <- function(workbook_path) {
  list(
    roles = safe_read_sheet(workbook_path, "roles") |>
      mutate(role_id = as.numeric(role_id)),

    experience_details = safe_read_sheet(workbook_path, "experience_details") |>
      mutate(role_id = as.numeric(role_id)),

    achievements = safe_read_sheet(workbook_path, "achievements") |>
      mutate(role_id = as.numeric(role_id)),

    metrics = safe_read_sheet(workbook_path, "metrics") |>
      mutate(role_id = as.numeric(role_id)),

    skills_stack = safe_read_sheet(workbook_path, "skills_stack"),

    education = safe_read_sheet(workbook_path, "education")
  )
}

build_role_context <- function(context, role_id) {
  role_id <- as.numeric(role_id)

  roles <- context$roles |>
    filter(role_id == !!role_id)

  details <- context$experience_details |>
    filter(role_id == !!role_id)

  achievements <- context$achievements |>
    filter(role_id == !!role_id)

  metrics <- context$metrics |>
    filter(role_id == !!role_id)

  paste(
    "ROLE:",
    collapse_row_text(roles),
    "",
    "EXPERIENCE DETAILS:",
    collapse_row_text(details),
    "",
    "ACHIEVEMENTS:",
    collapse_row_text(achievements),
    "",
    "METRICS:",
    collapse_row_text(metrics),
    sep = "\n"
  )
}

build_global_context <- function(context) {
  paste(
    "SKILLS STACK:",
    collapse_row_text(context$skills_stack),
    "",
    "EDUCATION:",
    collapse_row_text(context$education),
    sep = "\n"
  )
}

generate_role_summary <- function(
  context,
  role_id,
  job_description = NULL,
  recruiter_message = NULL,
  prompt_path = "prompts/forms/experience_summary.md",
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  min_words = 90,
  max_words = 140
) {
  base_prompt <- readr::read_file(prompt_path)

  role_context <- build_role_context(context, role_id)
  global_context <- build_global_context(context)

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

  prompt <- paste0(
    base_prompt,
    "\n\n---\n\n",
    "WORD LIMIT:\n",
    min_words, " to ", max_words, " words.\n\n",
    "ROLE CONTEXT:\n",
    role_context,
    "\n\nGLOBAL CANDIDATE CONTEXT:\n",
    global_context,
    jd_text,
    recruiter_text,
    "\n\n---\n\n",
    "Task:\n",
    "Write a polished application-form summary for this role.\n",
    "Use first person.\n",
    "Use only the provided facts.\n"
  )

  call_openai_text(
    prompt = prompt,
    model = model,
    api_key = api_key
  )
}

generate_all_role_summaries <- function(
  workbook_path,
  selected_role_ids = NULL,
  job_description = NULL,
  recruiter_message = NULL,
  output_dir = "output/forms",
  file_stem = "application",
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

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

    output_sections[[as.character(role_id)]] <- paste0(
      "==================================================\n",
      "COMPANY: ", company, "\n",
      "ROLE: ", role_title, "\n",
      "==================================================\n\n",
      summary,
      "\n\n"
    )
  }

  final_output <- paste(output_sections, collapse = "\n")

  combined_path <- file.path(
    output_dir,
    paste0(file_stem, "_role_summaries.txt")
  )

  writeLines(final_output, combined_path)

  tibble(
    output_path = combined_path,
    content = final_output
  )
}

generate_form_answer <- function(
  workbook_path,
  question,
  job_description = NULL,
  recruiter_message = NULL,
  selected_role_ids = NULL,
  prompt_path = "prompts/forms/question_answer.md",
  output_dir = "output/forms",
  file_stem = "application",
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

  role_contexts <- map_chr(
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
    "\n\n---\n\n",
    "Task:\n",
    "Write the strongest possible answer to the question.\n",
    "Use first person.\n",
    "Use only the provided facts.\n"
  )

  answer <- call_openai_text(
    prompt = prompt,
    model = model,
    api_key = api_key
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  safe_question <- question |>
    str_to_lower() |>
    str_sub(1, 60) |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_+$", "")

  out_path <- file.path(
    output_dir,
    paste0(file_stem, "_question_", safe_question, ".txt")
  )

  writeLines(answer, out_path)

  tibble(
    question = question,
    output_path = out_path,
    answer = answer
  )
}