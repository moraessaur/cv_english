library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)

call_openai_text <- function(prompt, model, api_key) {
  req_body <- list(
    model = model,
    input = list(
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = prompt
          )
        )
      )
    )
  )

  resp <- request("https://api.openai.com/v1/responses") %>%
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(req_body, auto_unbox = TRUE) %>%
    req_perform()

  resp_json <- resp_body_json(resp)

  if (!is.null(resp_json$output_text)) {
    return(resp_json$output_text)
  }

  if (!is.null(resp_json$output) &&
      length(resp_json$output) > 0 &&
      !is.null(resp_json$output[[1]]$content) &&
      length(resp_json$output[[1]]$content) > 0 &&
      !is.null(resp_json$output[[1]]$content[[1]]$text)) {
    return(resp_json$output[[1]]$content[[1]]$text)
  }

  stop("Could not extract model output.")
}

split_bullets <- function(text, max_bullets = 5) {
  bullets <- str_split(text, "\n")[[1]]
  bullets <- str_trim(bullets)
  bullets <- bullets[bullets != ""]
  bullets <- str_remove(bullets, "^[-•*]\\s*")
  bullets <- bullets[1:min(length(bullets), max_bullets)]

  if (length(bullets) < max_bullets) {
    bullets <- c(bullets, rep(NA_character_, max_bullets - length(bullets)))
  }

  bullets
}

collapse_description_cols <- function(df_row) {
  desc_cols <- grep("^description_", names(df_row), value = TRUE)
  vals <- unlist(df_row[desc_cols], use.names = FALSE)
  vals <- vals[!is.na(vals)]
  vals <- str_trim(as.character(vals))
  vals <- vals[vals != ""]
  paste(vals, collapse = "\n")
}

generate_role_entries <- function(
  workbook_path,
  variant = "balanced",
  job_description = NULL,
  tailoring_brief = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  max_bullets = NULL,
  selected_role_ids = NULL
) {
  roles <- read_excel(workbook_path, sheet = "roles")
  details <- read_excel(workbook_path, sheet = "experience_details")
  achievements <- read_excel(workbook_path, sheet = "achievements")
  metrics <- read_excel(workbook_path, sheet = "metrics")

  if (!is.null(selected_role_ids)) {
    roles <- roles %>% filter(role_id %in% selected_role_ids)
  }

  base_prompt <- read_file("prompts/base/base_rules.md")
  variant_prompt <- read_file(file.path("prompts/base", paste0(variant, ".md")))

  if (is.null(tailoring_brief)) {
    tailoring_brief <- list(
      overall_tone = "concise, technical, and business-oriented",
      role_focus = "emphasize business impact, technical execution, and ownership",
      academic_focus = "emphasize transferable analytical and technical strengths",
      summary_focus = "position the candidate as a strong senior data scientist with practical delivery experience",
      priority_keywords = character(0),
      de_emphasize = character(0)
    )
  }

  tailoring_text <- format_tailoring_brief(tailoring_brief, section = "roles")

  if (is.null(max_bullets)) {
    bullet_config <- list(
      concise = 2,
      brief = 2,
      balanced = 3,
      detailed = 5
    )
    max_bullets <- bullet_config[[variant]] %||% 3
  }

  out <- vector("list", nrow(roles))

  for (i in seq_len(nrow(roles))) {
    role <- roles[i, ]

    role_detail <- details %>%
      filter(role_id == role$role_id) %>%
      pull(raw_text)

    role_detail <- if (length(role_detail) == 0) {
      ""
    } else {
      paste(role_detail, collapse = "\n\n")
    }

    role_achievements <- achievements %>%
      filter(role_id == role$role_id)

    achievements_text <- if (nrow(role_achievements) > 0) {
      paste0("- ", role_achievements$raw_text, collapse = "\n")
    } else {
      "None provided."
    }

    role_metrics <- metrics %>%
      filter(role_id == role$role_id)

    metrics_text <- if (nrow(role_metrics) > 0) {
      if ("description" %in% names(role_metrics)) {
        paste0("- ", role_metrics$description, collapse = "\n")
      } else {
        paste(capture.output(print(role_metrics)), collapse = "\n")
      }
    } else {
      "None provided."
    }

    role_location <- if ("location" %in% names(role)) as.character(role$location) else NA_character_

    jd_text <- if (!is.null(job_description) && nzchar(job_description)) {
      paste0("\n\nJob description:\n", job_description)
    } else {
      ""
    }

    prompt <- paste0(
      base_prompt,
      "\n\n---\n\n",
      variant_prompt,
      "\n\n---\n\n",
      tailoring_text,
      "\n---\n\n",
      "Role information:\n",
      "Role ID: ", role$role_id, "\n",
      "Title: ", role$title, "\n",
      "Company: ", role$company, "\n",
      "Start: ", role$start, "\n",
      "End: ", role$end, "\n",
      "Location: ", role_location, "\n\n",
      "Detailed role context:\n",
      role_detail, "\n\n",
      "Achievements:\n",
      achievements_text, "\n\n",
      "Metrics:\n",
      metrics_text,
      jd_text,
      "\n\n---\n\n",
      "Task:\n",
      "Write CV bullet points for this role.\n",
      "Tailor the bullets to the provided tailoring brief and job description if available.\n",
      "Prioritize skills, tools, achievements, and outcomes that match the target opportunity.\n",
      "Use only the information provided in the role context, achievements, metrics, and job description.\n",
      "Do NOT invent experience, metrics, scope, tools, responsibilities, or results.\n",
      "Return plain text only, one bullet per line."
    )

    model_output <- call_openai_text(
      prompt = prompt,
      model = model,
      api_key = api_key
    )

    bullets <- split_bullets(model_output, max_bullets = max_bullets)

    desc <- rep(NA_character_, 5)
    desc[1:length(bullets)] <- bullets

    out[[i]] <- tibble(
      role_id = role$role_id,
      section = role$section,
      title = role$title,
      loc = role_location,
      institution = role$company,
      start = as.character(role$start),
      end = as.character(role$end),
      description_1 = desc[1],
      description_2 = desc[2],
      description_3 = desc[3],
      description_4 = desc[4],
      description_5 = desc[5],
      in_resume = TRUE
    )
  }

  bind_rows(out)
}

generate_education_entries <- function(workbook_path) {
  education <- read_excel(workbook_path, sheet = "education")

  education %>%
    mutate(
      section = "education",
      title = paste(degree, field, sep = ", "),
      loc = loc,
      institution = institution,
      start = as.character(start),
      end = as.character(end),
      description_1 = raw_text,
      description_2 = NA_character_,
      description_3 = NA_character_,
      description_4 = NA_character_,
      description_5 = NA_character_,
      in_resume = TRUE
    ) %>%
    select(
      section, title, loc, institution, start, end,
      description_1, description_2, description_3, description_4, description_5,
      in_resume
    )
}

generate_academic_entries <- function(
  workbook_path,
  variant = "balanced",
  job_description = NULL,
  tailoring_brief = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  max_bullets = NULL
) {
  academic <- read_excel(workbook_path, sheet = "academic_articles")

  base_prompt <- read_file("prompts/base/base_rules.md")
  variant_prompt <- read_file(file.path("prompts/base", paste0(variant, ".md")))

  if (is.null(tailoring_brief)) {
    tailoring_brief <- list(
      overall_tone = "concise, technical, and business-oriented",
      role_focus = "emphasize business impact, technical execution, and ownership",
      academic_focus = "emphasize transferable analytical and technical strengths",
      summary_focus = "position the candidate as a strong senior data scientist with practical delivery experience",
      priority_keywords = character(0),
      de_emphasize = character(0)
    )
  }

  tailoring_text <- format_tailoring_brief(tailoring_brief, section = "academic")

  if (is.null(max_bullets)) {
    bullet_config <- list(
      concise = 2,
      brief = 2,
      balanced = 3,
      detailed = 5
    )
    max_bullets <- bullet_config[[variant]] %||% 3
  }

  out <- vector("list", nrow(academic))

  for (i in seq_len(nrow(academic))) {
    row <- academic[i, ]

    source_text <- collapse_description_cols(row)

    academic_loc <- if ("loc" %in% names(row)) as.character(row$loc) else NA_character_
    academic_institution <- if ("institution" %in% names(row)) as.character(row$institution) else NA_character_
    academic_start <- if ("start" %in% names(row)) as.character(row$start) else NA_character_
    academic_end <- if ("end" %in% names(row)) as.character(row$end) else NA_character_
    academic_in_resume <- if ("in_resume" %in% names(row)) row$in_resume else TRUE

    jd_text <- if (!is.null(job_description) && nzchar(job_description)) {
      paste0("\n\nJob description:\n", job_description)
    } else {
      ""
    }

    prompt <- paste0(
      base_prompt,
      "\n\n---\n\n",
      variant_prompt,
      "\n\n---\n\n",
      tailoring_text,
      "\n---\n\n",
      "Academic / skills section information:\n",
      "Title: ", as.character(row$title), "\n",
      "Location: ", academic_loc, "\n",
      "Institution: ", academic_institution, "\n",
      "Start: ", academic_start, "\n",
      "End: ", academic_end, "\n\n",
      "Source text:\n",
      source_text,
      jd_text,
      "\n\n---\n\n",
      "Task:\n",
      "Rewrite this academic / skills section into CV bullet points.\n",
      "Tailor the bullets to the provided tailoring brief and job description if available.\n",
      "Focus on transferable strengths and relevant technical foundations.\n",
      "Use ONLY the information provided.\n",
      "Do NOT invent experience, tools, results, employers, or metrics.\n",
      "Return plain text only, one bullet per line."
    )

    model_output <- call_openai_text(
      prompt = prompt,
      model = model,
      api_key = api_key
    )

    bullets <- split_bullets(model_output, max_bullets = max_bullets)

    desc <- rep(NA_character_, 5)
    desc[1:length(bullets)] <- bullets

    out[[i]] <- tibble(
      section = "academic_articles",
      title = as.character(row$title),
      loc = academic_loc,
      institution = academic_institution,
      start = academic_start,
      end = academic_end,
      description_1 = desc[1],
      description_2 = desc[2],
      description_3 = desc[3],
      description_4 = desc[4],
      description_5 = desc[5],
      in_resume = academic_in_resume
    )
  }

  bind_rows(out)
}

pad_description_cols <- function(df, max_desc = 5) {
  for (i in seq_len(max_desc)) {
    col <- paste0("description_", i)
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }

  df %>%
    select(
      section, title, loc, institution, start, end,
      description_1, description_2, description_3, description_4, description_5,
      in_resume
    )
}

generate_cv_entries <- function(
  workbook_path,
  role_variant = "balanced",
  academic_variant = "brief",
  job_description = NULL,
  recruiter_message = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  role_max_bullets = 4,
  academic_max_bullets = 2,
  selected_role_ids = NULL
) {
  tailoring_brief <- generate_tailoring_brief(
    job_description = job_description,
    recruiter_message = recruiter_message,
    model = model,
    api_key = api_key
  )

  roles_df <- generate_role_entries(
    workbook_path = workbook_path,
    variant = role_variant,
    job_description = job_description,
    tailoring_brief = tailoring_brief,
    model = model,
    api_key = api_key,
    max_bullets = role_max_bullets,
    selected_role_ids = selected_role_ids
  )

  education_df <- generate_education_entries(workbook_path)

  academic_df <- generate_academic_entries(
    workbook_path = workbook_path,
    variant = academic_variant,
    job_description = job_description,
    tailoring_brief = tailoring_brief,
    model = model,
    api_key = api_key,
    max_bullets = academic_max_bullets
  )

  bind_rows(
    pad_description_cols(roles_df),
    pad_description_cols(education_df),
    pad_description_cols(academic_df)
  )
}