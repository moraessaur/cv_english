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

generate_role_entries <- function(
  workbook_path,
  variant = "balanced",
  job_description = NULL,
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

  if (is.null(max_bullets)) {
    bullet_config <- list(
      concise = 2,
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

    role_detail <- if (length(role_detail) == 0) "" else paste(role_detail, collapse = "\n\n")

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

generate_academic_entries <- function(workbook_path) {
  academic <- read_excel(workbook_path, sheet = "academic_articles")

  desc_cols <- grep("^description_", names(academic), value = TRUE)

  academic %>%
    mutate(
      section = "academic_articles",
      loc = if ("loc" %in% names(.)) loc else NA_character_,
      institution = if ("institution" %in% names(.)) institution else NA_character_,
      start = if ("start" %in% names(.)) start else NA_character_,
      end = if ("end" %in% names(.)) end else NA_character_,
      in_resume = if ("in_resume" %in% names(.)) in_resume else TRUE
    ) %>%
    select(
      section, title, loc, institution, start, end,
      all_of(desc_cols), in_resume
    )
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
  variant = "balanced",
  job_description = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  max_bullets = NULL,
  selected_role_ids = NULL
) {
  roles_df <- generate_role_entries(
    workbook_path = workbook_path,
    variant = variant,
    job_description = job_description,
    model = model,
    api_key = api_key,
    max_bullets = max_bullets,
    selected_role_ids = selected_role_ids
  )

  education_df <- generate_education_entries(workbook_path)
  academic_df <- generate_academic_entries(workbook_path)

  bind_rows(
    pad_description_cols(roles_df),
    pad_description_cols(education_df),
    pad_description_cols(academic_df)
  )
}