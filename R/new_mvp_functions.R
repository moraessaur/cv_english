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

select_relevant_role_content <- function(
  role,
  role_detail_df,
  role_achievements_df,
  role_metrics_df,
  tailoring_brief,
  job_description = NULL,
  recruiter_message = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  min_metrics_per_role = 1,
  max_details_keep = 2,
  max_achievements_keep = 2,
  max_metrics_keep = 2
) {
  collapse_to_text <- function(x) {
    if (is.null(x)) return(NULL)
    x <- as.character(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    paste(x, collapse = "\n")
  }

  strip_code_fences <- function(x) {
    x <- trimws(x)
    x <- sub("^```json\\s*", "", x, ignore.case = TRUE)
    x <- sub("^```\\s*", "", x)
    x <- sub("\\s*```$", "", x)
    trimws(x)
  }

  to_logical_flag <- function(x) {
    if (is.null(x)) return(rep(FALSE, 0))
    x_chr <- tolower(trimws(as.character(x)))
    x_chr %in% c("true", "t", "1", "yes", "y")
  }

  job_description <- collapse_to_text(job_description)
  recruiter_message <- collapse_to_text(recruiter_message)

  role_detail_df <- role_detail_df %>%
    mutate(
      item_id = paste0("detail_", row_number()),
      item_type = "detail",
      text = as.character(raw_text),
      must_use = FALSE,
      priority = NA_character_
    ) %>%
    select(item_id, item_type, text, must_use, priority)

  role_achievements_df <- role_achievements_df %>%
    mutate(
      item_id = paste0("achievement_", row_number()),
      item_type = "achievement",
      text = as.character(raw_text),
      must_use = if ("must_use" %in% names(.)) to_logical_flag(must_use) else FALSE,
      priority = if ("priority" %in% names(.)) as.character(priority) else NA_character_
    ) %>%
    select(item_id, item_type, text, must_use, priority)

  role_metrics_df <- role_metrics_df %>%
    mutate(
      item_id = paste0("metric_", row_number()),
      item_type = "metric",
      text = dplyr::case_when(
        "description" %in% names(.) & !is.na(description) & trimws(as.character(description)) != "" ~ as.character(description),
        "value" %in% names(.) & !is.na(value) & trimws(as.character(value)) != "" ~ as.character(value),
        TRUE ~ apply(., 1, function(row) paste(row, collapse = " | "))
      ),
      must_use = if ("must_use" %in% names(.)) to_logical_flag(must_use) else FALSE,
      priority = if ("priority" %in% names(.)) as.character(priority) else NA_character_
    ) %>%
    select(item_id, item_type, text, must_use, priority)

  candidates <- bind_rows(
    role_detail_df,
    role_achievements_df,
    role_metrics_df
  ) %>%
    filter(!is.na(text), str_trim(text) != "")

  if (nrow(candidates) == 0) {
    return(list(
      details = character(0),
      achievements = character(0),
      metrics = character(0),
      raw_selection = NULL
    ))
  }

  tailoring_text <- format_tailoring_brief(tailoring_brief, section = "roles")

  candidates_text <- paste0(
    apply(candidates, 1, function(row) {
      paste0(
        "[", row[["item_id"]], "] ",
        "type=", row[["item_type"]],
        if (!is.na(row[["priority"]])) paste0(" | priority=", row[["priority"]]) else "",
        if (isTRUE(as.logical(row[["must_use"]]))) " | must_use=TRUE" else "",
        " | ",
        row[["text"]]
      )
    }),
    collapse = "\n"
  )

  prompt <- paste0(
    "You are selecting the most relevant evidence for an English CV.\n\n",
    "Choose which candidate items are most relevant for the target opportunity.\n",
    "Prefer content that best supports the target role.\n",
    "De-prioritize content that is true but less relevant.\n",
    "Quantified results and business-impact metrics are often stronger CV evidence than long descriptive context.\n",
    "Do not let long narrative details crowd out strong metrics or achievements.\n",
    "Forecast accuracy metrics like MAPE may be high priority for forecasting roles and low priority for other roles.\n",
    "Speed, efficiency, automation, and cost-savings metrics may be especially important for MLOps/platform or business-impact roles.\n\n",
    "Return ONLY valid JSON. Do not use markdown fences.\n\n",
    "Return an object with one key named 'items', containing an array of objects.\n",
    "Each object must have:\n",
    "- item_id\n",
    "- relevance  (high | medium | low)\n",
    "- reason\n\n",
    tailoring_text, "\n",
    "Role information:\n",
    "Role ID: ", role$role_id, "\n",
    "Title: ", role$title, "\n",
    "Company: ", role$company, "\n\n",
    "Job description:\n",
    ifelse(is.null(job_description), "", job_description), "\n\n",
    "Recruiter message:\n",
    ifelse(is.null(recruiter_message), "", recruiter_message), "\n\n",
    "Candidate items:\n",
    candidates_text, "\n\n",
    "Rules:\n",
    "- Keep the output grounded in the provided content\n",
    "- Do not invent facts\n",
    "- Use 'high' for the strongest supporting evidence for this opportunity\n",
    "- Use 'medium' for useful but secondary evidence\n",
    "- Use 'low' for true but low-priority evidence\n",
    "- If an item is marked must_use=TRUE, it should almost always be high unless clearly irrelevant\n"
  )

  raw <- call_openai_text(
    prompt = prompt,
    model = model,
    api_key = api_key
  )

  raw_clean <- strip_code_fences(raw)

  parsed <- tryCatch(
    jsonlite::fromJSON(raw_clean, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed) || is.null(parsed$items)) {
    candidates_scored <- candidates %>%
      mutate(relevance = "medium", reason = NA_character_)
  } else {
    selected <- as_tibble(parsed$items) %>%
      mutate(
        item_id = as.character(item_id),
        relevance = as.character(relevance),
        reason = as.character(reason)
      )

    candidates_scored <- candidates %>%
      left_join(selected, by = "item_id") %>%
      mutate(relevance = ifelse(is.na(relevance), "medium", relevance))
  }

  candidates_scored <- candidates_scored %>%
    mutate(
      must_use = ifelse(is.na(must_use), FALSE, must_use),
      priority_rank = case_when(
        tolower(priority) == "high" ~ 1L,
        tolower(priority) == "medium" ~ 2L,
        tolower(priority) == "low" ~ 3L,
        TRUE ~ 4L
      ),
      type_rank = case_when(
        item_type == "metric" ~ 1L,
        item_type == "achievement" ~ 2L,
        item_type == "detail" ~ 3L,
        TRUE ~ 4L
      ),
      relevance_rank = case_when(
        relevance == "high" ~ 1L,
        relevance == "medium" ~ 2L,
        relevance == "low" ~ 3L,
        TRUE ~ 4L
      )
    )

  forced_keep_df <- candidates_scored %>%
    filter(must_use %in% TRUE)

  high_df <- candidates_scored %>%
    filter(relevance == "high")

  medium_df <- candidates_scored %>%
    filter(relevance == "medium") %>%
    arrange(type_rank, priority_rank)

  medium_metrics_df <- medium_df %>%
    filter(item_type == "metric") %>%
    slice_head(n = max_metrics_keep)

  medium_achievements_df <- medium_df %>%
    filter(item_type == "achievement") %>%
    slice_head(n = max_achievements_keep)

  medium_details_df <- medium_df %>%
    filter(item_type == "detail") %>%
    slice_head(n = max_details_keep)

  keep_df <- bind_rows(
    forced_keep_df,
    high_df,
    medium_metrics_df,
    medium_achievements_df,
    medium_details_df
  ) %>%
    distinct(item_id, .keep_all = TRUE)

  current_metric_n <- sum(keep_df$item_type == "metric")

  if (min_metrics_per_role > 0 && current_metric_n < min_metrics_per_role) {
    needed <- min_metrics_per_role - current_metric_n

    extra_metrics_df <- candidates_scored %>%
      filter(item_type == "metric") %>%
      anti_join(keep_df %>% select(item_id), by = "item_id") %>%
      arrange(relevance_rank, priority_rank) %>%
      slice_head(n = needed)

    keep_df <- bind_rows(keep_df, extra_metrics_df) %>%
      distinct(item_id, .keep_all = TRUE)
  }

  keep_df <- keep_df %>%
    arrange(type_rank, priority_rank, relevance_rank)

  list(
    details = keep_df %>% filter(item_type == "detail") %>% pull(text),
    achievements = keep_df %>% filter(item_type == "achievement") %>% pull(text),
    metrics = keep_df %>% filter(item_type == "metric") %>% pull(text),
    raw_selection = candidates_scored
  )
}

generate_role_entries <- function(
  workbook_path,
  variant = "balanced",
  job_description = NULL,
  recruiter_message = NULL,
  tailoring_brief = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  max_bullets = NULL,
  selected_role_ids = NULL,
  min_metrics_per_role = 1,
  max_details_keep = 2,
  max_achievements_keep = 2,
  max_metrics_keep = 2
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

    role_detail_df <- details %>% filter(role_id == role$role_id)
    role_achievements_df <- achievements %>% filter(role_id == role$role_id)
    role_metrics_df <- metrics %>% filter(role_id == role$role_id)

    selected_content <- select_relevant_role_content(
      role = role,
      role_detail_df = role_detail_df,
      role_achievements_df = role_achievements_df,
      role_metrics_df = role_metrics_df,
      tailoring_brief = tailoring_brief,
      job_description = job_description,
      recruiter_message = recruiter_message,
      model = model,
      api_key = api_key,
      min_metrics_per_role = min_metrics_per_role,
      max_details_keep = max_details_keep,
      max_achievements_keep = max_achievements_keep,
      max_metrics_keep = max_metrics_keep
    )

    role_detail <- if (length(selected_content$details) == 0) {
      ""
    } else {
      paste(selected_content$details, collapse = "\n\n")
    }

    achievements_text <- if (length(selected_content$achievements) > 0) {
      paste0("- ", selected_content$achievements, collapse = "\n")
    } else {
      "None provided."
    }

    # Use raw role_metrics_df so value + description always reach the prompt
    metrics_text <- if (nrow(role_metrics_df) > 0) {
      paste0(
        "- ", role_metrics_df$type, ": ",
        role_metrics_df$value,
        " (", role_metrics_df$description, ")",
        collapse = "\n"
      )
    } else {
      "None provided."
    }

    priority_metrics_df <- role_metrics_df

    if ("priority" %in% names(priority_metrics_df)) {
      priority_metrics_df <- priority_metrics_df %>%
        filter(tolower(as.character(priority)) == "high")
    } else {
      priority_metrics_df <- priority_metrics_df[0, ]
    }

    priority_metrics_text <- if (nrow(priority_metrics_df) > 0) {
      paste0(
        "- ", priority_metrics_df$type, ": ",
        priority_metrics_df$value,
        " (", priority_metrics_df$description, ")",
        collapse = "\n"
      )
    } else {
      "None provided."
    }

    role_location <- if ("location" %in% names(role)) as.character(role$location) else NA_character_

    jd_text <- if (!is.null(job_description) && nzchar(paste(job_description, collapse = ""))) {
      paste0("\n\nJob description:\n", paste(job_description, collapse = "\n"))
    } else {
      ""
    }

    recruiter_text <- if (!is.null(recruiter_message) && nzchar(paste(recruiter_message, collapse = ""))) {
      paste0("\n\nRecruiter message:\n", paste(recruiter_message, collapse = "\n"))
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

      "MANDATORY ROLE-SPECIFIC METRICS — THESE EXACT VALUES MUST APPEAR NATURALLY IN THE BULLETS:\n",
      priority_metrics_text, "\n\n",

      "ALL AVAILABLE ROLE METRICS:\n",
      metrics_text, "\n\n",

      "Filtered achievements:\n",
      achievements_text, "\n\n",

      "Filtered role context:\n",
      role_detail,
      jd_text,
      recruiter_text,

      "\n\n---\n\n",
      "Task:\n",
      "Write CV bullet points for this role.\n",
      "Tailor the bullets to the provided tailoring brief, job description, and recruiter message if available.\n",
      "Prioritize skills, tools, achievements, metrics, and outcomes that match the target opportunity.\n",
      "Use only the information provided.\n\n",

      "Metric rules:\n",
      "- If mandatory role-specific metrics are provided, at least one bullet MUST include an exact metric value.\n",
      "- Use the exact metric values naturally, without sounding forced.\n",
      "- Do not replace numeric metrics with vague phrases like 'improved', 'optimized', or 'enhanced'.\n",
      "- Do not transfer metrics or terms from one role to another.\n",
      "- Prefer savings, accuracy, efficiency, or speed metrics when space is limited.\n\n",

      "Style rules:\n",
      "- Keep bullets concise and professional.\n",
      "- Around 20 words per bullet.\n",
      "- Do NOT invent experience, metrics, scope, tools, responsibilities, or results.\n",
      "- Return plain text only, one bullet per line."
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
  variant_prompt <- read_file(file.path("prompts/stack_academic", paste0(variant, ".md")))

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

generate_skills_stack_entries <- function(
  workbook_path,
  max_bullets = 5,
  max_items_per_bullet = 3
) {
  skills <- read_excel(workbook_path, sheet = "skills_stack")

  bullets <- skills %>%
    filter(
      !is.na(Category),
      !is.na(Description),
      str_trim(as.character(Category)) != "",
      str_trim(as.character(Description)) != ""
    ) %>%
    group_by(Category) %>%
    summarise(
      bullet = paste0(
        first(Category),
        ": ",
        paste(head(Description, max_items_per_bullet), collapse = ", ")
      ),
      .groups = "drop"
    ) %>%
    slice_head(n = max_bullets) %>%
    pull(bullet)

  desc <- rep(NA_character_, 5)
  desc[seq_len(min(length(bullets), 5))] <- bullets[seq_len(min(length(bullets), 5))]

  tibble(
    section = "academic_articles",
    title = "Skills & stack",
    loc = NA_character_,
    institution = NA_character_,
    start = NA_character_,
    end = NA_character_,
    description_1 = desc[1],
    description_2 = desc[2],
    description_3 = desc[3],
    description_4 = desc[4],
    description_5 = desc[5],
    in_resume = TRUE
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
  role_variant = "balanced",
  academic_variant = "brief",
  job_description = NULL,
  recruiter_message = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  role_max_bullets = 4,
  academic_max_bullets = 2,
  selected_role_ids = NULL,
  min_metrics_per_role = 1,
  max_details_keep = 2,
  max_achievements_keep = 2,
  max_metrics_keep = 2
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
    recruiter_message = recruiter_message,
    tailoring_brief = tailoring_brief,
    model = model,
    api_key = api_key,
    max_bullets = role_max_bullets,
    selected_role_ids = selected_role_ids,
    min_metrics_per_role = min_metrics_per_role,
    max_details_keep = max_details_keep,
    max_achievements_keep = max_achievements_keep,
    max_metrics_keep = max_metrics_keep
  )

  education_df <- generate_education_entries(workbook_path)

  academic_df <- generate_skills_stack_entries(
  workbook_path = workbook_path,
  max_bullets = academic_max_bullets,
  max_items_per_bullet = 2)

  bind_rows(
    pad_description_cols(roles_df),
    pad_description_cols(education_df),
    pad_description_cols(academic_df)
  )
}