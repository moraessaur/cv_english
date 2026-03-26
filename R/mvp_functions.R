library(readxl)
library(readr)
library(dplyr)
library(stringr)

generate_cv_entries <- function(
  workbook_path,
  variant = "balanced",
  job_description = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
) {

  # ---------------------------
  # 1. Load all sheets
  # ---------------------------
  roles <- read_excel(workbook_path, sheet = "roles")
  details <- read_excel(workbook_path, sheet = "experience_details")
  achievements <- read_excel(workbook_path, sheet = "achievements")
  metrics <- read_excel(workbook_path, sheet = "metrics")

  # ---------------------------
  # 2. Load prompts
  # ---------------------------
  base_prompt <- read_file("prompts/base/base_rules.md")
  variant_prompt <- read_file(paste0("prompts/base/", variant, ".md"))

  # ---------------------------
  # 3. Loop per role
  # ---------------------------
  results <- list()

  for (i in seq_len(nrow(roles))) {

    role <- roles[i, ]

    # --- context (long text)
    role_detail <- details %>%
      filter(role_id == role$role_id) %>%
      pull(raw_text) %>%
      paste(collapse = "\n\n")

    # --- achievements
    role_ach <- achievements %>%
      filter(role_id == role$role_id)

    ach_text <- paste0("- ", role_ach$raw_text, collapse = "\n")

    # --- metrics
    role_metrics <- metrics %>%
      filter(role_id == role$role_id)

    metrics_text <- if (nrow(role_metrics) > 0) {
      paste0("- ", role_metrics$description, collapse = "\n")
    } else {
      "None"
    }

    # ---------------------------
    # 4. Build prompt
    # ---------------------------
    prompt <- paste(
      base_prompt,
      variant_prompt,

      "\n---\nROLE\n",
      paste(
        "Title:", role$title,
        "\nCompany:", role$company,
        "\nStart:", role$start,
        "\nEnd:", role$end
      ),

      "\n---\nCONTEXT\n",
      role_detail,

      "\n---\nACHIEVEMENTS\n",
      ach_text,

      "\n---\nMETRICS\n",
      metrics_text,

      if (!is.null(job_description)) {
        paste("\n---\nJOB DESCRIPTION\n", job_description)
      } else "",

      "\n---\nTASK\n",
      "Generate 3–5 CV bullet points.\nReturn plain text, one bullet per line.",

      sep = "\n"
    )

    # ---------------------------
    # 5. Call LLM
    # ---------------------------
    output <- call_openai_text(
      prompt = prompt,
      model = model,
      api_key = api_key
    )

    # ---------------------------
    # 6. Split bullets
    # ---------------------------
    bullets <- str_split(output, "\n")[[1]]
    bullets <- str_trim(bullets)
    bullets <- bullets[bullets != ""]
    bullets <- str_remove(bullets, "^[-•*]\\s*")

    bullets <- bullets[1:min(5, length(bullets))]

    bullets <- c(bullets, rep(NA, 5 - length(bullets)))

    # ---------------------------
    # 7. Build entry row
    # ---------------------------
    results[[i]] <- tibble(
      section = role$section,
      title = role$title,
      institution = role$company,
      start = role$start,
      end = role$end,
      description_1 = bullets[1],
      description_2 = bullets[2],
      description_3 = bullets[3],
      description_4 = bullets[4],
      description_5 = bullets[5],
      in_resume = TRUE
    )
  }

  bind_rows(results)
}

call_openai_text <- function(prompt, model, api_key) {

  req_body <- list(
    model = model,
    input = list(
      list(
        role = "user",
        content = list(
          list(type = "input_text", text = prompt)
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

  resp_json$output[[1]]$content[[1]]$text
}