alter_dataframe_text <- function(
  df,
  instruction,
  text_columns = NULL,
  model = "gpt-4.1-mini",
  api_key
) {
  if (missing(api_key) || is.null(api_key) || api_key == "") {
    stop("You must provide a valid OpenAI API key.")
  }

  if (missing(instruction) || is.null(instruction) || instruction == "") {
    stop("You must provide an instruction string.")
  }

  if (!is.data.frame(df)) {
    stop("`df` must be a dataframe.")
  }

  # Default: alter all character columns
  if (is.null(text_columns)) {
    text_columns <- names(df)[vapply(df, is.character, logical(1))]
  }

  if (length(text_columns) == 0) {
    stop("No text columns found to alter.")
  }

  missing_cols <- setdiff(text_columns, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "These columns are not in the dataframe:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # Build payload with row id
  payload_df <- df %>%
    mutate(.row_id = row_number()) %>%
    select(.row_id, all_of(text_columns))

  df_json <- jsonlite::toJSON(
    payload_df,
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  prompt_text <- paste0(
    "You will receive a JSON array representing rows from a dataframe.\n\n",
    "Your task is to modify ONLY these columns: ",
    paste(text_columns, collapse = ", "),
    ".\n\n",
    "User instruction:\n",
    instruction,
    "\n\n",
    "Rules:\n",
    "- Preserve the number of rows exactly.\n",
    "- Preserve .row_id exactly.\n",
    "- Return exactly one object per input row.\n",
    "- Return ONLY raw JSON.\n",
    "- Do not include markdown.\n",
    "- Do not include explanations.\n",
    "- Do not add or remove columns.\n",
    "- Keep null values as null.\n",
    "- Only change the requested text content.\n\n",
    "Return a JSON array of objects with exactly these keys:\n",
    paste(c(".row_id", text_columns), collapse = ", "),
    "\n\n",
    "Input data:\n",
    df_json
  )

  req_body <- list(
    model = model,
    input = list(
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = prompt_text
          )
        )
      )
    )
  )

  safe_openai_post <- function(req) {
    resp <- req_perform(req)
    status <- resp_status(resp)
    body <- resp_body_string(resp)

    if (status >= 400) {
      cat("HTTP status:", status, "\n")
      cat(body, "\n")
      stop("OpenAI request failed. See body above.")
    }

    resp
  }

  extract_output_text <- function(resp_json) {
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

    stop("Could not extract text output from the API response.")
  }

  clean_json_array <- function(x) {
    x <- gsub("```json", "", x, fixed = TRUE)
    x <- gsub("```", "", x, fixed = TRUE)
    x <- trimws(x)

    start_pos <- regexpr("\\[", x)
    end_pos <- max(gregexpr("\\]", x)[[1]])

    if (start_pos[1] == -1 || end_pos[1] == -1) {
      stop("Could not find a JSON array in the model output.")
    }

    substr(x, start_pos[1], end_pos[1])
  }

  req <- request("https://api.openai.com/v1/responses") %>%
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(req_body, auto_unbox = TRUE)

  resp <- safe_openai_post(req)
  resp_json <- resp_body_json(resp)

  out_text <- extract_output_text(resp_json)
  cleaned <- clean_json_array(out_text)

  rewritten <- jsonlite::fromJSON(cleaned, simplifyDataFrame = TRUE)

  if (!is.data.frame(rewritten)) {
    stop("Parsed output is not a dataframe.")
  }

  expected_cols <- c(".row_id", text_columns)
  missing_returned <- setdiff(expected_cols, names(rewritten))
  if (length(missing_returned) > 0) {
    stop(
      paste(
        "The model output is missing these expected columns:",
        paste(missing_returned, collapse = ", ")
      )
    )
  }

  if (nrow(rewritten) != nrow(df)) {
    stop(
      paste0(
        "Row count mismatch. Input rows: ", nrow(df),
        "; returned rows: ", nrow(rewritten), "."
      )
    )
  }

  rewritten <- rewritten %>%
    arrange(.row_id)

  result <- df %>%
    mutate(.row_id = row_number()) %>%
    left_join(
      rewritten %>% select(all_of(expected_cols)),
      by = ".row_id",
      suffix = c("", "__new")
    )

  for (col in text_columns) {
    new_col <- paste0(col, "__new")
    result[[col]] <- result[[new_col]]
    result[[new_col]] <- NULL
  }

  result$.row_id <- NULL

  result
}
