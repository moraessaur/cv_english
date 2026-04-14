library(httr2)
library(jsonlite)

call_openai_text <- function(prompt, model = "gpt-4.1-mini", api_key = Sys.getenv("OPENAI_API_KEY")) {
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

  resp <- request("https://api.openai.com/v1/responses") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(req_body, auto_unbox = TRUE) |>
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