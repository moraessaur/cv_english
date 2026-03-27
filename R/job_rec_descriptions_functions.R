generate_tailoring_brief <- function(
  job_description = NULL,
  recruiter_message = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
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

  default_brief <- list(
    overall_tone = "concise, technical, and business-oriented",
    role_focus = "emphasize business impact, technical execution, and ownership",
    academic_focus = "emphasize transferable analytical and technical strengths",
    summary_focus = "position the candidate as a strong senior data scientist with practical delivery experience",
    priority_keywords = character(0),
    de_emphasize = character(0)
  )

  job_description <- collapse_to_text(job_description)
  recruiter_message <- collapse_to_text(recruiter_message)

  if (
    is.null(job_description) &&
    is.null(recruiter_message)
  ) {
    return(default_brief)
  }

  prompt <- paste0(
    "You are helping tailor an English CV.\n\n",
    "If the job description or recruiter message is not in English, translate it to English first.\n",
    "Then analyze it.\n\n",
    "Return ONLY valid JSON. Do not use markdown fences. Do not add explanations.\n\n",
    "Return a compact JSON object with these keys:\n",
    "- overall_tone\n",
    "- role_focus\n",
    "- academic_focus\n",
    "- summary_focus\n",
    "- priority_keywords\n",
    "- de_emphasize\n\n",
    "Rules:\n",
    "- Keep the output grounded in the provided text\n",
    "- Do not invent requirements\n",
    "- Make the brief useful for tailoring CV sections\n",
    "- Keep fields short and practical\n",
    "- priority_keywords must be an array of strings\n",
    "- de_emphasize must be an array of strings\n\n",
    "Job description:\n",
    ifelse(is.null(job_description), "", job_description),
    "\n\nRecruiter message:\n",
    ifelse(is.null(recruiter_message), "", recruiter_message)
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

  if (is.null(parsed)) {
    message("Warning: could not parse tailoring brief JSON. Using default brief.")
    return(default_brief)
  }

  list(
    overall_tone = if (!is.null(parsed$overall_tone)) parsed$overall_tone else default_brief$overall_tone,
    role_focus = if (!is.null(parsed$role_focus)) parsed$role_focus else default_brief$role_focus,
    academic_focus = if (!is.null(parsed$academic_focus)) parsed$academic_focus else default_brief$academic_focus,
    summary_focus = if (!is.null(parsed$summary_focus)) parsed$summary_focus else default_brief$summary_focus,
    priority_keywords = if (!is.null(parsed$priority_keywords)) as.character(parsed$priority_keywords) else character(0),
    de_emphasize = if (!is.null(parsed$de_emphasize)) as.character(parsed$de_emphasize) else character(0)
  )
}


format_tailoring_brief <- function(tailoring_brief, section = c("roles", "academic", "summary")) {
  section <- match.arg(section)

  section_focus <- switch(
    section,
    roles = tailoring_brief$role_focus,
    academic = tailoring_brief$academic_focus,
    summary = tailoring_brief$summary_focus
  )

  keywords_text <- if (length(tailoring_brief$priority_keywords) > 0) {
    paste(tailoring_brief$priority_keywords, collapse = ", ")
  } else {
    "None"
  }

  deemphasize_text <- if (length(tailoring_brief$de_emphasize) > 0) {
    paste(tailoring_brief$de_emphasize, collapse = ", ")
  } else {
    "None"
  }

  paste0(
    "Tailoring brief:\n",
    "- Overall tone: ", tailoring_brief$overall_tone, "\n",
    "- Section focus: ", section_focus, "\n",
    "- Priority keywords: ", keywords_text, "\n",
    "- De-emphasize: ", deemphasize_text, "\n"
  )
}