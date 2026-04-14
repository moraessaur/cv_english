library(readr)
library(stringr)
library(glue)

read_md_file <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }

  text <- read_file(path)
  text <- str_trim(text)

  if (text == "") {
    stop("File is empty: ", path)
  }

  text
}

build_form_answer_prompt <- function(
  experience_text,
  job_description_text,
  question_text,
  prompt_template
) {
  glue(
    prompt_template,
    experience_text = experience_text,
    job_description_text = job_description_text,
    question_text = question_text,
    .open = "{{",
    .close = "}}"
  ) |> as.character()
}

generate_form_answer <- function(
  experience_path,
  job_description_path,
  question_path,
  prompt_template_path = "prompts/forms/answer_form_question.md",
  output_path = NULL,
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
) {
  experience_text <- read_md_file(experience_path)
  job_description_text <- read_md_file(job_description_path)
  question_text <- read_md_file(question_path)
  prompt_template <- read_md_file(prompt_template_path)

  prompt <- build_form_answer_prompt(
    experience_text = experience_text,
    job_description_text = job_description_text,
    question_text = question_text,
    prompt_template = prompt_template
  )

  answer <- call_openai_text(
    prompt = prompt,
    model = model,
    api_key = api_key
  )

  answer <- str_trim(answer)

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write_file(answer, output_path)
  }

  answer
}