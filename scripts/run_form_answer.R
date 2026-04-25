source("R/helpers_llm.R")
source("R/form_answers.R")
source("R/utils.R")

output_path <- "prompts/forms/outputs/why_this_role_answer.md"

answer <- generate_form_answer(
  experience_path = "prompts/forms/my_experience.md",
  job_description_path = "prompts/job_descriptions/biostatistician_roche.md",
  question_path = "prompts/forms/dummy_form_question.md",
  output_path = output_path
)

upload_markdown_to_drive(
  input_path = output_path,
  drive_folder = "mimic_tear/renders/forms"
)

cat(answer)