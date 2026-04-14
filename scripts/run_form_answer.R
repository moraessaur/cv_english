source("R/helpers_llm.R")
source("R/form_answers.R")

answer <- generate_form_answer(
  experience_path = "prompts/forms/my_experience.md",
  job_description_path = "prompts/job_descriptions/biostatistician_roche.md",
  question_path = "prompts/forms/dummy_form_question.md",
  output_path = "prompts/forms/outputs/why_this_role_answer.md"
)

cat(answer)