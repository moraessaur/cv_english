library(tidyverse)
library(readxl)
library(readr)
library(gptr)
library(httr2)
library(jsonlite)
library(dplyr)
library(writexl)
library(glue)
library(uuid)

source('R/gpt_functions.R')
source('R/new_mvp_functions.R')
source('R/render.R')
source('R/job_rec_descriptions_functions.R')

job_description_path <- "job_descriptions/biostatistician_roche.md"
cv_id <- UUIDgenerate()

if (!is.null(job_description_path)) {
  job_string <- str_extract(job_description_path, "[^/]+(?=\\.md$)")
} else {
  job_string <- "cv_render"
}


jd_text <- readr::read_file(job_description_path)

#just borrowing language, text blocks and contact from here
source_file <- "data/cv_new_reworked.xlsx"  # <- your existing file

language_skills <- read_excel(source_file, sheet = "language_skills")
text_blocks     <- read_excel(source_file, sheet = "text_blocks")
contact_info    <- read_excel(source_file, sheet = "contact_info")

entries <- generate_cv_entries(
  workbook_path = "data/cv_main.xlsx",
  role_variant = "business_oriented",
  academic_variant = "balanced_academic",
  role_max_bullets = 3,
  academic_max_bullets = 3,
  selected_role_ids = c(4,3,2),
  job_description = jd_text,
  recruiter_message = NULL

)


entries <- entries |> 
  mutate(loc=institution) |> 
  mutate(institution=NA)

cv_render <- list(
  entries = entries,
  language_skills = language_skills,
  text_blocks = text_blocks,
  contact_info = contact_info
)

# 4. Save to Excel
write_xlsx(cv_render, "data/cv_render_new.xlsx")

library(googledrive)

drive_auth()

folder <- drive_get("llm_dump")

file <- drive_upload(
  media = "data/cv_render_new.xlsx",
  path = folder,
  name = "dead_render_public4.xlsx",
  type = 'spreadsheet'
)

# Make it public (anyone with link can view)
drive_share(file, role = "reader", type = "anyone")

# Get link
file <- drive_get(as_id(file$id))  # refresh metadata

link <- file$drive_resource[[1]]$webViewLink


render_cv_from_sheet(
  data_location = link,
  input_file = "scripts/cv.rmd",
  output_file = glue("../renders/cv_{job_string}_{cv_id}.html"),
  pdf_mode = TRUE
)