library(tidyverse)
library(readxl)
library(readr)
library(gptr)
library(httr2)
library(jsonlite)
library(dplyr)
library(writexl)

source('R/gpt_functions.R')
source('R/new_mvp_functions.R')
source('R/render.R')
source('R/job_rec_descriptions_functions.R')

#jd_text <- readr::read_file("job_descriptions/example_magazine_luiza.md")

source_file <- "data/cv_new_reworked.xlsx"  # <- your existing file

language_skills <- read_excel(source_file, sheet = "language_skills")
text_blocks     <- read_excel(source_file, sheet = "text_blocks")
contact_info    <- read_excel(source_file, sheet = "contact_info")

entries <- generate_cv_entries(
  workbook_path = "data/cv_main.xlsx",
  role_variant = "balanced",
  academic_variant = "balanced_academic",
  role_max_bullets = 3,
  academic_max_bullets = 3,
  selected_role_ids = c(4,3,2),
  job_description = NULL,
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
  output_file = "../renders/cv_balanced.html",
  pdf_mode = TRUE
)