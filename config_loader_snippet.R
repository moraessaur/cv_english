# Add this near the top of scripts/run_cv_pipeline.R
library(jsonlite)

config_path <- Sys.getenv("CV_PIPELINE_CONFIG", unset = "configs/cv_pipeline_config.json")

if (!file.exists(config_path)) {
  stop("Config file not found: ", config_path)
}

raw_config <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
p <- raw_config$parameters

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

cfg <- make_pipeline_config(
  file_stem = p$file_stem,
  job_description_stem = p$job_description_stem %||% NULL,
  recruiter_message_stem = p$recruiter_message_stem %||% NULL,
  optional_obs = p$optional_obs %||% NULL,
  role_variant = p$role_variant,
  academic_variant = p$academic_variant,
  role_max_bullets = p$role_max_bullets,
  academic_max_bullets = p$academic_max_bullets,
  selected_role_ids = unlist(p$selected_role_ids),
  source_file = p$source_file,
  workbook_path = p$workbook_path,
  render_xlsx_path = p$render_xlsx_path,
  render_output_dir = p$render_output_dir,
  drive_sheet_folder = p$drive_sheet_folder,
  input_file = p$input_file,
  pdf_mode = p$pdf_mode
)

# Then replace hardcoded values later in the script:
# - drive_download(file = as_id(p$master_drive_file_id), ...)
# - generate_cv_entries(max_categories = p$max_categories, use_expertise = p$use_expertise)
# - HTML upload path = p$html_drive_folder
# - PDF upload path = p$pdf_drive_folder
