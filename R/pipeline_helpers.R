resolve_md_path <- function(file_stem = NULL, folder) {
  if (is.null(file_stem) || !nzchar(file_stem)) {
    return(NULL)
  }

  candidate <- file.path(folder, paste0(file_stem, ".md"))

  if (file.exists(candidate)) {
    return(candidate)
  }

  NULL
}

read_optional_md <- function(path) {
  if (is.null(path)) {
    return(NULL)
  }

  readr::read_file(path)
}

make_pipeline_config <- function(
  file_stem = NULL,
  job_description_stem = file_stem,
  recruiter_message_stem = file_stem,
  optional_obs = NULL,
  role_variant = "base_rules",
  academic_variant = "results",
  role_max_bullets = 3,
  academic_max_bullets = 3,
  selected_role_ids = c(4, 3, 2),
  source_file = "data/cv_new_reworked.xlsx",
  workbook_path = "data/cv_main.xlsx",
  render_xlsx_path = "data/cv_render_new.xlsx",
  render_output_dir = "../renders",
  drive_sheet_folder = "mimic_tear/renders/sheets",
  input_file = "scripts/cv.rmd",
  pdf_mode = TRUE,
  prefix_master = "master",
  prefix_render_sheet = "rendered_sheet",
  prefix_render_cv_html = "rendered_html",
  prefix_render_cv_pdf = "rendered_pdf"
) {

  cv_id <- uuid::UUIDgenerate()

  stamp <- format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  job_description_path <- resolve_md_path(
    job_description_stem,
    "prompts/job_descriptions"
  )

  recruiter_message_path <- resolve_md_path(
    recruiter_message_stem,
    "prompts/recruiter_messages"
  )

  jd_text <- read_optional_md(job_description_path)

  recruiter_message <- read_optional_md(
    recruiter_message_path
  )

  job_string <- if (
    !is.null(file_stem) &&
    nzchar(trimws(file_stem))
  ) {

    if (
      !is.null(job_description_stem) &&
      nzchar(trimws(job_description_stem))
    ) {

      paste0(
        file_stem,
        "_",
        job_description_stem
      )

    } else {

      file_stem
    }

  } else if (
    !is.null(job_description_stem) &&
    nzchar(trimws(job_description_stem))
  ) {

    job_description_stem

  } else if (
    !is.null(recruiter_message_stem) &&
    nzchar(trimws(recruiter_message_stem))
  ) {

    recruiter_message_stem

  } else {

    "cv_render"
  }

  if (is.null(optional_obs)) {

    optional_obs <- if (
      !is.null(recruiter_message_path) &&
      is.null(job_description_path)
    ) {

      "recruiter_message"

    } else if (
      !is.null(job_description_path) &&
      is.null(recruiter_message_path)
    ) {

      "job_description"

    } else if (
      !is.null(job_description_path) &&
      !is.null(recruiter_message_path)
    ) {

      "job_and_recruiter"

    } else {

      "generic"
    }
  }

  dir.create(
    render_output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  html_out <- file.path(
    render_output_dir,
    glue::glue(
      "{job_string}_{optional_obs}_{cv_id}_{stamp}.html"
    )
  )

  pdf_out <- sub(
    "\\.html$",
    ".pdf",
    html_out
  )

  render_sheet_name <- glue::glue(
    "{job_string}_{optional_obs}_{cv_id}_{stamp}.xlsx"
  )

  list(
    job_description = jd_text,
    recruiter_message = recruiter_message,
    job_description_path = job_description_path,
    recruiter_message_path = recruiter_message_path,
    job_string = job_string,
    optional_obs = optional_obs,
    cv_id = cv_id,
    stamp = stamp,
    role_variant = role_variant,
    academic_variant = academic_variant,
    role_max_bullets = role_max_bullets,
    academic_max_bullets = academic_max_bullets,
    selected_role_ids = selected_role_ids,
    source_file = source_file,
    workbook_path = workbook_path,
    render_xlsx_path = render_xlsx_path,
    render_output_dir = render_output_dir,
    drive_sheet_folder = drive_sheet_folder,
    input_file = input_file,
    pdf_mode = pdf_mode,
    prefix_master = prefix_master,
    prefix_render_sheet = prefix_render_sheet,
    prefix_render_cv_html = prefix_render_cv_html,
    prefix_render_cv_pdf = prefix_render_cv_pdf,
    render_sheet_name = render_sheet_name,
    html_out = html_out,
    pdf_out = pdf_out
  )
}