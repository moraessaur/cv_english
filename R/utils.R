library(googledrive)
library(tidyverse)
library(readxl)
library(glue)
library(uuid)
library(tools)
library(googlesheets4)
library(purrr)

save_spreadsheet <- function(x, folder, prefix = "master") {
  drive_auth()
  gs4_auth()

  if (
    !is.character(x) ||
      length(x) != 1 ||
      !grepl("\\.xlsx$", x, ignore.case = TRUE)
  ) {
    stop("`x` must be a single path to an .xlsx file.")
  }

  if (!file.exists(x)) {
    stop("File does not exist: ", x)
  }

  # Read all sheets from the local Excel file
  sheet_names <- excel_sheets(x)
  all_data <- set_names(
    map(sheet_names, ~ read_excel(x, sheet = .x)),
    sheet_names
  )

  # Timestamped versioned name
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- paste0(prefix, "_", stamp)

  # Resolve target Drive folder
  folder_dribble <- if (inherits(folder, "dribble")) {
    folder
  } else {
    drive_get(folder)
  }

  if (nrow(folder_dribble) != 1) {
    stop("Could not uniquely identify target folder.")
  }

  # Create a blank Google Sheet in that folder
  ss <- drive_create(
    name = file_name,
    path = folder_dribble,
    type = "spreadsheet"
  )

  # Write first sheet into the default first worksheet
  first_name <- sheet_names[[1]]
  sheet_write(
    data = all_data[[first_name]],
    ss = ss,
    sheet = first_name
  )

  # Write remaining sheets as additional tabs
  if (length(sheet_names) > 1) {
    walk(sheet_names[-1], function(snm) {
      sheet_write(
        data = all_data[[snm]],
        ss = ss,
        sheet = snm
      )
    })

    sheets <- sheet_names(ss)

    if ("Sheet1" %in% sheets) {
      sheet_delete(ss, "Sheet1")
    }
  }

  # Optional: make public
  # drive_share(ss, role = "reader", type = "anyone")

  # Return useful metadata
  list(
    file = ss,
    file_id = as.character(ss$id),
    name = file_name,
    url = paste0("https://docs.google.com/spreadsheets/d/", ss$id),
    timestamp = stamp
  )
}

library(pagedown)
library(googledrive)
library(glue)
library(tools)

library(pagedown)
library(googledrive)
library(tools)

convert_html_to_pdf_and_upload <- function(
  local_render_dir = "renders",
  input_path = NULL,
  drive_folder = NULL,
  suffix = NULL,
  add_timestamp = FALSE,
  single_page = FALSE,
  overwrite_local_pdf = FALSE,
  upload_to_drive = FALSE,
  drive_share_public = FALSE
) {
  if (upload_to_drive) {
    drive_auth()
  }

  if (!dir.exists(local_render_dir)) {
    stop("Local render directory does not exist: ", local_render_dir)
  }

  # Decide which HTML files to process
  if (!is.null(input_path)) {
    if (!file.exists(input_path)) {
      stop("Specified input file does not exist: ", input_path)
    }
    if (!grepl("\\.html$", input_path, ignore.case = TRUE)) {
      stop("Specified input file must be an .html file.")
    }
    html_files <- normalizePath(input_path)
  } else {
    html_files <- list.files(
      path = local_render_dir,
      pattern = "\\.html$",
      full.names = TRUE
    )

    if (length(html_files) == 0) {
      message("No HTML files found in: ", local_render_dir)
      return(invisible(NULL))
    }

    html_files <- normalizePath(html_files)
  }

  # Resolve drive folder if needed
  folder_dribble <- NULL
  if (upload_to_drive) {
    if (is.null(drive_folder)) {
      stop("If upload_to_drive = TRUE, you must provide drive_folder.")
    }

    folder_dribble <- if (inherits(drive_folder, "dribble")) {
      drive_folder
    } else {
      drive_get(drive_folder)
    }

    if (nrow(folder_dribble) != 1) {
      stop("Could not uniquely identify target Google Drive folder.")
    }
  }

  timestamp_text <- if (add_timestamp) format(Sys.time(), "%Y%m%d_%H%M%S") else NULL
  single_page_text <- if (single_page) "single_page" else NULL

  build_pdf_name <- function(html_file) {
    base <- file_path_sans_ext(basename(html_file))

    extra_parts <- c(
      suffix,
      single_page_text,
      timestamp_text
    )
    extra_parts <- extra_parts[!is.na(extra_parts) & nzchar(extra_parts)]

    if (length(extra_parts) > 0) {
      paste0(base, "_", paste(extra_parts, collapse = "_"), ".pdf")
    } else {
      paste0(base, ".pdf")
    }
  }

  results <- vector("list", length(html_files))

  for (i in seq_along(html_files)) {
    html_file <- html_files[[i]]
    pdf_file <- file.path(dirname(html_file), build_pdf_name(html_file))

    if (file.exists(pdf_file) && !overwrite_local_pdf) {
      message("PDF already exists, skipping conversion: ", pdf_file)
    } else {
      message("Converting: ", html_file)

      # Note:
      # single_page is only reflected in the filename here.
      # Actual single-page behavior depends on the HTML/CSS you rendered.
      chrome_print(
        input = html_file,
        output = normalizePath(pdf_file, mustWork = FALSE)
      )
    }

    drive_file <- NULL

    if (upload_to_drive) {
      message("Uploading PDF to Drive: ", pdf_file)
      drive_file <- drive_upload(
        media = pdf_file,
        path = folder_dribble,
        name = basename(pdf_file)
      )

      if (drive_share_public) {
        drive_share(drive_file, role = "reader", type = "anyone")
      }
    }

    results[[i]] <- list(
      html = html_file,
      pdf = pdf_file,
      drive_file = drive_file
    )
  }

  invisible(results)
}


upload_markdown_to_drive <- function(
  input_path,
  drive_folder,
  add_uuid = TRUE,
  add_timestamp = TRUE,
  overwrite = FALSE,
  drive_share_public = FALSE
) {
  drive_auth()

  if (!file.exists(input_path)) {
    stop("File does not exist: ", input_path)
  }

  if (!grepl("\\.md$", input_path, ignore.case = TRUE)) {
    stop("`input_path` must point to a .md file.")
  }

  folder_dribble <- if (inherits(drive_folder, "dribble")) {
    drive_folder
  } else {
    drive_get(drive_folder)
  }

  if (nrow(folder_dribble) != 1) {
    stop("Could not uniquely identify target Google Drive folder.")
  }

  base <- tools::file_path_sans_ext(basename(input_path))
  ext <- tools::file_ext(input_path)

  extra_parts <- c(
    if (add_uuid) uuid::UUIDgenerate() else NULL,
    if (add_timestamp) format(Sys.time(), "%Y%m%d_%H%M%S") else NULL
  )
  extra_parts <- extra_parts[!is.na(extra_parts) & nzchar(extra_parts)]

  drive_name <- if (length(extra_parts) > 0) {
    glue::glue("{base}_{paste(extra_parts, collapse = '_')}.{ext}")
  } else {
    glue::glue("{base}.{ext}")
  }

  drive_file <- drive_upload(
    media = input_path,
    path = folder_dribble,
    name = drive_name,
    overwrite = overwrite
  )

  if (drive_share_public) {
    drive_share(drive_file, role = "reader", type = "anyone")
  }

  list(
    local_path = input_path,
    drive_file = drive_file,
    drive_name = drive_name
  )
}