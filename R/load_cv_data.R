load_cv_data <- function(data_location,
                         pdf_mode = FALSE,
                         sheet_is_publicly_readable = TRUE) {

  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(
    data_location,
    "docs\\.google\\.com"
  )

  if (is_google_sheets_location) {
    if (sheet_is_publicly_readable) {
      googlesheets4::gs4_deauth()
    } else {
      options(gargle_oauth_cache = ".secrets")
    }

    read_gsheet <- function(sheet_id) {
      googlesheets4::read_sheet(
        data_location,
        sheet = sheet_id,
        skip = 1,
        col_types = "c"
      )
    }

    cv$entries_data <- read_gsheet("entries")
    cv$skills <- read_gsheet("language_skills")
    cv$text_blocks <- read_gsheet("text_blocks")
    cv$contact_info <- read_gsheet("contact_info")

  } else {
    cv$entries_data <- readr::read_csv(
      file.path(data_location, "entries.csv"),
      skip = 1,
      show_col_types = FALSE
    )
    cv$skills <- readr::read_csv(
      file.path(data_location, "language_skills.csv"),
      skip = 1,
      show_col_types = FALSE
    )
    cv$text_blocks <- readr::read_csv(
      file.path(data_location, "text_blocks.csv"),
      skip = 1,
      show_col_types = FALSE
    )
    cv$contact_info <- readr::read_csv(
      file.path(data_location, "contact_info.csv"),
      skip = 1,
      show_col_types = FALSE
    )
  }

  extract_year <- function(dates) {
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- as.character(lubridate::year(Sys.Date()) + 10)
    date_year
  }

  parse_dates <- function(dates) {
    date_month <- stringr::str_extract(
      dates,
      "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})"
    )
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") |>
      lubridate::dmy()
  }

  cv$entries_data <- cv$entries_data |>
    tidyr::unite(
      tidyr::starts_with("description"),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) |>
    dplyr::mutate(
      description_bullets = dplyr::if_else(
        description_bullets != "",
        paste0("- ", description_bullets),
        ""
      ),
      start = dplyr::if_else(start == "NULL", NA_character_, start),
      end = dplyr::if_else(end == "NULL", NA_character_, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start & no_end  ~ "N/A",
        no_start & has_end ~ as.character(end),
        has_start & no_end ~ paste("Present -", start),
        TRUE ~ paste(end, "-", start)
      )
    ) |>
    dplyr::arrange(dplyr::desc(parse_dates(end))) |>
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ dplyr::if_else(is.na(.), "N/A", .)
      )
    )

  cv
}