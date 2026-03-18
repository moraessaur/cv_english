`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

build_cv_payload <- function(cv_data, variant) {
  get_text_block <- function(label) {
    text_block <- cv_data$text_blocks |>
      dplyr::filter(loc == label) |>
      dplyr::pull(text)

    if (length(text_block) == 0) {
      return(NULL)
    }

    text_block[[1]]
  }

  get_section_entries <- function(section_id) {
    cv_data$entries_data |>
      dplyr::filter(section == section_id)
  }

  payload <- list(
    meta = list(
      variant_name = variant$name %||% "unnamed_variant",
      language = variant$language %||% "en",
      pdf_mode = cv_data$pdf_mode
    ),
    intro = get_text_block("intro"),
    contact_info = cv_data$contact_info,
    skills = cv_data$skills,
    sections = list()
  )

  section_ids <- variant$sections %||% character(0)
  entry_sections <- setdiff(section_ids, "intro")

  for (section_id in entry_sections) {
    payload$sections[[section_id]] <- get_section_entries(section_id)
  }

  payload
}