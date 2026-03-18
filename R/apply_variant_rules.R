`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

apply_variant_rules <- function(payload, variant) {
  filtered_payload <- payload

  max_entries <- variant$filters$max_entries_per_section %||% NULL
  include_section_ids <- variant$filters$include_section_ids %||% names(payload$sections)

  filtered_payload$sections <- filtered_payload$sections[include_section_ids]

  for (section_name in names(filtered_payload$sections)) {
    section_df <- filtered_payload$sections[[section_name]]

    if (!is.null(max_entries) && !is.null(section_df) && nrow(section_df) > max_entries) {
      section_df <- section_df |>
        dplyr::slice_head(n = max_entries)
    }

    filtered_payload$sections[[section_name]] <- section_df
  }

  filtered_payload
}