backup_drive_file <- function(
  input_path,
  output_folder,
  extension,
  suffix_time_format = "%Y%m%d%H%M%S"
) {

  googledrive::drive_auth()

  file <- googledrive::drive_get(input_path)

  if (nrow(file) == 0) {
    stop("No file found at input_path: ", input_path)
  }

  if (nrow(file) > 1) {
    stop("More than one file found at input_path. Use a more specific path or ID.")
  }

  original_name <- file$name[[1]]

  base_name <- tools::file_path_sans_ext(original_name)

  stamp <- format(Sys.time(), suffix_time_format)
  run_id <- uuid::UUIDgenerate()

  backup_name <- paste0(
    base_name,
    "_",
    stamp,
    "_",
    run_id,
    ".",
    extension
  )

  backup <- googledrive::drive_cp(
    file = file,
    path = output_folder,
    name = backup_name
  )

  backup <- googledrive::drive_get(
    googledrive::as_id(backup$id)
  )

  tibble::tibble(
    input_path = input_path,
    output_folder = output_folder,
    original_name = original_name,
    backup_name = backup_name,
    backup_id = backup$id,
    backup_link = backup$drive_resource[[1]]$webViewLink
  )
}