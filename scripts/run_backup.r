library(googledrive)
library(uuid)
library(tibble)

source("R/drive_helpers.R")

backup <- backup_drive_file(
  input_path = "main",
  output_folder = "mimic_tear/backups",
  extension = "xlsx"
)

print(backup)