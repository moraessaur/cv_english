load_variant <- function(path) {
  if (!file.exists(path)) {
    stop("Variant file not found: ", path)
  }

  yaml::read_yaml(path)
}