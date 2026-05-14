library(shiny)

if (!dir.exists("../renders")) {
  dir.create("../renders", recursive = TRUE, showWarnings = FALSE)
}

if (!dir.exists("renders")) {
  dir.create("renders", recursive = TRUE, showWarnings = FALSE)
}

addResourcePath("renders_parent", normalizePath("../renders", mustWork = FALSE))
addResourcePath("renders_local", normalizePath("renders", mustWork = FALSE))
addResourcePath("scripts_static", normalizePath("scripts", mustWork = FALSE))

source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)