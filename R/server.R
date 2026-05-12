library(shiny)

source("R/helpers.R")
source("R/run_cv_pipeline.R")

server <- function(input, output, session) {

  logs <- reactiveVal("Ready.")
  rendered_files <- reactiveVal(NULL)
  prompt_editor_status <- reactiveVal("Select a folder and file, or create a new .md file.")

  refresh_trigger <- reactiveVal(0)

  refresh_prompt_dropdowns <- function() {

    updateSelectInput(
      session,
      "job_description_stem",
      choices = get_stems("prompts/job_descriptions"),
      selected = input$job_description_stem
    )

    updateSelectInput(
      session,
      "recruiter_message_stem",
      choices = c(
        "None" = "",
        get_stems("prompts/recruiter_messages")
      ),
      selected = input$recruiter_message_stem
    )

    updateSelectInput(
      session,
      "role_variant",
      choices = get_stems("prompts/base"),
      selected = input$role_variant
    )

    updateSelectInput(
      session,
      "academic_variant",
      choices = get_stems("prompts/skills_stack"),
      selected = input$academic_variant
    )
  }

  observe({
    refresh_trigger()

    files <- get_md_files(input$prompt_folder)

    updateSelectInput(
      session,
      "prompt_file",
      choices = files,
      selected = if (length(files) > 0) files[[1]] else character(0)
    )
  })

  observeEvent(input$prompt_folder, {
    files <- get_md_files(input$prompt_folder)

    updateSelectInput(
      session,
      "prompt_file",
      choices = files,
      selected = if (length(files) > 0) files[[1]] else character(0)
    )

    updateTextAreaInput(
      session,
      "prompt_text",
      value = ""
    )

    updateTextInput(
      session,
      "new_prompt_name",
      value = ""
    )

    prompt_editor_status(
      paste("Current folder:", input$prompt_folder)
    )
  })

  observeEvent(input$load_prompt, {

    req(input$prompt_folder)
    req(input$prompt_file)

    path <- file.path(
      input$prompt_folder,
      input$prompt_file
    )

    txt <- read_md_file(path)

    updateTextAreaInput(
      session,
      "prompt_text",
      value = txt
    )

    updateTextInput(
      session,
      "new_prompt_name",
      value = tools::file_path_sans_ext(input$prompt_file)
    )

    prompt_editor_status(
      paste("Loaded:", path)
    )
  })

  observeEvent(input$save_prompt, {

    req(input$prompt_folder)

    filename <- safe_md_name(input$new_prompt_name)

    if (!nzchar(filename)) {
      if (is.null(input$prompt_file) || !nzchar(input$prompt_file)) {
        showNotification(
          "Choose an existing file or provide a new file name.",
          type = "error"
        )
        return()
      }

      filename <- input$prompt_file
    }

    if (!dir.exists(input$prompt_folder)) {
      dir.create(
        input$prompt_folder,
        recursive = TRUE
      )
    }

    path <- file.path(
      input$prompt_folder,
      filename
    )

    writeLines(
      input$prompt_text,
      con = path,
      useBytes = TRUE
    )

    refresh_trigger(refresh_trigger() + 1)
    refresh_prompt_dropdowns()

    showNotification(
      paste("Saved:", path),
      type = "message"
    )

    prompt_editor_status(
      paste("Saved:", path)
    )
  })

  observeEvent(input$clear_prompt, {

    updateTextAreaInput(
      session,
      "prompt_text",
      value = ""
    )

    updateTextInput(
      session,
      "new_prompt_name",
      value = ""
    )

    prompt_editor_status("Editor cleared.")
  })

  observeEvent(input$render_cv, {

    recruiter_message_stem <- if (
      input$recruiter_message_stem == ""
    ) {
      NULL
    } else {
      input$recruiter_message_stem
    }

    cfg <- make_pipeline_config(

      file_stem = input$file_stem,

      job_description_stem = input$job_description_stem,

      recruiter_message_stem = recruiter_message_stem,

      optional_obs = NULL,

      role_variant = input$role_variant,

      academic_variant = input$academic_variant,

      role_max_bullets = input$role_max_bullets,

      academic_max_bullets = input$academic_max_bullets,

      selected_role_ids = as.numeric(input$selected_role_ids),

      source_file = "data/cv_new_reworked.xlsx",

      workbook_path = "data/cv_main.xlsx",

      render_xlsx_path = "data/cv_render_new.xlsx",

      render_output_dir = "../renders",

      drive_sheet_folder = "mimic_tear/renders/sheets",

      input_file = "scripts/cv.rmd",

      pdf_mode = input$pdf_mode
    )

    result <- capture.output({

      rendered <- run_cv_pipeline(
        cfg = cfg,
        max_categories = input$max_categories,
        use_expertise = input$use_expertise
      )

      rendered_files(rendered)

    })

    logs(paste(result, collapse = "\n"))

  })

  output$logs <- renderText({
    logs()
  })

  output$prompt_editor_status <- renderText({
    prompt_editor_status()
  })

  output$render_links <- renderUI({

    files <- rendered_files()

    if (is.null(files)) {
      return(NULL)
    }

    tagList(

      tags$p(tags$b("HTML:")),

      tags$a(
        href = files$html_path,
        files$html_path,
        target = "_blank"
      ),

      if (!is.null(files$pdf_path)) {

        tagList(

          tags$p(tags$b("PDF:")),

          tags$a(
            href = files$pdf_path,
            files$pdf_path,
            target = "_blank"
          )

        )

      }

    )

  })

}