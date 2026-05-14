library(shiny)

source("R/helpers.R")
source("R/run_cv_pipeline.R")
source("R/run_form_pipeline_app.R")

server <- function(input, output, session) {

  logs <- reactiveVal("Ready.")
  rendered_files <- reactiveVal(NULL)

  form_logs <- reactiveVal("Ready.")
  form_result <- reactiveVal(NULL)
  form_saved_path <- reactiveVal(NULL)

  prompt_editor_status <- reactiveVal("Select a folder and file, or create a new .md file.")
  refresh_trigger <- reactiveVal(0)
  html_render_refresh <- reactiveVal(0)

  refresh_prompt_dropdowns <- function() {
    updateSelectInput(session, "job_description_stem",
      choices = get_stems("prompts/job_descriptions"),
      selected = input$job_description_stem
    )

    updateSelectInput(session, "recruiter_message_stem",
      choices = c("None" = "", get_stems("prompts/recruiter_messages")),
      selected = input$recruiter_message_stem
    )

    updateSelectInput(session, "role_variant",
      choices = get_stems("prompts/base"),
      selected = input$role_variant
    )

    updateSelectInput(session, "academic_variant",
      choices = get_stems("prompts/skills_stack"),
      selected = input$academic_variant
    )

    updateSelectInput(session, "form_job_description_stem",
      choices = get_stems("prompts/job_descriptions"),
      selected = input$form_job_description_stem
    )

    updateSelectInput(session, "form_recruiter_message_stem",
      choices = c("None" = "", get_stems("prompts/recruiter_messages")),
      selected = input$form_recruiter_message_stem
    )

    updateSelectInput(session, "form_question_stem",
      choices = get_stems("prompts/forms/questions"),
      selected = input$form_question_stem
    )
  }

  observeEvent(input$refresh_html_renders, {
    html_render_refresh(html_render_refresh() + 1)
  })

  observe({
    html_render_refresh()

    files <- get_recent_html_renders()

    updateSelectInput(
      session,
      "selected_html_render",
      choices = html_render_choices(files),
      selected = if (length(files) > 0) files[[1]] else character(0)
    )
  })

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

    updateTextAreaInput(session, "prompt_text", value = "")
    updateTextInput(session, "new_prompt_name", value = "")

    prompt_editor_status(paste("Current folder:", input$prompt_folder))
  })

  observeEvent(input$load_prompt, {
    req(input$prompt_folder)
    req(input$prompt_file)

    path <- file.path(input$prompt_folder, input$prompt_file)
    txt <- read_md_file(path)

    updateTextAreaInput(session, "prompt_text", value = txt)

    updateTextInput(
      session,
      "new_prompt_name",
      value = tools::file_path_sans_ext(input$prompt_file)
    )

    prompt_editor_status(paste("Loaded:", path))
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
      dir.create(input$prompt_folder, recursive = TRUE)
    }

    path <- file.path(input$prompt_folder, filename)

    writeLines(input$prompt_text, con = path, useBytes = TRUE)

    refresh_trigger(refresh_trigger() + 1)
    refresh_prompt_dropdowns()

    updateSelectInput(session, "prompt_file", selected = filename)

    showNotification(paste("Saved:", path), type = "message")
    prompt_editor_status(paste("Saved:", path))
  })

  observeEvent(input$delete_prompt, {
    req(input$prompt_folder)
    req(input$prompt_file)

    path <- file.path(input$prompt_folder, input$prompt_file)

    if (!file.exists(path)) {
      showNotification("Selected file does not exist.", type = "error")
      return()
    }

    ok <- file.remove(path)

    if (ok) {
      refresh_trigger(refresh_trigger() + 1)
      refresh_prompt_dropdowns()

      updateTextAreaInput(session, "prompt_text", value = "")
      updateTextInput(session, "new_prompt_name", value = "")

      showNotification(paste("Deleted:", path), type = "warning")
      prompt_editor_status(paste("Deleted:", path))
    } else {
      showNotification(paste("Could not delete:", path), type = "error")
    }
  })

  observeEvent(input$clear_prompt, {
    updateTextAreaInput(session, "prompt_text", value = "")
    updateTextInput(session, "new_prompt_name", value = "")
    prompt_editor_status("Editor cleared.")
  })

  observeEvent(input$render_cv, {
    recruiter_message_stem <- if (input$recruiter_message_stem == "") {
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
      selected_role_ids = as.numeric(unname(input$selected_role_ids)),
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
      html_render_refresh(html_render_refresh() + 1)
    })

    logs(paste(result, collapse = "\n"))
  })

  observeEvent(input$generate_form_preview, {

    form_recruiter_message_stem <- if (input$form_recruiter_message_stem == "") {
      NULL
    } else {
      input$form_recruiter_message_stem
    }

    form_saved_path(NULL)

    result <- capture.output({
      out <- run_form_pipeline_preview(
        mode = input$form_mode,
        file_stem = input$form_file_stem,
        job_description_stem = input$form_job_description_stem,
        recruiter_message_stem = form_recruiter_message_stem,
        question_stem = input$form_question_stem,
        question_text = input$form_question_text,
        additional_guidance = input$form_additional_guidance,
        selected_role_ids = as.numeric(unname(input$form_selected_role_ids)),
        source_file = "data/cv_main.xlsx",
        workbook_path = "data/cv_main.xlsx"
      )

      form_result(out)

      updateTextAreaInput(
        session,
        "form_output_preview",
        value = out$content
      )
    })

    form_logs(paste(result, collapse = "\n"))
  })

  observeEvent(input$save_form_output, {

    out <- form_result()

    if (is.null(out)) {
      showNotification("Generate a preview before saving.", type = "error")
      return()
    }

    preview_text <- input$form_output_preview

    if (is.null(preview_text) || !nzchar(trimws(preview_text))) {
      showNotification("Preview is empty. Nothing to save.", type = "error")
      return()
    }

    save_path <- save_form_preview_to_file(
      content = preview_text,
      file_stem = out$file_stem,
      mode = out$mode,
      output_dir = file.path("output/forms", out$file_stem)
    )

    form_saved_path(save_path)

    showNotification(paste("Saved:", save_path), type = "message")
  })

  output$logs <- renderText({
    logs()
  })

  output$form_logs <- renderText({
    form_logs()
  })

  output$prompt_editor_status <- renderText({
    prompt_editor_status()
  })

  output$form_output_path <- renderUI({
    path <- form_saved_path()

    if (is.null(path)) {
      return(tags$p("No file saved yet. Generate a preview, edit it if needed, then save."))
    }

    tagList(
      tags$p(tags$b("Saved file:")),
      tags$code(path)
    )
  })

  output$html_render_open_link <- renderUI({
    req(input$selected_html_render)

    web_path <- html_file_to_resource_path(input$selected_html_render)

    tagList(
      tags$p(tags$b("Selected file:")),
      tags$code(input$selected_html_render),
      tags$br(),
      tags$a(
        href = web_path,
        target = "_blank",
        "Open selected render in new tab"
      )
    )
  })

  output$html_render_preview <- renderUI({
    req(input$selected_html_render)

    web_path <- html_file_to_resource_path(input$selected_html_render)

    tags$iframe(
      src = web_path,
      style = "width:100%; height:850px; border:1px solid #ccc; background:white;"
    )
  })

  output$render_links <- renderUI({
    files <- rendered_files()

    if (is.null(files)) return(NULL)

    tagList(
      tags$p(tags$b("HTML:")),
      tags$code(files$html_path),

      tags$p(tags$b("PDF:")),
      if (!is.null(files$pdf_path)) {
        tags$code(files$pdf_path)
      } else {
        tags$span("PDF not generated.")
      }
    )
  })
}