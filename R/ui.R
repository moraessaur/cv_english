library(shiny)

source("R/helpers.R")

ui <- fluidPage(

  titlePanel("CV Generator"),

  sidebarLayout(

    sidebarPanel(

      h3("CV parameters"),

      textInput("file_stem", "File stem", value = "nestle"),

      selectInput(
        "job_description_stem",
        "Job description",
        choices = get_stems("prompts/job_descriptions"),
        selected = "nestle"
      ),

      selectInput(
        "recruiter_message_stem",
        "Recruiter message",
        choices = c("None" = "", get_stems("prompts/recruiter_messages")),
        selected = ""
      ),

      selectInput(
        "role_variant",
        "Role variant",
        choices = get_stems("prompts/base"),
        selected = "retail"
      ),

      selectInput(
        "academic_variant",
        "Academic variant",
        choices = get_stems("prompts/skills_stack"),
        selected = "proficiency_calibrated"
      ),

      numericInput("role_max_bullets", "Role max bullets", value = 3, min = 1, max = 10),

      numericInput("academic_max_bullets", "Academic max bullets", value = 4, min = 1, max = 10),

      sliderInput("max_categories", "Max skill categories", min = 1, max = 10, value = 3),

      checkboxInput("use_expertise", "Use expertise calibration", value = TRUE),

      checkboxInput("pdf_mode", "Generate PDF", value = TRUE),

      checkboxGroupInput(
        "selected_role_ids",
        "Selected role IDs",
        choices = c(1, 2, 3, 4),
        selected = c(4, 3, 1)
      ),

      actionButton("render_cv", "Render CV")
    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "Run",

          h3("Logs"),
          verbatimTextOutput("logs"),

          h3("Generated files"),
          uiOutput("render_links")
        ),

        tabPanel(
          "Prompt editor",

          h3("Markdown prompt editor"),

          selectInput(
            "prompt_folder",
            "Prompt folder",
            choices = c(
              "Job descriptions" = "prompts/job_descriptions",
              "Recruiter messages" = "prompts/recruiter_messages",
              "Role/base variants" = "prompts/base",
              "Skills stack variants" = "prompts/skills_stack"
            ),
            selected = "prompts/job_descriptions"
          ),

          selectInput(
            "prompt_file",
            "Existing .md file",
            choices = NULL
          ),

          actionButton("load_prompt", "Load selected file"),

          hr(),

          textInput(
            "new_prompt_name",
            "New file name without .md",
            value = ""
          ),

          textAreaInput(
            "prompt_text",
            "Markdown content",
            value = "",
            rows = 22,
            width = "100%"
          ),

          actionButton("save_prompt", "Save / overwrite .md"),

          actionButton("clear_prompt", "Clear editor"),

          actionButton(
            "delete_prompt",
            "Delete selected .md",
            class = "btn-danger"
          ),

          br(),
          br(),

          verbatimTextOutput("prompt_editor_status")
        )
      )
    )
  )
)