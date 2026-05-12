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

      numericInput(
        "role_max_bullets",
        "Role max bullets",
        value = 3,
        min = 1,
        max = 10
      ),

      numericInput(
        "academic_max_bullets",
        "Academic max bullets",
        value = 4,
        min = 1,
        max = 10
      ),

      sliderInput(
        "max_categories",
        "Max skill categories",
        min = 1,
        max = 10,
        value = 3
      ),

      checkboxInput(
        "use_expertise",
        "Use expertise calibration",
        value = TRUE
      ),

      checkboxInput(
        "pdf_mode",
        "Generate PDF",
        value = TRUE
      ),

      checkboxGroupInput(
        "selected_role_ids",
        "Selected roles",
        choices = get_role_choices("data/cv_main.xlsx"),
        selected = c(4, 3, 1)
      ),

      actionButton(
        "render_cv",
        "Render CV"
      )
    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "CV run",

          h3("Logs"),
          verbatimTextOutput("logs"),

          h3("Generated files"),
          uiOutput("render_links")
        ),

        tabPanel(
          "Forms",

          h3("Application form generator"),

          selectInput(
            "form_mode",
            "Form mode",
            choices = c(
              "Answer one question" = "question",
              "Generate role summaries" = "summaries"
            ),
            selected = "question"
          ),

          textInput(
            "form_file_stem",
            "Form file stem",
            value = "riachuelo"
          ),

          selectInput(
            "form_job_description_stem",
            "Job description",
            choices = get_stems("prompts/job_descriptions"),
            selected = "riachuelo"
          ),

          selectInput(
            "form_recruiter_message_stem",
            "Recruiter message",
            choices = c(
              "None" = "",
              get_stems("prompts/recruiter_messages")
            ),
            selected = ""
          ),

          selectInput(
            "form_question_stem",
            "Question file",
            choices = get_stems("prompts/forms/questions"),
            selected = NULL
          ),

          textAreaInput(
            "form_question_text",
            "Or paste form question directly",
            value = "",
            rows = 8,
            width = "100%",
            placeholder = "Paste the application form question here. If filled, this overrides the selected question file."
          ),

          textAreaInput(
            "form_additional_guidance",
            "Additional answer guidance",
            value = "",
            rows = 6,
            width = "100%",
            placeholder = "Optional: tone, emphasis, style, seniority, brevity, technical focus, business focus, word limit, etc."
          ),

          checkboxGroupInput(
            "form_selected_role_ids",
            "Selected roles",
            choices = get_role_choices("data/cv_main.xlsx"),
            selected = c(4, 3, 2, 1)
          ),

          actionButton(
            "generate_form_preview",
            "Generate preview"
          ),

          actionButton(
            "save_form_output",
            "Save preview to file"
          ),

          hr(),

          h3("Form logs"),
          verbatimTextOutput("form_logs"),

          h3("Saved output"),
          uiOutput("form_output_path"),

          h3("Editable preview"),

          textAreaInput(
            "form_output_preview",
            "Generated preview",
            value = "",
            rows = 22,
            width = "100%"
          )
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
              "Skills stack variants" = "prompts/skills_stack",
              "Form question prompts" = "prompts/forms/questions",
              "Form system prompts" = "prompts/forms"
            ),
            selected = "prompts/job_descriptions"
          ),

          selectInput(
            "prompt_file",
            "Existing .md file",
            choices = NULL
          ),

          actionButton(
            "load_prompt",
            "Load selected file"
          ),

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

          actionButton(
            "save_prompt",
            "Save / overwrite .md"
          ),

          actionButton(
            "clear_prompt",
            "Clear editor"
          ),

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