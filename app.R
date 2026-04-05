# app.R — minimal Shiny GUI for the Data Quality & Validation Engine
# Run from this folder:  shiny::runApp()
# Or in RStudio: open this file and click "Run App"

library(shiny)
library(readr)

source("audit.R")
source("scoring.R")
source("cleaning.R")
source("report.R")

#' Capture console report as a single string (reuses generate_report).
report_as_text <- function(audit, score_obj, title) {
  paste(capture.output(generate_report(audit, score_obj, title)), collapse = "\n")
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: system-ui, Segoe UI, sans-serif; max-width: 960px; margin: 1rem auto; padding: 0 1rem; }
      h1 { font-size: 1.35rem; font-weight: 600; margin-bottom: 0.25rem; }
      .sub { color: #555; font-size: 0.9rem; margin-bottom: 1rem; }
      .panel { border: 1px solid #ddd; border-radius: 6px; padding: 1rem; margin-bottom: 1rem; background: #fafafa; }
      .score { font-size: 2rem; font-weight: 700; margin: 0.25rem 0; }
      .badge { display: inline-block; padding: 0.2rem 0.6rem; border-radius: 4px; font-size: 0.85rem; font-weight: 600; }
      .ok { background: #d4edda; color: #155724; }
      .warn { background: #fff3cd; color: #856404; }
      .bad { background: #f8d7da; color: #721c24; }
      pre.shiny-text-output { white-space: pre-wrap; font-size: 0.8rem; background: #fff; border: 1px solid #e0e0e0; padding: 0.75rem; border-radius: 4px; max-height: 320px; overflow-y: auto; width: 100%; }
      table { font-size: 0.85rem; }
    "))
  ),
  titlePanel(title = NULL),
  h1("Data quality check"),
  div(class = "sub", "Upload a CSV, run the audit, optionally clean and download."),

  fluidRow(
    column(
      width = 4,
      div(
        class = "panel",
        fileInput("upload", "1. Choose CSV", accept = c("text/csv", ".csv"), buttonLabel = "Browse…"),
        actionButton("go", "2. Run audit", class = "btn-primary btn-block", style = "margin-bottom:8px;"),
        actionButton("clean_btn", "3. Clean data", class = "btn-default btn-block", style = "margin-bottom:8px;"),
        uiOutput("download_ui"),
        helpText(style = "font-size:11px;margin-top:10px;", "Cleaning uses the same rules as cleaning.R (trim, impute, dedupe, IQR caps). Download appears after step 3.")
      )
    ),
    column(
      width = 8,
      div(class = "panel", uiOutput("score_header")),
      fluidRow(
        column(6, h4("Report — before"), verbatimTextOutput("report_before")),
        column(6, h4("Report — after cleaning"), verbatimTextOutput("report_after"))
      ),
      h4("Data preview (first 15 rows)"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    raw = NULL,
    cleaned = NULL,
    audit_before = NULL,
    ent_before = NULL,
    score_before = NULL,
    audit_after = NULL,
    ent_after = NULL,
    score_after = NULL
  )

  observeEvent(input$upload, {
    req(input$upload)
    rv$raw <- read_csv(input$upload$datapath, show_col_types = FALSE)
    rv$cleaned <- NULL
    rv$audit_before <- rv$ent_before <- rv$score_before <- NULL
    rv$audit_after <- rv$ent_after <- rv$score_after <- NULL
  })

  observeEvent(input$go, {
    req(rv$raw)
    rv$audit_before <- audit_data(rv$raw)
    rv$ent_before <- entropy_score(rv$raw)
    rv$score_before <- calculate_score(rv$audit_before, rv$ent_before, rv$raw)
    rv$cleaned <- NULL
    rv$audit_after <- rv$ent_after <- rv$score_after <- NULL
  })

  observeEvent(input$clean_btn, {
    req(rv$raw)
    rv$cleaned <- clean_data(rv$raw)
    rv$audit_after <- audit_data(rv$cleaned)
    rv$ent_after <- entropy_score(rv$cleaned)
    rv$score_after <- calculate_score(rv$audit_after, rv$ent_after, rv$cleaned)
  })

  output$score_header <- renderUI({
    req(rv$score_before)
    s <- rv$score_before
    cls <- if (s$score >= 80) "ok" else if (s$score >= 50) "warn" else "bad"
    tagList(
      div("Quality score (before cleaning)"),
      div(class = "score", sprintf("%.1f / 100", s$score)),
      span(class = paste("badge", cls), s$status),
      span(style = "margin-left:8px;color:#666;", paste0("Severity: ", s$severity))
    )
  })

  output$report_before <- renderText({
    req(rv$audit_before, rv$score_before)
    report_as_text(rv$audit_before, rv$score_before, "BEFORE CLEANING")
  })

  output$report_after <- renderText({
    if (is.null(rv$audit_after) || is.null(rv$score_after)) {
      return("Run “Clean data” to see the after report.")
    }
    report_as_text(rv$audit_after, rv$score_after, "AFTER CLEANING")
  })

  output$preview <- renderTable({
    req(rv$raw)
    head(rv$raw, 15)
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%", align = "l")

  output$download_ui <- renderUI({
    if (is.null(rv$cleaned)) {
      return(helpText(style = "font-size:12px;", "Run “Clean data” to enable download."))
    }
    downloadButton("download_clean", "4. Download cleaned CSV", class = "btn-block", style = "width:100%;")
  })

  output$download_clean <- downloadHandler(
    filename = function() {
      paste0("cleaned_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$cleaned)
      utils::write.csv(rv$cleaned, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
