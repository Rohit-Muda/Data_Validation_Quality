# Data Quality & Validation Engine
# Run: shiny::runApp("app.R")
# Dependencies: shiny, readr, dplyr, stringr

library(shiny); library(readr); library(dplyr); library(stringr)

# ── Audit ────────────────────────────────────────────────────
audit_data <- function(df) {
  n <- nrow(df); p <- ncol(df)
  missing_col   <- colSums(is.na(df)) / n * 100
  missing_total <- sum(is.na(df)) / (n * p) * 100
  exact_dup <- sum(duplicated(df))
  
  outliers <- 0L
  for (col in names(df)[sapply(df, is.numeric)]) {
    x <- df[[col]]; q1 <- quantile(x,.25,na.rm=TRUE); q3 <- quantile(x,.75,na.rm=TRUE)
    outliers <- outliers + sum(!is.na(x) & (x < q1-1.5*(q3-q1) | x > q3+1.5*(q3-q1)))
  }
  
  email_invalid <- 0L
  if ("email" %in% names(df))
    email_invalid <- sum(!is.na(df$email) &
                           !grepl("^[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}$", df$email, perl=TRUE))
  
  phone_invalid <- 0L
  pc <- names(df)[grepl("^phone$|^mobile$", names(df), ignore.case=TRUE)][1]
  if (!is.na(pc)) {
    x <- as.character(df[[pc]])
    phone_invalid <- sum(!is.na(x) & x != "" &
                           !grepl("^\\+?[0-9][0-9\\s\\-\\(\\)]{7,}$", x, perl=TRUE))
  }
  
  list(missing_col=missing_col, missing_total=missing_total, exact_dup=exact_dup,
       outliers=outliers, invalid_email=email_invalid, invalid_phone=phone_invalid,
       n_rows=n, n_cols=p)
}

# ── Entropy ──────────────────────────────────────────────────
entropy_score <- function(df) {
  chars <- df[sapply(df, is.character)]
  if (ncol(chars) == 0) return(1)
  mean(vapply(chars, function(x) {
    x <- x[!is.na(x)]; if (!length(x)) return(1)
    tab <- table(x); p <- as.numeric(tab)/length(x); p <- p[p>0]; k <- length(tab)
    if (k <= 1) return(0); -sum(p*log2(p))/log2(k)
  }, numeric(1)))
}

# ── Score ────────────────────────────────────────────────────
calculate_score <- function(audit, ent, df) {
  n <- nrow(df); cells <- n * ncol(df)
  pm <- min(40, audit$missing_total * 1.2)
  pd <- min(35, audit$exact_dup / max(n,1) * 100)
  po <- min(30, audit$outliers / max(cells,1) * 100 * 1.2)
  pf <- min(20, (audit$invalid_email + audit$invalid_phone) / max(n,1) * 100 * 0.4)
  pe <- min(10, (1 - ent) * 100 * 0.10)
  score <- round(max(0, min(100, 100 - pm - pd - po - pf - pe)), 1)
  list(score=score, entropy=round(ent,3),
       status=if(score>=80)"Approved" else if(score>=50)"Needs Improvement" else "Rejected",
       pen=c(Missing=round(pm,1), Duplicates=round(pd,1), Outliers=round(po,1),
             Format=round(pf,1), Entropy=round(pe,1)))
}

# ── Clean ────────────────────────────────────────────────────
clean_data <- function(df) {
  df <- df %>% mutate(across(where(is.character), ~str_to_lower(str_trim(.))))
  for (col in names(df)) {
    if (all(is.na(df[[col]]))) next
    if (is.numeric(df[[col]])) df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm=TRUE)
    else df[[col]][is.na(df[[col]])] <- names(sort(table(df[[col]]),decreasing=TRUE))[1]
  }
  if ("email" %in% names(df)) {
    pat <- "^[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}$"
    ok <- grepl(pat, df$email, perl=TRUE)
    if (any(ok)) df$email[!ok] <- names(sort(table(df$email[ok]),decreasing=TRUE))[1]
  }
  df <- distinct(df)
  for (col in names(df)[sapply(df, is.numeric)]) {
    x <- df[[col]]; q1 <- quantile(x,.25,na.rm=TRUE); q3 <- quantile(x,.75,na.rm=TRUE)
    df[[col]] <- pmin(pmax(x, q1-1.5*(q3-q1)), q3+1.5*(q3-q1))
  }
  df
}

# ── CSS ──────────────────────────────────────────────────────
css <- "
  body { font-family:'Inter',system-ui,sans-serif; background:#f5f5f5; color:#1a1a1a; font-size:14px; margin:0; }
  .header { background:#fff; border-bottom:1px solid #e0e0e0; padding:16px 32px; margin-bottom:24px; }
  .header h1 { font-size:1.1rem; font-weight:600; margin:0; }
  .header p  { font-size:0.8rem; color:#999; margin:2px 0 0; }
  .wrap { max-width:1200px; margin:0 auto; padding:0 24px 48px; }
  .card { background:#fff; border:1px solid #e0e0e0; border-radius:6px; padding:20px; margin-bottom:16px; }
  .card-title { font-size:0.68rem; font-weight:600; text-transform:uppercase; letter-spacing:.08em; color:#aaa; margin-bottom:14px; }
  .step { font-size:0.68rem; font-weight:600; text-transform:uppercase; letter-spacing:.07em; color:#bbb; margin:14px 0 5px; }
  .step:first-of-type { margin-top:0; }
  .btn-primary { background:#111 !important; color:#fff !important; border:none !important;
    border-radius:5px !important; font-size:0.85rem !important; font-weight:500 !important;
    padding:9px 16px !important; width:100%; margin-bottom:8px; }
  .btn-primary:hover { background:#333 !important; }
  .btn-success { background:#fff !important; color:#111 !important; border:1px solid #ccc !important;
    border-radius:5px !important; font-size:0.85rem !important; font-weight:500 !important;
    padding:9px 16px !important; width:100%; margin-bottom:8px; }
  .btn-dl { background:#fff !important; border:1px solid #e0e0e0 !important; border-radius:5px !important;
    color:#666 !important; font-size:0.82rem !important; padding:8px 14px !important; width:100%; }
  .form-control, input[type=file] { border:1px solid #ddd !important; border-radius:5px !important; font-size:0.85rem !important; }
  label { font-size:0.8rem !important; color:#777 !important; margin-bottom:4px !important; }
  .score-val { font-size:2.8rem; font-weight:700; line-height:1; letter-spacing:-.03em; }
  .score-val.red{color:#dc2626;} .score-val.amber{color:#d97706;} .score-val.green{color:#16a34a;}
  .pill { display:inline-block; padding:3px 10px; border-radius:99px; font-size:0.72rem; font-weight:600; margin-top:8px; margin-right:4px; }
  .pill.red{background:#fee2e2;color:#dc2626;} .pill.amber{background:#fef3c7;color:#d97706;} .pill.green{background:#dcfce7;color:#16a34a;}
  .two-col { display:grid; grid-template-columns:1fr 1fr; gap:16px; margin-bottom:16px; }
  .metric { display:flex; justify-content:space-between; padding:7px 0; border-bottom:1px solid #f0f0f0; font-size:0.87rem; }
  .metric:last-child { border-bottom:none; }
  .metric-label { color:#777; }
  .metric-val { font-weight:600; font-family:monospace; font-size:0.87rem; }
  .metric-val.red{color:#dc2626;} .metric-val.amber{color:#d97706;} .metric-val.green{color:#16a34a;}
  .bar-row { margin-bottom:9px; }
  .bar-label { display:flex; justify-content:space-between; font-size:0.75rem; color:#aaa; margin-bottom:3px; }
  .bar-track { height:4px; background:#f0f0f0; border-radius:99px; overflow:hidden; }
  .bar-fill  { height:100%; background:#111; border-radius:99px; }
  .tbl { width:100%; border-collapse:collapse; font-size:0.83rem; }
  .tbl th { text-align:left; padding:8px 12px; font-size:0.7rem; font-weight:600; text-transform:uppercase;
            letter-spacing:.05em; color:#aaa; border-bottom:1px solid #e0e0e0; }
  .tbl td { padding:7px 12px; color:#333; border-bottom:1px solid #f5f5f5; }
  .tbl tr:hover td { background:#fafafa; }
  .note { font-size:0.75rem; color:#ccc; margin-top:12px; line-height:1.6; }
  .empty { text-align:center; padding:36px; color:#ccc; font-size:0.88rem; }
  .shiny-input-container { margin-bottom:0 !important; }
  .form-group { margin-bottom:0 !important; }
"

# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    tags$style(HTML(css))
  ),
  div(class="header", tags$h1("Data Quality Engine"), tags$p("Audit  —  Score  —  Clean")),
  div(class="wrap",
      fluidRow(
        column(3,
               div(class="card",
                   div(class="step", "1. Upload"),
                   fileInput("upload", NULL, accept=c("text/csv",".csv"), buttonLabel="Choose CSV"),
                   div(class="step", "2. Audit"),
                   actionButton("go", "Run Audit", class="btn-primary"),
                   div(class="step", "3. Clean"),
                   actionButton("clean_btn", "Clean Data", class="btn-success"),
                   div(class="step", "4. Export"),
                   uiOutput("dl_ui"),
                   div(class="note", "Imputes missing values, removes duplicates, caps outliers, normalises text.")
               )
        ),
        column(9,
               uiOutput("score_ui"),
               div(class="two-col",
                   div(class="card", div(class="card-title", "Before Cleaning"), uiOutput("m_before")),
                   div(class="card", div(class="card-title", "After Cleaning"),  uiOutput("m_after"))
               ),
               div(class="card", div(class="card-title", "Data Preview"), uiOutput("preview_ui"))
        )
      )
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(raw=NULL, cleaned=NULL, ab=NULL, eb=NULL, sb=NULL, aa=NULL, ea=NULL, sa=NULL)
  
  observeEvent(input$upload, {
    req(input$upload)
    rv$raw <- read_csv(input$upload$datapath, show_col_types=FALSE)
    rv$cleaned <- rv$ab <- rv$eb <- rv$sb <- rv$aa <- rv$ea <- rv$sa <- NULL
  })
  
  observeEvent(input$go, {
    req(rv$raw)
    rv$ab <- audit_data(rv$raw); rv$eb <- entropy_score(rv$raw)
    rv$sb <- calculate_score(rv$ab, rv$eb, rv$raw)
    rv$cleaned <- rv$aa <- rv$ea <- rv$sa <- NULL
  })
  
  observeEvent(input$clean_btn, {
    req(rv$raw)
    if (is.null(rv$ab)) {
      rv$ab <- audit_data(rv$raw); rv$eb <- entropy_score(rv$raw)
      rv$sb <- calculate_score(rv$ab, rv$eb, rv$raw)
    }
    rv$cleaned <- clean_data(rv$raw)
    rv$aa <- audit_data(rv$cleaned); rv$ea <- entropy_score(rv$cleaned)
    rv$sa <- calculate_score(rv$aa, rv$ea, rv$cleaned)
  })
  
  sc   <- function(s) if (s>=80) "green" else if (s>=50) "amber" else "red"
  mcls <- function(v, w=5) if (v==0) "green" else if (v<w) "amber" else "red"
  
  output$score_ui <- renderUI({
    if (is.null(rv$sb))
      return(div(class="card", div(class="empty", "Upload a CSV and click Run Audit to begin.")))
    s <- rv$sb; s2 <- rv$sa
    div(class="card",
        div(class="card-title", "Quality Score"),
        div(style="display:flex;gap:48px;align-items:flex-start;flex-wrap:wrap;",
            div(
              div(class="card-title", "Before"),
              div(class=paste("score-val",sc(s$score)), sprintf("%.1f",s$score)),
              span(class=paste("pill",sc(s$score)), s$status),
              if (!is.null(s2)) tagList(
                div(class="card-title", style="margin-top:20px;", "After"),
                div(class=paste("score-val",sc(s2$score)), sprintf("%.1f",s2$score)),
                span(class=paste("pill",sc(s2$score)), s2$status))
            ),
            div(style="flex:1;min-width:200px;padding-top:2px;",
                lapply(names(s$pen), function(nm) {
                  caps <- c(Missing=40,Duplicates=35,Outliers=30,Format=20,Entropy=10)
                  pct  <- round(s$pen[nm] / caps[nm] * 100)
                  div(class="bar-row",
                      div(class="bar-label", span(nm), span(sprintf("-%0.1f pts",s$pen[nm]))),
                      div(class="bar-track", div(class="bar-fill", style=sprintf("width:%d%%",pct))))
                })
            )
        )
    )
  })
  
  mk_metrics <- function(a, s) {
    if (is.null(a)) return(div(class="empty", "—"))
    items <- list(
      list("Score",          sprintf("%.1f / 100",s$score), sc(s$score)),
      list("Rows",           as.character(a$n_rows),        "green"),
      list("Columns",        as.character(a$n_cols),        "green"),
      list("Missing (%)",    sprintf("%.2f%%",a$missing_total), mcls(a$missing_total)),
      list("Duplicates",     as.character(a$exact_dup),     mcls(a$exact_dup,3)),
      list("Outliers",       as.character(a$outliers),      mcls(a$outliers,10)),
      list("Invalid emails", as.character(a$invalid_email), mcls(a$invalid_email,3)),
      list("Invalid phones", as.character(a$invalid_phone), mcls(a$invalid_phone,3)),
      list("Entropy",        sprintf("%.3f",s$entropy),     if(s$entropy>=.7)"green" else if(s$entropy>=.4)"amber" else "red"),
      list("Status",         s$status,                      sc(s$score))
    )
    tagList(lapply(items, function(r)
      div(class="metric", span(class="metric-label",r[[1]]), span(class=paste("metric-val",r[[3]]),r[[2]]))))
  }
  
  output$m_before  <- renderUI(mk_metrics(rv$ab, rv$sb))
  output$m_after   <- renderUI(mk_metrics(rv$aa, rv$sa))
  
  output$preview_ui <- renderUI({
    if (is.null(rv$raw)) return(div(class="empty","No file loaded."))
    tbl <- head(if (!is.null(rv$cleaned)) rv$cleaned else rv$raw, 15)
    tags$table(class="tbl",
               tags$thead(tags$tr(lapply(names(tbl), tags$th))),
               tags$tbody(lapply(seq_len(nrow(tbl)), function(i)
                 tags$tr(lapply(names(tbl), function(n) tags$td(as.character(tbl[i,n,drop=TRUE])))))))
  })
  
  output$dl_ui <- renderUI({
    if (is.null(rv$cleaned))
      return(tags$button("Download CSV", class="btn-dl", disabled=NA, style="opacity:.35;cursor:default;"))
    downloadButton("dl", "Download Cleaned CSV", class="btn-dl")
  })
  output$dl <- downloadHandler(
    filename = function() paste0("cleaned_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(rv$cleaned, f, row.names=FALSE)
  )
}
shinyApp(ui, server)
