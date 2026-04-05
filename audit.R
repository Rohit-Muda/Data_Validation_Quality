# audit.R — quick quality checks (easy to walk through in a demo)

library(dplyr)
library(stringdist)
library(stringr)

#' Run all checks; returns a flat list the scorer and report can use.
audit_data <- function(df) {
  n <- nrow(df)
  p <- ncol(df)

  # 1) Missing: per column + share of all cells
  missing_col <- colSums(is.na(df)) / n * 100
  missing_total <- sum(is.na(df)) / (n * p) * 100

  # 2) Exact duplicate rows
  exact_dup <- sum(duplicated(df))

  # 3) Fuzzy duplicates (tiny demo): count pairs in the FIRST character column
  #    with Levenshtein distance <= 2. Good enough to illustrate "near duplicates".
  fuzzy_dup <- 0
  char_cols <- df %>% dplyr::select(where(is.character))
  if (ncol(char_cols) > 0 && n >= 2) {
    v <- char_cols[[1]]
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (!is.na(v[i]) && !is.na(v[j]) && stringdist(v[i], v[j], method = "lv") <= 2) {
          fuzzy_dup <- fuzzy_dup + 1
        }
      }
    }
  }

  # Optional: cluster unique strings (shows stringdistmatrix + hclust in one place)
  fuzzy_clusters <- 0
  if (ncol(char_cols) > 0) {
    u <- unique(stats::na.omit(char_cols[[1]]))
    if (length(u) >= 2 && length(u) <= 200) {
      m <- stringdistmatrix(u, u, method = "lv")
      cl <- cutree(hclust(as.dist(m)), h = 3)
      fuzzy_clusters <- sum(table(cl) > 1) # how many groups have more than one spelling
    }
  }

  # 4) Outliers: classic IQR rule on numeric columns
  outliers <- 0
  num_cols <- df %>% dplyr::select(where(is.numeric))
  for (col in names(num_cols)) {
    x <- df[[col]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    low <- q1 - 1.5 * iqr
    high <- q3 + 1.5 * iqr
    outliers <- outliers + sum(!is.na(x) & (x < low | x > high))
  }

  # 5) Schema (simple): store column classes — handy when explaining "expectations"
  schema <- data.frame(
    column = names(df),
    r_type = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
    stringsAsFactors = FALSE
  )

  # 6) Regex rules: email + optional phone column
  email_invalid <- 0
  if ("email" %in% names(df)) {
    pat <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
    email_invalid <- sum(!is.na(df$email) & !grepl(pat, df$email, perl = TRUE))
  }
  phone_invalid <- 0
  phone_col <- names(df)[grepl("^phone$|^mobile$", names(df), ignore.case = TRUE)][1]
  if (!is.na(phone_col)) {
    pat <- "^\\+?[0-9][0-9\\s\\-\\(\\)]{7,}$"
    x <- df[[phone_col]]
    phone_invalid <- sum(!is.na(x) & x != "" & !grepl(pat, as.character(x), perl = TRUE))
  }

  list(
    missing_col = missing_col,
    missing_total = missing_total,
    exact_dup = exact_dup,
    fuzzy_dup = fuzzy_dup,
    fuzzy_clusters = fuzzy_clusters,
    outliers = outliers,
    invalid_email = email_invalid,
    invalid_phone = phone_invalid,
    schema = schema
  )
}
