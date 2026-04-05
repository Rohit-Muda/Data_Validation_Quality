library(dplyr)
library(stringdist)
library(stringr)

audit_data <- function(df) {

  # Missing %
  missing_col <- colSums(is.na(df)) / nrow(df) * 100
  missing_total <- sum(is.na(df)) / (nrow(df) * ncol(df)) * 100

  # Exact duplicates
  exact_dup <- sum(duplicated(df))

  # Fuzzy duplicates (on character cols)
  char_cols <- df %>% select(where(is.character))
  fuzzy_dup <- 0

  if (ncol(char_cols) > 0) {
    mat <- stringdistmatrix(char_cols[[1]], char_cols[[1]], method = "lv")
    clusters <- hclust(as.dist(mat))
    fuzzy_dup <- sum(cutree(clusters, h = 2) > 1)
  }

  # Outliers (IQR)
  outliers <- 0
  num_cols <- df %>% select(where(is.numeric))
  for (col in names(num_cols)) {
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    outliers <- outliers + sum(df[[col]] < (q1 - 1.5 * iqr) | df[[col]] > (q3 + 1.5 * iqr), na.rm = TRUE)
  }

  # Regex checks
  email_invalid <- 0
  if ("email" %in% names(df)) {
    email_invalid <- sum(!grepl("^[^@]+@[^@]+\\.[^@]+$", df$email), na.rm = TRUE)
  }

  return(list(
    missing_col = missing_col,
    missing_total = missing_total,
    exact_dup = exact_dup,
    fuzzy_dup = fuzzy_dup,
    outliers = outliers,
    invalid_email = email_invalid
  ))
}