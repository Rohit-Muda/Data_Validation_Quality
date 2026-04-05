library(entropy)

entropy_score <- function(df) {
  cat_cols <- df[, sapply(df, is.character), drop = FALSE]
  scores <- c()
  
  for (col in names(cat_cols)) {
    probs <- table(cat_cols[[col]]) / nrow(df)
    ent <- entropy::entropy(probs, unit = "log2")
    scores <- c(scores, ent)
  }
  
  if (length(scores) == 0) return(1)
  return(mean(scores) / log2(nrow(df)))
}

calculate_score <- function(audit, entropy_val, df) {
  
  total_cells <- nrow(df) * ncol(df)
  
  missing_p <- audit$missing_total
  dup_p <- (audit$exact_dup + audit$fuzzy_dup) / nrow(df) * 100
  outlier_p <- audit$outliers / total_cells * 100
  invalid_p <- audit$invalid_email / nrow(df) * 100
  entropy_penalty <- (1 - entropy_val) * 100
  
  score <- 100 -
    (missing_p * 0.3 +
       dup_p * 0.2 +
       outlier_p * 0.2 +
       invalid_p * 0.2 +
       entropy_penalty * 0.1)
  
  status <- ifelse(score >= 70, "Approved",
                   ifelse(score >= 50, "Needs Improvement", "Rejected"))
  
  return(list(score = round(score,2), status = status))
}