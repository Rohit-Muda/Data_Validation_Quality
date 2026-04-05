generate_report <- function(audit, score_obj) {

  cat("\n===== DATA QUALITY REPORT =====\n")

  cat("\nMissing % (overall):", round(audit$missing_total,2))
  cat("\nExact Duplicates:", audit$exact_dup)
  cat("\nFuzzy Duplicates:", audit$fuzzy_dup)
  cat("\nOutliers:", audit$outliers)
  cat("\nInvalid Emails:", audit$invalid_email)

  cat("\n\nFinal Score:", score_obj$score)
  cat("\nStatus:", score_obj$status)

  cat("\n================================\n")
}