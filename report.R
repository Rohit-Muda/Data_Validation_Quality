# report.R — human-readable summary for presentations

generate_report <- function(audit, score_obj, title = "DATA QUALITY REPORT") {
  cat("\n========== ", title, " ==========\n", sep = "")

  cat("\n--- Summary ---\n")
  cat(sprintf("Overall missing (%% of all cells): %.2f\n", audit$missing_total))
  cat(sprintf("Exact duplicate rows: %d\n", audit$exact_dup))
  cat(sprintf("Fuzzy near-duplicate pairs (1st text col, dist<=2): %d\n", audit$fuzzy_dup))
  cat(sprintf("String clusters with typos (hclust, h=3): %d\n", audit$fuzzy_clusters))
  cat(sprintf("Outlier cells (IQR rule): %d\n", audit$outliers))
  cat(sprintf("Invalid emails: %d | Invalid phones: %d\n", audit$invalid_email, audit$invalid_phone))

  cat("\n--- Worst columns by missing % ---\n")
  top <- sort(audit$missing_col, decreasing = TRUE)[1:min(5, length(audit$missing_col))]
  print(round(top, 2))

  cat("\n--- Score ---\n")
  cat(sprintf("Score: %.1f | Status: %s | Severity: %s\n",
              score_obj$score, score_obj$status, score_obj$severity))
  cat("=====================================\n\n")
}

#' Call this in a live demo to read the intuition aloud from the console.
explain_methods <- function() {
  cat("
  HOW TO EXPLAIN THIS MINI-ENGINE (short version)
  -----------------------------------------------
  1) Fuzzy duplicates: stringdist measures edit distance between strings;
     small distance = possible typo or copy-paste variant.
  2) Clustering: stringdistmatrix + hclust groups similar spellings;
     groups with >1 string are 'suspicious clusters'.
  3) Entropy: on categories, H = -sum(p*log2(p)). Divide by log2(k) so
     1 = evenly spread, 0 = one dominant label. Low score = penalty.
  4) Score: start at 100, subtract capped penalties for missing, dupes,
     outliers, bad formats, and low entropy. Bands: 80+ OK, 50–79 improve, <50 reject.
")
}
