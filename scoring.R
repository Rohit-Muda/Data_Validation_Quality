# scoring.R — Shannon entropy (by hand) + one simple quality score

#' Shannon entropy H = -sum(p * log2(p)), normalized by log2(k) so it is in [0, 1].
#' 1 = evenly spread categories, 0 = only one category. Easy to explain on a slide.
entropy_normalized <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(1)
  tab <- table(x)
  p <- as.numeric(tab) / length(x)
  p <- p[p > 0]
  H <- -sum(p * log2(p))
  k <- length(tab)
  if (k <= 1) return(0)
  H / log2(k)
}

#' Average normalized entropy across character columns (higher = more "spread out").
entropy_score <- function(df) {
  chars <- df[sapply(df, is.character)]
  if (ncol(chars) == 0) return(1)
  mean(vapply(chars, entropy_normalized, numeric(1)))
}

#' Score = 100 minus a few penalties. Weights are moderate so the story stays intuitive.
calculate_score <- function(audit, entropy_val, df) {
  n <- nrow(df)
  cells <- n * ncol(df)

  # Penalties are capped so one bad thing cannot wipe the whole score instantly
  pen_miss <- min(30, audit$missing_total * 0.5)
  pen_dup <- min(25, (audit$exact_dup + audit$fuzzy_dup) / max(n, 1) * 100 * 0.35)
  pen_out <- min(20, audit$outliers / max(cells, 1) * 100 * 0.4)
  pen_fmt <- min(20, (audit$invalid_email + audit$invalid_phone) / max(n, 1) * 100 * 0.35)
  pen_ent <- min(15, (1 - entropy_val) * 100 * 0.12)

  score <- 100 - pen_miss - pen_dup - pen_out - pen_fmt - pen_ent
  score <- max(0, min(100, score))

  status <- if (score >= 80) {
    "Approved"
  } else if (score >= 50) {
    "Needs Improvement"
  } else {
    "Rejected"
  }

  list(
    score = round(score, 1),
    status = status,
    severity = if (score >= 80) "Info" else if (score >= 50) "Warning" else "Critical"
  )
}
