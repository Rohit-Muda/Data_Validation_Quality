# cleaning.R — small cleaning path for demos

library(dplyr)
library(stringr)
library(stringdist)

clean_data <- function(df) {
  df <- df %>% mutate(across(where(is.character), ~ str_to_lower(str_trim(.))))

  for (col in names(df)) {
    if (all(is.na(df[[col]]))) next
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    } else {
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
    }
  }

  if ("email" %in% names(df)) {
    pat <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
    ok <- grepl(pat, df$email, perl = TRUE)
    if (any(ok)) {
      best <- names(sort(table(df$email[ok]), decreasing = TRUE))[1]
      df$email[!ok] <- best
    }
  }

  df <- distinct(df)

  if (nrow(df) >= 2) {
    if (all(c("name", "city") %in% names(df))) {
      key <- paste(df$name, df$city)
    } else {
      cc <- df %>% select(where(is.character))
      if (ncol(cc) == 0) {
        key <- NULL
      } else if (ncol(cc) >= 2) {
        key <- paste(cc[[1]], cc[[2]])
      } else {
        key <- cc[[1]]
      }
    }
    if (!is.null(key)) {
      keep <- rep(TRUE, nrow(df))
      for (i in 1:(nrow(df) - 1)) {
        if (!keep[i]) next
        for (j in (i + 1):nrow(df)) {
          if (stringdist(key[i], key[j], method = "lv") <= 2) keep[j] <- FALSE
        }
      }
      df <- df[keep, , drop = FALSE]
    }
  }

  for (col in names(df %>% select(where(is.numeric)))) {
    x <- df[[col]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    low <- q1 - 1.5 * iqr
    high <- q3 + 1.5 * iqr
    df[[col]] <- pmin(pmax(x, low), high)
  }

  df
}
