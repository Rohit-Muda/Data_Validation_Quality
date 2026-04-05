library(dplyr)
library(stringr)
library(stringdist)

clean_data <- function(df) {
  
  # -----------------------------
  # STEP 1: FORMAT FIX FIRST
  # -----------------------------
  df <- df %>% mutate(across(where(is.character), str_trim))
  df <- df %>% mutate(across(where(is.character), str_to_lower))
  
  # -----------------------------
  # STEP 2: MISSING VALUES
  # -----------------------------
  for (col in names(df)) {
    
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      
    } else {
      if (all(is.na(df[[col]]))) next
      
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
    }
  }
  
  # -----------------------------
  # STEP 3: EMAIL FIX
  # -----------------------------
  if ("email" %in% names(df)) {
    
    valid_pattern <- "^[^@]+@[^@]+\\.[^@]+$"
    
    invalid_idx <- !grepl(valid_pattern, df$email)
    valid_emails <- df$email[grepl(valid_pattern, df$email)]
    
    if (length(valid_emails) > 0) {
      mode_email <- names(sort(table(valid_emails), decreasing = TRUE))[1]
      df$email[invalid_idx] <- mode_email
    }
  }
  
  # -----------------------------
  # STEP 4: REMOVE EXACT DUPLICATES
  # -----------------------------
  df <- df %>% distinct()
  
  # -----------------------------
  # STEP 5: FUZZY DUPLICATE REMOVAL (SAFE VERSION)
  # -----------------------------
  remove_fuzzy_duplicates <- function(df) {
    
    if (nrow(df) < 2) return(df)
    if (!all(c("name", "city") %in% names(df))) return(df)
    
    keep <- rep(TRUE, nrow(df))
    
    for (i in 1:(nrow(df)-1)) {
      
      if (!keep[i]) next
      
      for (j in (i+1):nrow(df)) {
        
        # Compare combined fields (more reliable)
        str1 <- paste(df$name[i], df$city[i])
        str2 <- paste(df$name[j], df$city[j])
        
        dist <- stringdist(str1, str2, method = "lv")
        
        # STRICT threshold → avoids over-removal
        if (dist <= 2) {
          keep[j] <- FALSE
        }
      }
    }
    
    return(df[keep, ])
  }
  
  # APPLY fuzzy duplicate removal
  df <- remove_fuzzy_duplicates(df)
  
  # -----------------------------
  # STEP 6: OUTLIER HANDLING
  # -----------------------------
  num_cols <- df %>% select(where(is.numeric))
  
  for (col in names(num_cols)) {
    
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    df[[col]] <- ifelse(df[[col]] < lower, lower,
                        ifelse(df[[col]] > upper, upper, df[[col]]))
    
    # Domain rule (important for demo)
    if (col == "age") {
      df[[col]] <- pmin(df[[col]], 100)
    }
  }
  
  # -----------------------------
  # FINAL OUTPUT
  # -----------------------------
  return(df)
}