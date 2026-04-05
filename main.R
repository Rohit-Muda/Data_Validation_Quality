# main.R — run the whole pipeline (keep this file short: it is just the storyboard)

library(readr)

source("audit.R")
source("scoring.R")
source("cleaning.R")
source("report.R")

# CSV path: first command-line arg, else sample.csv in this folder
args <- commandArgs(trailingOnly = TRUE)
csv_file <- if (length(args) >= 1) args[1] else "sample.csv"

df <- read_csv(csv_file, show_col_types = FALSE)

audit <- audit_data(df)
ent <- entropy_score(df)
score_obj <- calculate_score(audit, ent, df)

generate_report(audit, score_obj, "BEFORE CLEANING")

choice <- readline(prompt = "Do you want to clean the dataset? (yes/no): ")

if (tolower(trimws(choice)) == "yes") {
  clean_df <- clean_data(df)
  write.csv(clean_df, "cleaned_data.csv", row.names = FALSE)
  cat("\nSaved: cleaned_data.csv\n")

  audit2 <- audit_data(clean_df)
  ent2 <- entropy_score(clean_df)
  score2 <- calculate_score(audit2, ent2, clean_df)
  generate_report(audit2, score2, "AFTER CLEANING")
} else {
  cat("\nNo cleaning performed.\n")
}

# Uncomment to print talking points for your audience:
# explain_methods()
