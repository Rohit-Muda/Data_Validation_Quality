library(readr)

source("audit.R")
source("scoring.R")
source("cleaning.R")
source("report.R")

# INPUT
df <- read_csv("cleaned_data.csv")

# AUDIT
audit <- audit_data(df)

# ENTROPY
ent <- entropy_score(df)

# SCORE
score_obj <- calculate_score(audit, ent, df)

# REPORT
generate_report(audit, score_obj)

# USER DECISION
choice <- readline(prompt = "Do you want to clean the dataset? (yes/no): ")

if (tolower(trimws(choice)) == "yes") {
  clean_df <- clean_data(df)
  
  write.csv(clean_df, "cleaned_data.csv", row.names = FALSE)
  
  cat("\nCleaned dataset saved as cleaned_data.csv\n")
} else {
  cat("\nNo cleaning performed.\n")
}
