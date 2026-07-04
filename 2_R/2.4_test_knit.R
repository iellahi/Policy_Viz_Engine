# ==============================================================================
# CERP Analytics: Master Render Script
# Purpose: Automatically knit all parameterized .Rmd templates into HTML reports.
# ==============================================================================

library(rmarkdown)
library(fs)
library(cli)

# 0. Run Pre-Processing: Convert any dropped Excel files to CSVs automatically
source("2.3_excel_to_csv.R")

# 1. Define your core directories based on the project architecture
template_dir <- "../3_templates_testing"
output_dir   <- "../4_output"

# Ensure the output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 2. Dynamically locate all R Markdown files in the templates folder
# This ignores the fact that 3.4 is missing and just grabs what is actually there.
template_files <- dir_ls(template_dir, regexp = "\\.Rmd$")

if (length(template_files) == 0) {
  cli_alert_danger("No .Rmd files found in the 3_templates directory.")
} else {
  cli_alert_success(paste("Found", length(template_files), "templates ready for rendering."))
  cat("\n")
}

# 3. Loop through and render each file securely
for (file in template_files) {
  
  file_name <- basename(file)
  cli_alert_info(paste("Rendering:", file_name, "..."))
  
  tryCatch({
    render(
      input = file,
      output_dir = output_dir,
      # Run in a strictly isolated environment to prevent data bleed between templates
      envir = new.env(), 
      quiet = TRUE
    )
    cli_alert_success(paste("Successfully rendered:", file_name))
    
  }, error = function(e) {
    # If one template fails, catch the error, print it, and move to the next one
    cli_alert_danger(paste("FAILED to render:", file_name))
    cli_alert_warning(paste("Error message:", e$message))
  })
  
  cat("\n")
}

cli_alert_success("Master render cycle complete! Check the /4_output folder.")