# ==============================================================================
# CERP Analytics: Testing Render Script
# Purpose: Render every .Rmd in 3_templates_testing/ (each with its own YAML
#          defaults — no render_config.yml) into HTML. This is the stress
#          sandbox: the same template bodies pointed at messy real data.
#
# NOTE ON OUTPUT COLLISION: testing renders write to the SAME 4_output/ folder
# as production, and share filenames with the production outputs (e.g. both emit
# 3.06_slopegraph.html). A testing render therefore overwrites the production
# HTML of the same id, and vice versa. This is known and intentional for now —
# behavior is left unchanged from prior phases; do not "fix" it here.
# ==============================================================================

library(here)

# Shared render helper: cerp_render_one() (isolated env, tryCatch-continue,
# consistent cli messages) is defined in 2.5_helpers.R, the same helper the
# production script (2.2) uses. Sourcing it also loads the house data helpers.
source(here::here("2_R", "2.5_helpers.R"))

# 0. Pre-processing: convert any dropped Excel files to CSVs automatically
source(here::here("2_R", "2.3_excel_to_csv.R"))

# 1. Core directories
template_dir <- here::here("3_templates_testing")
output_dir   <- here::here("4_output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 2. Render every template in the testing folder with its own defaults. One
#    failure never stops the batch — cerp_render_one() tryCatch-continues and
#    reports pass/fail per file.
template_files <- sort(list.files(template_dir, pattern = "\\.Rmd$", full.names = TRUE))

if (length(template_files) == 0) {
  stop("No .Rmd files found in 3_templates_testing/.", call. = FALSE)
}

for (input in template_files) {
  cerp_render_one(input = input, output_dir = output_dir)
  cat("\n")
}
