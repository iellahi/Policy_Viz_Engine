# ==============================================================================
# CERP Analytics: Master Render Script (Phase 3 — config-driven)
# Purpose: Render the visuals declared in render_config.yml, then (optionally)
#          stitch selected ones into a single combined HTML report.
# Users edit render_config.yml — never this script.
# ==============================================================================

library(rmarkdown)
library(fs)
library(cli)
library(here)
library(yaml)

`%||%` <- function(x, y) if (is.null(x)) y else x

# 0. Pre-processing: convert any dropped Excel files to CSVs automatically
source(here::here("2_R", "2.3_excel_to_csv.R"))

# 1. Core directories + config
config_path  <- here::here("render_config.yml")
template_dir <- here::here("3_templates")
output_dir   <- here::here("4_output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(config_path)) {
  cli_abort("Config not found: {.file render_config.yml} expected at project root.")
}
config    <- yaml::read_yaml(config_path)
data_dir  <- config$defaults$data_dir %||% "1_data"
reports   <- config$reports %||% list()

if (length(reports) == 0) {
  cli_abort("No entries under {.field reports:} in render_config.yml — nothing to render.")
}

# 2. Validate the whole config up front — fail loud before any rendering.
#    (Same ethos as cerp_validate(): a clear error beats a silent-wrong render.)
problems <- character(0)
seen_ids <- character(0)

for (i in seq_along(reports)) {
  entry <- reports[[i]]
  tag   <- entry$id %||% paste0("<entry #", i, ", no id>")

  if (is.null(entry$id)) {
    problems <- c(problems, paste0("Entry #", i, " is missing a required 'id'."))
  } else if (entry$id %in% seen_ids) {
    problems <- c(problems, paste0("Duplicate id '", entry$id, "' — ids must be unique."))
  } else {
    seen_ids <- c(seen_ids, entry$id)
  }

  if (is.null(entry$template)) {
    problems <- c(problems, paste0("[", tag, "] missing required 'template'."))
  } else if (!file.exists(file.path(template_dir, entry$template))) {
    problems <- c(problems, paste0("[", tag, "] template not found: 3_templates/", entry$template))
  }

  if (!is.null(entry$data) &&
      !file.exists(here::here(data_dir, entry$data))) {
    problems <- c(problems, paste0("[", tag, "] data not found: ", data_dir, "/", entry$data))
  }
}

# Combined-report include ids must reference real, enabled entries.
combined <- config$combined
if (isTRUE(combined$enabled)) {
  enabled_ids <- vapply(reports, function(e) {
    inc <- e$enabled %||% TRUE
    if (isTRUE(inc)) e$id %||% NA_character_ else NA_character_
  }, character(1))
  enabled_ids <- enabled_ids[!is.na(enabled_ids)]
  for (id in (combined$include %||% list())) {
    if (!id %in% enabled_ids) {
      problems <- c(problems, paste0("[combined] include id '", id,
                                     "' is not an enabled report entry."))
    }
  }
}

if (length(problems) > 0) {
  cli_alert_danger("render_config.yml has {length(problems)} problem{?s}:")
  cli_ul(problems)
  cli_abort("Fix render_config.yml and re-run.")
}

# 3. Render each enabled entry (override-only params; one failure never stops the batch)
n_ok <- 0L; n_fail <- 0L
enabled_reports <- Filter(function(e) isTRUE(e$enabled %||% TRUE), reports)

cli_alert_success("Config OK. Rendering {length(enabled_reports)} enabled report{?s}.")
cat("\n")

for (entry in enabled_reports) {
  # Output name keeps the template's numeric prefix (e.g. 3.01_baseline_endline.html)
  # so 4_output/ sorts in template order; id keeps it unique.
  num      <- regmatches(entry$template, regexpr("^[0-9]+\\.[0-9]+", entry$template))
  stem     <- if (length(num) == 1L) paste0(num, "_", entry$id) else entry$id
  out_file <- paste0(stem, ".html")
  cli_alert_info("Rendering: {entry$id}  ({entry$template})")

  # Build param overrides: entry params + resolved absolute data_path (if given)
  overrides <- entry$params %||% list()
  if (!is.null(entry$data)) {
    overrides$data_path <- here::here(data_dir, entry$data)
  }

  tryCatch({
    render(
      input        = file.path(template_dir, entry$template),
      output_dir   = output_dir,
      output_file  = out_file,
      params       = overrides,
      envir        = new.env(),   # isolate: no data bleed between templates
      quiet        = TRUE
    )
    cli_alert_success("  -> 4_output/{out_file}")
    n_ok <- n_ok + 1L
  }, error = function(e) {
    cli_alert_danger("  FAILED: {entry$id}")
    cli_alert_warning("  {e$message}")
    n_fail <<- n_fail + 1L
  })
  cat("\n")
}

# 4. Combined report — stitch selected visuals into one HTML (opt-in via config)
if (isTRUE(combined$enabled)) {
  parent <- file.path(template_dir, "3.00_combined_report.Rmd")
  if (!file.exists(parent)) {
    cli_alert_danger("combined.enabled is true but parent template is missing: 3_templates/3.00_combined_report.Rmd")
  } else {
    cli_alert_info("Building combined report: {combined$output %||% 'cerp_combined_report.html'}")
    tryCatch({
      render(
        input       = parent,
        output_dir  = output_dir,
        output_file = combined$output %||% "3.00_cerp_combined_report.html",
        envir       = new.env(),
        quiet       = TRUE
      )
      cli_alert_success("  -> 4_output/{combined$output %||% 'cerp_combined_report.html'}")
    }, error = function(e) {
      cli_alert_danger("  Combined report FAILED.")
      cli_alert_warning("  {e$message}")
    })
    cat("\n")
  }
}

cli_rule()
cli_alert_success("Master render cycle complete: {n_ok} ok, {n_fail} failed. See /4_output.")
