# ==============================================================================
# Script Name: 5_tests/stress_harness.R
# Purpose:     Drive every viz_*() function against the corrupted variants built
#              by 5_tests/stress_data_generator.R and classify each outcome. The
#              suite exists to enforce hard rule 4: bad data must produce a loud,
#              clear error — NEVER a silently-wrong chart.
#
#              For each template x variant it reproduces the Rmd wrapper's path —
#              cerp_load() -> cerp_validate() -> viz_*() -> force the ggplot to
#              BUILD (ggplot2 is lazy; a plot that only errors when drawn would
#              otherwise look like a pass) — inside a tryCatch that also traps
#              warnings, then labels the result:
#
#                pass            succeeded on input we expect the engine to absorb.
#                clean-error     failed LOUDLY with an informative message (names
#                                the column / problem — the desired fail-loud).
#                bad-error       failed with an opaque, low-level message (e.g.
#                                "subscript out of bounds") — needs a clear guard.
#                silent-success  succeeded on input that should have been rejected
#                                or is ambiguous — the human review queue.
#
#              `review = TRUE` marks every row a human must look at: all
#              bad-errors, all silent-successes, and any case the engine
#              over-rejected (errored where we expected it to cope).
#
# Output:      5_tests/stress_results.csv (GITIGNORED). Review it together; the
#              fix step (PLAN.md phase 4, item 4) prefers strengthening
#              2_R/2.5_helpers.R over per-template patches.
#
# Run:         source(here::here("5_tests", "stress_data_generator.R"))  # first
#              source(here::here("5_tests", "stress_harness.R"))
# renv owns packages — nothing is installed here.
# ==============================================================================

library(here)

# House engine: helpers (cerp_load/validate/...), theme (cerp_cols, theme_cerp),
# and the chart functions themselves. Same load order the templates use.
source(here::here("2_R", "2.5_helpers.R"))
source(here::here("2_R", "2.0_setup_theme.R"))
source(here::here("2_R", "2.6_viz_functions.R"))
source(here::here("5_tests", "stress_spec.R"))

# Attach the same optional packages the wrappers attach, so unqualified geoms
# (ggridges) and dispatch (sf, fixest) resolve exactly as in a real render.
cerp_require(c("ggrepel", "ggridges", "sf", "fixest", "cli"))

# --- paths --------------------------------------------------------------------
data_dir      <- here::here("1_data")
stress_dir    <- here::here("5_tests", "stress_data")
manifest_csv  <- file.path(stress_dir, "_manifest.csv")
results_csv   <- here::here("5_tests", "stress_results.csv")

if (!file.exists(manifest_csv)) {
  stop("No stress variants found. Run 5_tests/stress_data_generator.R first.",
       call. = FALSE)
}
manifest <- readr::read_csv(manifest_csv, show_col_types = FALSE)

# ------------------------------------------------------------------------------
# is_clean_message(): decide whether an error message is the loud, specific kind
# we WANT (names a column or the concrete problem) versus an opaque internal
# error. Clean if it matches a known house fail-loud phrase, or mentions one of
# the data's columns or the *_var values in play. Everything else is treated as
# a bad (opaque) error so it surfaces for a clearer guard — we would rather
# over-flag than let an opaque failure hide.
# ------------------------------------------------------------------------------
clean_phrases <- c(
  "not found in the data", "Columns available", "did you mean",
  "could not be matched to the map geometry", "closest geometry name",
  "distinct categories", "No non-missing values found",
  "Multiple rows share the same", "Unknown aggregate option",
  "No rows remain after filtering", "is not present in", "Available event times",
  "is not a field in the geometry", "Data file not found", "Geometry file not found",
  "Run renv::restore", "is required for", "Fields available"
)

is_clean_message <- function(msg, tokens) {
  if (is.na(msg) || !nzchar(msg)) return(FALSE)
  if (any(vapply(clean_phrases, function(p) grepl(p, msg, fixed = TRUE),
                 logical(1)))) return(TRUE)
  tokens <- unique(tokens[nzchar(tokens)])
  any(vapply(tokens, function(t) grepl(t, msg, fixed = TRUE), logical(1)))
}

# ------------------------------------------------------------------------------
# run_case(): execute one template x variant exactly as the wrapper would, force
# the plot to build, and capture the first error plus every warning. Returns a
# one-row data.frame ready for classification.
# ------------------------------------------------------------------------------
run_case <- function(entry, variant_file, expect, targets, note) {
  fn   <- get(entry$fn)
  path <- file.path(stress_dir, variant_file)
  warn_msgs <- character(0)
  err_msg   <- NA_character_

  result <- tryCatch(
    withCallingHandlers(
      {
        df <- cerp_load(path)                       # squish headers/values
        cerp_validate(df, stress_var_args(entry))   # fail loud on bad *_var

        arglist <- c(list(data = df), entry$args)
        if (isTRUE(entry$spatial)) {                # choropleth needs geo + xwalk
          geo_path    <- file.path(data_dir, "geo", entry$geo_file)
          lookup_path <- file.path(data_dir, "geo", entry$lookup_file)
          geo <- sf::st_read(geo_path, quiet = TRUE)
          lookup <- NULL
          if (file.exists(lookup_path)) {
            lookup <- readr::read_csv(lookup_path, comment = "#",
                                      show_col_types = FALSE, trim_ws = TRUE)
            names(lookup) <- stringr::str_squish(names(lookup))
          }
          arglist <- c(arglist, list(geo = geo, lookup = lookup))
        }

        p <- do.call(fn, arglist)
        invisible(ggplot2::ggplot_build(p))         # FORCE lazy plot to compute
        "success"
      },
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")              # collect, don't abort
      }
    ),
    error = function(e) {
      err_msg <<- conditionMessage(e)
      "error"
    }
  )

  # Classify --------------------------------------------------------------------
  tokens <- c(unlist(entry$args, use.names = FALSE))   # column names + labels
  errored <- identical(result, "error")
  if (errored) {
    status <- if (is_clean_message(err_msg, tokens)) "clean-error" else "bad-error"
  } else {
    status <- if (identical(expect, "ok")) "pass" else "silent-success"
  }

  # Review queue: opaque errors, silent successes, and over-rejections
  # (errored on a case we expected the engine to absorb).
  review <- status %in% c("bad-error", "silent-success") ||
    (errored && identical(expect, "ok"))

  first_msg <- if (errored) err_msg else
    if (length(warn_msgs) > 0) warn_msgs[1] else ""

  data.frame(
    template  = entry$id,
    fn        = entry$fn,
    data      = entry$data,
    variant   = sub("^.*__(.*)\\.csv$", "\\1", variant_file),
    expect    = expect,
    status    = status,
    review    = review,
    n_warn    = length(warn_msgs),
    message   = gsub("[\r\n]+", " ", first_msg),
    note      = note,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# Drive the matrix: every template against the common battery for its file plus
# the specials aimed at it. `entry$id` is filled from the list name so the spec
# stays terse.
# ------------------------------------------------------------------------------
cli::cli_h1("CERP stress harness — {nrow(manifest)} variants on disk")

rows <- list()
for (id in names(stress_templates)) {
  entry <- stress_templates[[id]]
  entry$id <- id

  applicable <- manifest[manifest$data == entry$data &
    (manifest$targets == "*" |
       vapply(strsplit(manifest$targets, ";"), function(t) id %in% t, logical(1))), ]

  if (nrow(applicable) == 0) next
  cli::cli_alert_info("{id}  {entry$fn}  ({nrow(applicable)} variant{?s})")

  for (i in seq_len(nrow(applicable))) {
    rows[[length(rows) + 1]] <- run_case(
      entry, applicable$file[i], applicable$expect[i],
      applicable$targets[i], applicable$note[i]
    )
  }
}

results <- do.call(rbind, rows)
readr::write_csv(results, results_csv)

# ------------------------------------------------------------------------------
# Summary ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
counts <- table(factor(results$status,
                       levels = c("pass", "clean-error", "bad-error", "silent-success")))
review_q <- results[results$review, ]

cli::cli_rule()
cli::cli_h2("Results: {nrow(results)} cases")
cli::cli_alert_success("pass:            {counts[['pass']]}")
cli::cli_alert_success("clean-error:     {counts[['clean-error']]}")
cli::cli_alert_danger ("bad-error:       {counts[['bad-error']]}")
cli::cli_alert_warning("silent-success:  {counts[['silent-success']]}")
cli::cli_rule()

if (nrow(review_q) == 0) {
  cli::cli_alert_success("Review queue empty — nothing needs a human eye.")
} else {
  cli::cli_alert_warning("Review queue: {nrow(review_q)} case{?s} to eyeball together:")
  ul <- cli::cli_ul()
  for (i in seq_len(nrow(review_q))) {
    r <- review_q[i, ]
    cli::cli_li("{r$template} / {r$variant}  [{r$status}]  {substr(r$message, 1, 80)}")
  }
  cli::cli_end(ul)
}
cli::cli_alert_info("Full log: 5_tests/stress_results.csv")
