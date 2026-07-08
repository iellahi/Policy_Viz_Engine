# ==============================================================================
# Script Name: 5_tests/snapshot.R
# Purpose:     Golden-figure regression harness for the plot-function extraction
#              (phase 8). It renders the production suite on master data via the
#              shared render core (2_R/2.2_master_knit.R), then SHA-256-hashes
#              every figure PNG in 4_output/figures/ so a refactor can be proven
#              to be a no-op (identical pixels => identical hash).
#
# MODE (automatic, based on whether the golden manifest already exists):
#   * manifest MISSING -> RECORD: writes 5_tests/snapshots/manifest.csv, the
#       golden baseline. Run this ONCE on the UNTOUCHED templates BEFORE
#       extracting any plot functions, then commit the manifest.
#   * manifest PRESENT -> COMPARE: re-renders, re-hashes, and reports every
#       figure whose hash changed / went missing / newly appeared. Zero
#       mismatches means the refactor changed no pixels. Run after each
#       extraction batch.
#   To deliberately re-baseline, delete 5_tests/snapshots/manifest.csv and
#   re-source this script.
#
# IMPORTANT: hashes are only comparable on the SAME machine — font rasterization
#            differs across systems, so a PNG rendered on machine A will not hash
#            equal to the same plot on machine B. Always record AND compare on
#            your own machine. renv owns packages; nothing is installed here.
# ==============================================================================

library(here)
source(here::here("2_R", "2.5_helpers.R"))   # house helpers (cerp_require, %||%)
cerp_require(c("digest", "cli"))              # digest = SHA-256; cli = messaging

# --- paths --------------------------------------------------------------------
fig_dir      <- here::here("4_output", "figures")
snap_dir     <- here::here("5_tests", "snapshots")
manifest_csv <- file.path(snap_dir, "manifest.csv")
dir.create(snap_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# build_label_map(): figures are named "<chunk-label>-<n>.png", and each template
# uses a unique plot chunk label, so we can map every figure back to the template
# that produced it by scanning the template chunk headers. Used only to label the
# manifest rows for readability — the golden comparison itself keys on
# figure_file + sha256, not on this.
# ------------------------------------------------------------------------------
build_label_map <- function() {
  tpl_files <- list.files(here::here("3_templates"), pattern = "\\.Rmd$",
                          full.names = TRUE)
  map <- list()
  for (f in tpl_files) {
    lines  <- readLines(f, warn = FALSE)
    heads  <- regmatches(lines, regexpr("^```\\{r [^ ,}]+", lines))
    labels <- sub("^```\\{r ", "", heads)
    for (lab in labels) map[[lab]] <- basename(f)   # unique label -> template
  }
  map
}

figure_template_id <- function(figure_file, label_map) {
  lab <- sub("-[0-9]+\\.png$", "", figure_file)      # strip trailing -<n>.png
  label_map[[lab]] %||% NA_character_
}

# ------------------------------------------------------------------------------
# render_and_hash(): clear the figure dir first (so orphaned PNGs from renamed or
# removed chunks can never pollute the manifest), render the whole production
# suite through the shared core, then hash whatever figures the CURRENT templates
# produced. Returns a data.frame: template_id, figure_file, sha256, rendered_at.
# ------------------------------------------------------------------------------
render_and_hash <- function() {
  old <- list.files(fig_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(old) > 0) file.remove(old)

  # Shared, config-driven production render. Sourced into a child environment so
  # its top-level objects don't leak into this script's namespace.
  cli::cli_h1("Rendering production suite (master data)")
  source(here::here("2_R", "2.2_master_knit.R"), local = new.env())

  pngs <- sort(list.files(fig_dir, pattern = "\\.png$"))
  if (length(pngs) == 0) {
    stop("No figures were produced in 4_output/figures/ — did the render fail?",
         call. = FALSE)
  }
  label_map <- build_label_map()
  data.frame(
    template_id = vapply(pngs, figure_template_id, character(1), label_map),
    figure_file = pngs,
    sha256      = vapply(file.path(fig_dir, pngs),
                         function(p) digest::digest(file = p, algo = "sha256"),
                         character(1)),
    rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    row.names   = NULL, stringsAsFactors = FALSE
  )
}

# --- RECORD or COMPARE --------------------------------------------------------
current <- render_and_hash()

if (!file.exists(manifest_csv)) {
  # ---- RECORD ----------------------------------------------------------------
  write.csv(current, manifest_csv, row.names = FALSE)
  cli::cli_rule()
  cli::cli_alert_success(
    "Baseline recorded: {nrow(current)} figure{?s} -> 5_tests/snapshots/manifest.csv")
  cli::cli_alert_info(
    "Commit this manifest, THEN begin plot-function extraction.")
} else {
  # ---- COMPARE ---------------------------------------------------------------
  golden <- read.csv(manifest_csv, stringsAsFactors = FALSE)
  # write the latest run alongside the golden for inspection/diffing
  write.csv(current, file.path(snap_dir, "manifest_latest.csv"), row.names = FALSE)

  g <- setNames(golden$sha256,  golden$figure_file)
  n <- setNames(current$sha256, current$figure_file)

  common   <- intersect(names(g), names(n))
  changed  <- common[g[common] != n[common]]     # same figure, different pixels
  missing  <- setdiff(names(g), names(n))         # in baseline, not reproduced
  appeared <- setdiff(names(n), names(g))         # produced now, not in baseline

  n_bad <- length(changed) + length(missing) + length(appeared)

  cli::cli_rule()
  if (n_bad == 0) {
    cli::cli_alert_success(
      "0 mismatches across {nrow(golden)} figure{?s}. Refactor is a no-op.")
  } else {
    cli::cli_alert_danger(
      "{n_bad} figure mismatch{?es} vs. the golden manifest:")
    for (f in changed)  cli::cli_alert_warning("  changed hash:    {f}")
    for (f in missing)  cli::cli_alert_warning("  not reproduced:  {f}")
    for (f in appeared) cli::cli_alert_warning("  unexpected new:  {f}")
    cli::cli_alert_info(
      "Latest hashes: 5_tests/snapshots/manifest_latest.csv. Fix before continuing.")
  }
}
