# ==============================================================================
# Script Name: 2.9_publish_docs.R
# Author:      Ibraheem Saqib Ellahi <ibraheemsaqib90@gmail.com>
# Purpose:     Publish the demo gallery to a committed docs/ folder so GitHub
#              Pages can serve it (Phase 13). 4_output/ itself stays gitignored
#              (field-data hard rule) — this script is the ONLY sanctioned path
#              from 4_output/ into git, and it is guarded:
#
#              GUARD 1 (config): every ENABLED entry in render_config.yml must
#                read an explicit master_*.csv. Any other data source (or a
#                missing `data:` line, which we cannot verify) aborts the
#                publish. Synthetic demo data is the only thing that may ever
#                reach docs/.
#              GUARD 2 (whitelist copy): docs/ is wiped and rebuilt from an
#                explicit whitelist — index.html, each enabled report's HTML,
#                the combined report, and only the figure PNGs index.html
#                actually references. Stale or testing renders sharing
#                4_output/ (they do — 2.4_test_knit.R writes there) are never
#                picked up; anything left behind is listed for awareness.
#              GUARD 3 (provenance): the testing knit can OVERWRITE
#                4_output/figures/*.png with field-data versions under the same
#                name. Where the golden snapshot manifest
#                (5_tests/snapshots/manifest.csv) covers a figure, its SHA-256
#                must match — proof the PNG on disk is the verified master-data
#                render. A mismatch aborts (see `strict`).
#
# When to run: manually, IMMEDIATELY AFTER a full production knit
#              (2_R/2.2_master_knit.R) — never after a testing knit:
#                  source(here::here("2_R", "2.9_publish_docs.R"))
#              (sourcing runs cerp_publish_docs() at the bottom of this file).
# Output:      docs/ — index.html + report HTMLs + figures/ + .nojekyll.
#              Commit docs/ and push; GitHub Pages (Settings > Pages > deploy
#              from branch main, folder /docs) serves it at:
#                  https://iellahi.github.io/Policy_Viz_Engine/
# ==============================================================================

# renv owns the environment — NEVER install at render time (hard rule 5).
local({
  pkgs <- c("yaml", "here", "digest")
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(!ok)) {
    stop(
      "Missing package(s): ", paste(pkgs[!ok], collapse = ", "),
      "\nThis project uses renv. Run renv::restore() from the project root, ",
      "then re-run.",
      call. = FALSE
    )
  }
})

`%||%` <- function(x, y) if (is.null(x)) y else x

.cerp_msg <- function(...) {
  if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info(paste0(...))
  else message(paste0(...))
}

# ------------------------------------------------------------------------------
# cerp_publish_docs(): guarded copy of the demo render into docs/.
#   config_path    render_config.yml (project root)
#   output_dir     4_output/ (the just-rendered demo suite)
#   docs_dir       docs/ (WIPED and rebuilt on every run)
#   manifest_path  5_tests/snapshots/manifest.csv (golden figure hashes)
#   strict         TRUE (default): abort if a manifest-covered figure's hash
#                  does not match. Set FALSE only if you have just deliberately
#                  changed a visual and re-verified the render is master-data —
#                  and re-record the manifest (5_tests/snapshot.R) right after.
# ------------------------------------------------------------------------------
cerp_publish_docs <- function(
    config_path   = here::here("render_config.yml"),
    output_dir    = here::here("4_output"),
    docs_dir      = here::here("docs"),
    manifest_path = here::here("5_tests", "snapshots", "manifest.csv"),
    strict        = TRUE
) {
  config  <- yaml::read_yaml(config_path)
  reports <- config$reports %||% list()
  enabled <- Filter(function(e) isTRUE(e$enabled %||% TRUE), reports)

  # --- GUARD 1: enabled entries must read explicit master_*.csv ---------------
  offenders <- character(0)
  for (entry in enabled) {
    id <- entry$id %||% "<no id>"
    d  <- entry$data
    if (is.null(d)) {
      offenders <- c(offenders, paste0(
        id, ": no `data:` line (template default cannot be verified — ",
        "state it explicitly)"
      ))
    } else if (!grepl("^master_.*\\.csv$", d)) {
      offenders <- c(offenders, paste0(id, ": data = ", d))
    }
  }
  if (length(offenders) > 0) {
    stop(
      "PUBLISH ABORTED — docs/ is public; only synthetic master_*.csv demo ",
      "renders may be published.\nEnabled config entries that fail the check:\n",
      paste0("  - ", offenders, collapse = "\n"),
      "\nPoint these at master_*.csv data or set `enabled: false`, re-run the ",
      "master knit, then publish again.",
      call. = FALSE
    )
  }

  # --- Build the whitelist (same output naming as 2.2_master_knit.R) ----------
  index_path <- file.path(output_dir, "index.html")
  if (!file.exists(index_path)) {
    stop(
      "No 4_output/index.html found. Run the full production knit first:\n",
      '  source(here::here("2_R", "2.2_master_knit.R"))',
      call. = FALSE
    )
  }

  html_files <- "index.html"
  for (entry in enabled) {
    template <- entry$template %||% ""
    num  <- regmatches(template, regexpr("^[0-9]+\\.[0-9]+", template))
    stem <- if (length(num) == 1L) paste0(num, "_", entry$id) else entry$id
    html_files <- c(html_files, paste0(stem, ".html"))
  }
  if (isTRUE(config$combined$enabled)) {
    html_files <- c(
      html_files, config$combined$output %||% "3.00_cerp_combined_report.html"
    )
  }

  missing <- html_files[!file.exists(file.path(output_dir, html_files))]
  if (length(missing) > 0) {
    stop(
      "PUBLISH ABORTED — enabled reports have no rendered HTML (the public ",
      "gallery would ship broken links):\n",
      paste0("  - ", missing, collapse = "\n"),
      "\nRun the full production knit, then publish again.",
      call. = FALSE
    )
  }

  # Figures: only what the gallery actually references (report HTMLs are
  # self-contained; thumbnails are the sole reason figures/ is published).
  index_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
  fig_refs   <- unique(unlist(
    regmatches(index_html, gregexpr('src="figures/[^"]+"', index_html))
  ))
  fig_files <- sub('^src="figures/', "", sub('"$', "", fig_refs))
  fig_missing <- fig_files[!file.exists(file.path(output_dir, "figures", fig_files))]
  if (length(fig_missing) > 0) {
    # index.html tolerates missing thumbnails (onerror), so warn, don't abort.
    .cerp_msg(
      "Note: ", length(fig_missing), " thumbnail(s) referenced by index.html ",
      "not found on disk (cards fall back gracefully): ",
      paste(fig_missing, collapse = ", ")
    )
    fig_files <- setdiff(fig_files, fig_missing)
  }

  # --- GUARD 3: provenance — figures must match the golden manifest -----------
  if (file.exists(manifest_path)) {
    manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
    covered  <- intersect(fig_files, manifest$figure_file)
    mismatched <- character(0)
    for (f in covered) {
      have <- digest::digest(
        file.path(output_dir, "figures", f), algo = "sha256", file = TRUE
      )
      want <- manifest$sha256[match(f, manifest$figure_file)]
      if (!identical(have, want)) mismatched <- c(mismatched, f)
    }
    uncovered <- setdiff(fig_files, manifest$figure_file)
    if (length(mismatched) > 0 && isTRUE(strict)) {
      stop(
        "PUBLISH ABORTED — figure(s) on disk do not match the golden snapshot ",
        "manifest:\n",
        paste0("  - figures/", mismatched, collapse = "\n"),
        "\nMost likely cause: a testing knit (2.4) overwrote them with a ",
        "FIELD-DATA render, or the last knit was not a clean master render.\n",
        "Fix: re-run the production knit (2.2_master_knit.R) and publish ",
        "immediately after.\nIf you have deliberately changed a visual: verify ",
        "the render, re-record the manifest (5_tests/snapshot.R), then re-run ",
        "this script (or, consciously, cerp_publish_docs(strict = FALSE)).",
        call. = FALSE
      )
    }
    if (length(mismatched) > 0) {
      .cerp_msg(
        "strict = FALSE: publishing ", length(mismatched),
        " figure(s) whose hash differs from the golden manifest: ",
        paste(mismatched, collapse = ", ")
      )
    }
    if (length(uncovered) > 0) {
      .cerp_msg(
        length(uncovered), " figure(s) not covered by the manifest ",
        "(e.g. templates newer than the last baseline) — copied unverified: ",
        paste(uncovered, collapse = ", ")
      )
    }
  } else {
    .cerp_msg(
      "No golden manifest at 5_tests/snapshots/manifest.csv — figure ",
      "provenance NOT verified. Publish only straight after a clean master knit."
    )
  }

  # --- Wipe and rebuild docs/ from the whitelist -------------------------------
  unlink(docs_dir, recursive = TRUE)
  dir.create(file.path(docs_dir, "figures"), recursive = TRUE)

  ok <- file.copy(file.path(output_dir, html_files), docs_dir, overwrite = TRUE)
  if (length(fig_files) > 0) {
    ok <- c(ok, file.copy(
      file.path(output_dir, "figures", fig_files),
      file.path(docs_dir, "figures"), overwrite = TRUE
    ))
  }
  if (any(!ok)) stop("File copy into docs/ failed — check permissions.", call. = FALSE)

  # GitHub Pages: skip Jekyll processing (serve files exactly as committed).
  file.create(file.path(docs_dir, ".nojekyll"))

  # --- Awareness: what stayed behind (never published) -------------------------
  left_behind <- setdiff(list.files(output_dir, pattern = "\\.html$"), html_files)
  if (length(left_behind) > 0) {
    .cerp_msg(
      length(left_behind), " HTML file(s) in 4_output/ NOT published (not in ",
      "the current config — stale, testing, or per-dataset renders): ",
      paste(left_behind, collapse = ", ")
    )
  }

  n_html <- length(html_files); n_fig <- length(fig_files)
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_success(
      "Published to {.file docs/}: {n_html} HTML file{?s} + {n_fig} figure{?s}."
    )
  } else {
    message("Published to docs/: ", n_html, " HTML files + ", n_fig, " figures.")
  }
  message(
    "Next steps (run yourself from a terminal at the project root):\n",
    '  git add docs/ && git commit -m "Publish demo gallery to docs/" && ',
    "git push origin main\n",
    "One-time: GitHub > Settings > Pages > Source: deploy from branch, ",
    "branch `main`, folder `/docs`.\n",
    "Gallery URL: https://iellahi.github.io/Policy_Viz_Engine/"
  )
  invisible(docs_dir)
}

# Sourcing this file publishes with defaults (mirrors 2.2/2.4 behavior).
cerp_publish_docs()
