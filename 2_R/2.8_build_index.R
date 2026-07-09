# ==============================================================================
# Script Name: 2.8_build_index.R
# Purpose:     Build 4_output/index.html — a static, browsable gallery of every
#              rendered report: one thumbnail card per enabled entry in
#              render_config.yml, plus a featured card for the combined report.
#              Zero new dependencies, no app (Phase 11). Reuses cerp_style.css
#              for page chrome and theme_colors.yml for placeholder tile color.
# When to run: automatically, as the final step of the production knit
#              (2_R/2.2_master_knit.R). Can also be run standalone AFTER a render:
#                  source(here::here("2_R", "2.8_build_index.R"))
#                  cerp_build_index()
# Output:      4_output/index.html — self-contained enough to open from disk
#              (thumbnails and report links are relative paths under 4_output/).
# ==============================================================================

# renv owns the environment — NEVER install at render time (hard rule 5). A
# missing package is a loud, fixable error, not an excuse to mutate the library.
local({
  pkgs <- c("yaml", "here")
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(!ok)) {
    stop(
      "Missing package(s): ", paste(pkgs[!ok], collapse = ", "),
      "\nThis project uses renv. Run renv::restore() from the project root, ",
      "then re-render.",
      call. = FALSE
    )
  }
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------------------------
# .cerp_html_escape(): make a character string safe to drop into HTML text.
# ------------------------------------------------------------------------------
.cerp_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# ------------------------------------------------------------------------------
# .cerp_first_figure(): a report's thumbnail is its first rendered figure PNG.
# Figures are named <chunk-label>-1.png under 4_output/figures/, so we read the
# template, list its code-chunk labels IN DOCUMENT ORDER, and return the first
# one that has a matching PNG on disk. Table-only outputs (e.g. 3.21, a gt table)
# produce no figure and fall through to NA — the caller draws a placeholder tile.
# Deterministic (no dependence on render order/timing) and self-repairing: a
# template whose figure hasn't been rendered yet simply gets the placeholder.
# ------------------------------------------------------------------------------
.cerp_first_figure <- function(template_path, figures_dir) {
  if (!file.exists(template_path)) return(NA_character_)
  lines <- readLines(template_path, warn = FALSE)
  # Chunk headers look like:  ```{r label, opt=...}   — capture 'label'.
  m       <- regexpr("^```\\{r[[:space:]]+[A-Za-z0-9._-]+", lines)
  headers <- regmatches(lines, m)                       # only lines that matched
  labels  <- sub("^```\\{r[[:space:]]+", "", headers)
  for (lab in labels) {
    if (file.exists(file.path(figures_dir, paste0(lab, "-1.png")))) {
      return(paste0("figures/", lab, "-1.png"))
    }
  }
  NA_character_
}

# ------------------------------------------------------------------------------
# cerp_build_index(): write 4_output/index.html from render_config.yml.
#   config_path   render_config.yml (defaults to project root)
#   template_dir  3_templates/ (to read chunk labels for thumbnails)
#   output_dir    4_output/ (index.html + figures/ + the report HTMLs live here)
#   css_path      2_R/cerp_style.css (page chrome, reused)
#   colors_path   2_R/theme_colors.yml (placeholder-tile palette)
# Cards follow the `reports:` order in the config; disabled entries are skipped;
# a missing thumbnail never breaks the page (placeholder + <img> onerror).
# ------------------------------------------------------------------------------
cerp_build_index <- function(
    config_path  = here::here("render_config.yml"),
    template_dir = here::here("3_templates"),
    output_dir   = here::here("4_output"),
    css_path     = here::here("2_R", "cerp_style.css"),
    colors_path  = here::here("2_R", "theme_colors.yml")
) {
  config      <- yaml::read_yaml(config_path)
  reports     <- config$reports %||% list()
  combined    <- config$combined
  figures_dir <- file.path(output_dir, "figures")

  # Placeholder-tile palette (for table-only / missing-figure cards). Read from
  # the brand single-source-of-truth; fall back to a safe default if absent.
  pal_default <- c("#457b9d", "#74a892", "#f2cc8f", "#e07a5f", "#b7c9d3", "#a8dadc")
  ph_palette  <- pal_default
  if (file.exists(colors_path)) {
    hx <- yaml::read_yaml(colors_path)
    keys <- c("primary", "sage", "sand", "accent", "neutral", "secondary")
    got  <- unlist(hx[keys], use.names = FALSE)
    if (length(got) > 0) ph_palette <- got
  }

  # --- Build one <a> card per enabled report, in config order ------------------
  cards    <- character(0)
  n_cards  <- 0L
  ph_i     <- 0L

  for (entry in reports) {
    if (!isTRUE(entry$enabled %||% TRUE)) next          # skip disabled entries
    id       <- entry$id %||% "untitled"
    template <- entry$template %||% ""

    # Output filename must match 2.2_master_knit.R's naming exactly.
    num      <- regmatches(template, regexpr("^[0-9]+\\.[0-9]+", template))
    stem     <- if (length(num) == 1L) paste0(num, "_", id) else id
    out_file <- paste0(stem, ".html")
    tid      <- if (length(num) == 1L) num else id      # displayed template id

    title <- entry$params$report_title %||% entry$report_title %||% id

    thumb <- .cerp_first_figure(file.path(template_dir, template), figures_dir)

    if (!is.na(thumb)) {
      media <- sprintf(
        '<div class="cerp-thumb"><img src="%s" alt="%s" loading="lazy" onerror="this.style.display=\'none\'"></div>',
        .cerp_html_escape(thumb), .cerp_html_escape(title)
      )
    } else {
      col   <- ph_palette[(ph_i %% length(ph_palette)) + 1L]
      ph_i  <- ph_i + 1L
      media <- sprintf(
        '<div class="cerp-thumb"><div class="cerp-ph" style="background:%s"><span class="cerp-ph-id">%s</span><span class="cerp-ph-tag">table</span></div></div>',
        .cerp_html_escape(col), .cerp_html_escape(tid)
      )
    }

    cards <- c(cards, sprintf(
      paste0(
        '<a class="cerp-card" href="%s">',
        '%s',
        '<div class="cerp-card-body">',
        '<span class="cerp-tid">%s</span>',
        '<h3 class="cerp-card-title">%s</h3>',
        '<span class="cerp-fname">%s</span>',
        '</div></a>'
      ),
      .cerp_html_escape(out_file), media,
      .cerp_html_escape(tid),
      .cerp_html_escape(title),
      .cerp_html_escape(template)
    ))
    n_cards <- n_cards + 1L
  }

  # --- Featured card for the combined report (top of the page) -----------------
  featured <- ""
  if (isTRUE(combined$enabled)) {
    combined_out <- combined$output %||% "3.00_cerp_combined_report.html"
    ctitle <- combined$title %||% "Combined Report"
    prim   <- ph_palette[1]
    sec    <- if (length(ph_palette) >= 6) ph_palette[6] else "#a8dadc"
    n_inc  <- length(combined$include %||% list())
    featured <- sprintf(
      paste0(
        '<a class="cerp-featured" href="%s">',
        '<div class="cerp-featured-banner" style="background:linear-gradient(135deg,%s,%s)"></div>',
        '<div class="cerp-featured-body">',
        '<span class="cerp-tid">Combined report</span>',
        '<h2 class="cerp-featured-title">%s</h2>',
        '<p class="cerp-featured-sub">%s visuals stitched under one house style.</p>',
        '<span class="cerp-fname">%s</span>',
        '</div></a>'
      ),
      .cerp_html_escape(combined_out),
      .cerp_html_escape(prim), .cerp_html_escape(sec),
      .cerp_html_escape(ctitle),
      n_inc,
      .cerp_html_escape(combined_out)
    )
  }

  # --- Page chrome: reuse cerp_style.css, then gallery-specific CSS -------------
  base_css <- if (file.exists(css_path)) {
    paste(readLines(css_path, warn = FALSE), collapse = "\n")
  } else ""

  gallery_css <- '
/* --- Phase 11 gallery-specific layout (index.html only) ------------------- */
body { margin: 0; }
.cerp-wrap { max-width: 1120px; margin: 0 auto; padding: 2.25rem 1.25rem 3.5rem; }
.cerp-page-title { font-size: 1.9rem; margin: 0 0 .2rem; }
.cerp-page-sub { color: #54707f; font-family: "Libre Franklin","Helvetica Neue",Arial,sans-serif; margin: 0 0 1.75rem; }
.cerp-featured { display: grid; grid-template-columns: 240px 1fr; margin: 0 0 1.75rem; background: #fff; border: 1px solid #e5e1d8; border-radius: 12px; overflow: hidden; text-decoration: none; color: inherit; box-shadow: 0 4px 14px rgba(27,58,75,.08); transition: box-shadow .15s, transform .15s; }
.cerp-featured:hover { box-shadow: 0 8px 22px rgba(27,58,75,.14); transform: translateY(-2px); }
.cerp-featured-banner { min-height: 156px; }
.cerp-featured-body { padding: 1.3rem 1.6rem; }
.cerp-featured-title { font-size: 1.35rem; margin: .25rem 0 .4rem; }
.cerp-featured-sub { color: #54707f; margin: 0 0 .6rem; font-family: "Libre Franklin",Arial,sans-serif; font-size: .95rem; }
.cerp-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(258px, 1fr)); gap: 1.25rem; }
.cerp-card { display: flex; flex-direction: column; text-decoration: none; color: inherit; background: #fff; border: 1px solid #e5e1d8; border-radius: 10px; overflow: hidden; transition: box-shadow .15s, transform .15s; }
.cerp-card:hover { box-shadow: 0 6px 18px rgba(27,58,75,.12); transform: translateY(-2px); }
.cerp-thumb { aspect-ratio: 16 / 10; background: #FAF9F6; display: flex; align-items: center; justify-content: center; overflow: hidden; border-bottom: 1px solid #eee; }
.cerp-thumb img { width: 100%; height: 100%; object-fit: contain; background: #FAF9F6; }
.cerp-ph { width: 100%; height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; color: #fff; gap: .3rem; }
.cerp-ph-id { font-family: "Libre Franklin",Arial,sans-serif; font-weight: 600; font-size: 1.5rem; letter-spacing: .02em; }
.cerp-ph-tag { font-family: "Libre Franklin",Arial,sans-serif; text-transform: uppercase; letter-spacing: .12em; font-size: .68rem; opacity: .85; }
.cerp-card-body { padding: .85rem 1rem 1.1rem; }
.cerp-tid { font-family: "Libre Franklin",Arial,sans-serif; font-size: .72rem; letter-spacing: .05em; text-transform: uppercase; color: #54707f; }
.cerp-card-title { font-size: 1.02rem; margin: .22rem 0 .4rem; line-height: 1.25; }
.cerp-fname { font-family: ui-monospace, Menlo, Consolas, monospace; font-size: .72rem; color: #8a9aa3; word-break: break-all; }
@media (max-width: 560px) { .cerp-featured { grid-template-columns: 1fr; } .cerp-featured-banner { min-height: 96px; } }
'

  # --- Assemble the page -------------------------------------------------------
  page_title <- (combined$title %||% "CERP Visualization Suite")
  built_at   <- format(Sys.time(), "%Y-%m-%d %H:%M")

  html <- paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '<title>', .cerp_html_escape(page_title), ' — Gallery</title>\n',
    '<style>\n', base_css, '\n', gallery_css, '</style>\n',
    '</head>\n<body>\n',
    '<div class="cerp-wrap">\n',
    '<h1 class="cerp-page-title">', .cerp_html_escape(page_title), '</h1>\n',
    '<p class="cerp-page-sub">', n_cards,
    ' visuals — built ', .cerp_html_escape(built_at),
    '. Click any card to open the full report.</p>\n',
    featured,
    '<div class="cerp-grid">\n',
    paste(cards, collapse = "\n"),
    '\n</div>\n</div>\n</body>\n</html>\n'
  )

  out_path <- file.path(output_dir, "index.html")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat(html, file = out_path)

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_success(
      "Gallery written: {.file 4_output/index.html} ({n_cards} card{?s})."
    )
  } else {
    message("Gallery written: 4_output/index.html (", n_cards, " cards).")
  }
  invisible(out_path)
}
