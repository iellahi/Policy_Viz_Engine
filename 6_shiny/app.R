# ==============================================================================
# 6_shiny/app.R — CERP config-builder (phase 12)
# ------------------------------------------------------------------------------
# A *local* front end to render_config.yml. This is NOT a second render engine:
# it profiles a dropped CSV, recommends production templates, lets the user map
# columns to a template's *_var params, previews the chart via the SAME viz_*()
# functions the reports use, and emits a `reports:` entry to paste into
# render_config.yml. Field data never leaves the machine and nothing here calls
# an external API; hosted deployment is permanently out of scope.
#
# One source of truth: the app sources the engine (palette, helpers, chart
# functions) rather than re-implementing any of it.
#
# Launch — from the project root, in RStudio or R:
#     shiny::runApp(here::here("6_shiny"))
#
# Author: Ibraheem Saqib Ellahi <ibraheemsaqib90@gmail.com>
# ==============================================================================

# --- Dependencies: fail loud, never auto-install (hard rule 5) ----------------
# The engine scripts sourced below carry their own loud checks for the render
# stack (tidyverse, scales, sf, …). This block covers the app-chrome packages
# those scripts do NOT load, so a missing Shiny stack also fails with a clear
# renv::restore() instruction instead of an obscure "could not find function".
local({
  pkgs <- c("shiny", "bslib", "here", "yaml", "readr", "dplyr",
            "stringr", "rmarkdown", "knitr", "scales")
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(!ok)) {
    stop("Missing package(s): ", paste(pkgs[!ok], collapse = ", "),
         "\nThis project uses renv. Run renv::restore() from the project root, ",
         "then relaunch the app with shiny::runApp(here::here(\"6_shiny\")).",
         call. = FALSE)
  }
})

library(shiny)
library(bslib)

# --- Engine: shared palette, helpers, and chart functions ---------------------
# Sourced into the global environment (default local = FALSE) so cerp_cols, the
# cerp_*() helpers, and every viz_*() are visible to the server. Order matters:
# viz functions reference theme objects, so the theme is sourced first. Each of
# these scripts fails loud on its own missing packages.
source(here::here("2_R", "2.0_setup_theme.R"))     # cerp_cols, fonts, theme_cerp()
source(here::here("2_R", "2.5_helpers.R"))          # cerp_load / cerp_profile / cerp_recommend / cerp_validate / …
source(here::here("2_R", "2.6_viz_functions.R"))    # viz_*()

# --- Theme the app from the SAME palette + fonts as the reports ---------------
# Colors come straight from cerp_cols (which is loaded from theme_colors.yml), so
# editing the palette re-themes the app too — no second place to change. Fonts
# use locally installed families (no web fetch), matching the report intent:
# Charter/Georgia for the branded headings, Libre Franklin for UI/body text.
cerp_app_theme <- bs_theme(
  version      = 5,
  bg           = cerp_cols[["bg"]],
  fg           = cerp_cols[["text"]],
  primary      = cerp_cols[["primary"]],
  secondary    = cerp_cols[["secondary"]],
  base_font    = font_collection("Libre Franklin", "Franklin Gothic Medium", "sans-serif"),
  heading_font = font_collection("Charter", "Georgia", "serif")
)

# --- Small house-style callout (mirrors the 0.00 report's .cerp-callout) -------
cerp_callout <- function(..., kind = c("info", "warn", "flag", "ok")) {
  kind <- match.arg(kind)
  cls  <- if (kind == "info") "cerp-callout" else paste0("cerp-callout ", kind)
  div(class = cls, ...)
}

# --- Parse a recommendation mapping string into a named list ------------------
# cerp_recommend() emits mapping as "group_var = X; before_var = Y". Turn that
# back into list(group_var = "X", before_var = "Y") to pre-fill the Map tab's
# *_var dropdowns. Empty / malformed input yields an empty list.
cerp_parse_mapping <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(list())
  parts <- strsplit(s, "\\s*;\\s*")[[1]]
  kv    <- strsplit(parts, "\\s*=\\s*")
  keys  <- vapply(kv, function(x) trimws(x[[1]]), character(1))
  vals  <- lapply(kv, function(x) if (length(x) == 2) trimws(x[[2]]) else NA_character_)
  setNames(vals, keys)
}

# --- Grouped column choices for a *_var dropdown ------------------------------
# Expected-type columns surface under "Suggested (type)"; the rest under "Other
# columns" so nothing is hard-blocked. Falls back to a flat list if the param has
# no documented expectation.
cerp_col_choices <- function(param, all_cols, col_types) {
  exp <- cerp_var_types[[param]]
  if (is.null(exp)) return(as.list(all_cols))
  matched <- all_cols[col_types[all_cols] %in% exp]
  other   <- setdiff(all_cols, matched)
  grp <- list()
  if (length(matched))
    grp[[sprintf("Suggested (%s)", paste(exp, collapse = " / "))]] <- as.list(matched)
  if (length(other)) grp[["Other columns"]] <- as.list(other)
  if (length(grp) == 0) as.list(all_cols) else grp
}

# --- Build one Map-tab control from a param name + its YAML default -----------
# Control type is chosen from the param name / default value, so it stays generic
# across all templates (and any future param):
#   *_var  -> single column dropdown       *_vars -> multi column dropdown
#   logical default -> checkbox            numeric default -> numeric input
#   everything else  -> text input ("" keeps the template's auto default)
# --- YAML config-entry emitter ------------------------------------------------
# render_config.yml is OVERRIDE-ONLY: entries list only what differs from the
# template default. The app never rewrites the (heavily commented) file — it
# prints a `reports:` entry for the user to paste. These helpers format one entry
# with the file's 2-space style; scalars are quoted only when they need it.
cerp_yaml_scalar <- function(x) {
  if (is.logical(x)) return(tolower(as.character(x)))
  if (is.numeric(x)) return(format(x, trim = TRUE))
  x <- as.character(x)
  if (length(x) && grepl("^[A-Za-z0-9_.-]+$", x)) x
  else sprintf('"%s"', gsub('"', '\\\\"', x))
}

# Which params to write: every column mapping (*_var / *_vars) that has a value,
# plus any other param the user changed from its template default and left
# non-empty. Empty text = the template's auto default, so it is omitted.
cerp_config_params <- function(vals, defaults) {
  keep <- list()
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (grepl("_vars?$", nm)) {
      if (!is.null(v) && length(v) && !all(is.na(v)) &&
          any(nzchar(as.character(v)))) keep[[nm]] <- v
    } else {
      cur <- if (is.null(v)) "" else v
      d   <- if (is.null(defaults[[nm]])) "" else defaults[[nm]]
      same <- isTRUE(all.equal(as.character(cur), as.character(d)))
      if (any(nzchar(as.character(cur))) && !same) keep[[nm]] <- v
    }
  }
  keep
}

cerp_config_entry <- function(id, template_file, data_file, params_list) {
  lines <- c(sprintf("  - id: %s", id),
             sprintf("    template: %s", template_file),
             sprintf("    data: %s", data_file),
             "    enabled: true")
  if (length(params_list)) {
    lines <- c(lines, "    params:")
    for (nm in names(params_list)) {
      v <- params_list[[nm]]
      if (length(v) > 1) {
        lines <- c(lines, sprintf("      %s:", nm))
        for (item in v) lines <- c(lines, sprintf("        - %s", cerp_yaml_scalar(item)))
      } else {
        lines <- c(lines, sprintf("      %s: %s", nm, cerp_yaml_scalar(v)))
      }
    }
  }
  paste(lines, collapse = "\n")
}

# --- Force a plot to actually draw, to surface deferred errors ----------------
# ggplot/patchwork/gt objects build lazily — many mapping errors (non-numeric
# column, empty group) only throw when the object is PRINTED. Printing once to a
# null device inside the preview's tryCatch turns those into caught errors (a
# house-style callout) instead of a blank panel from renderPlot.
cerp_force_draw <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  print(p)
  invisible(TRUE)
}

# --- Session-only palette override --------------------------------------------
# Runs `expr` with the palette temporarily overridden by `overrides` (a named
# list role -> hex). Writes the merged palette to a THROWAWAY temp file, points
# 2.0_setup_theme.R at it via the cerp.palette_path option, and re-sources it so
# cerp_cols / cerp_palette / theme_cerp() / geom defaults all pick up the new
# colors. On exit it restores the default palette by re-sourcing from the real
# theme_colors.yml. The real file is NEVER written — the saved theme stays the
# default. With no overrides this is a plain force(expr), so nothing is re-sourced.
cerp_with_palette <- function(overrides, expr) {
  if (length(overrides)) {
    base   <- yaml::read_yaml(here::here("2_R", "theme_colors.yml"))
    merged <- utils::modifyList(base, overrides)
    tmp    <- tempfile(fileext = ".yml")
    yaml::write_yaml(merged, tmp)
    options(cerp.palette_path = tmp)
    on.exit({
      options(cerp.palette_path = NULL)
      source(here::here("2_R", "2.0_setup_theme.R"))
      unlink(tmp)
    }, add = TRUE)
    source(here::here("2_R", "2.0_setup_theme.R"))
  }
  force(expr)
}

# Palette roles the Theme tab exposes, with friendly labels. Defaults are read
# live from cerp_cols (i.e. from theme_colors.yml), so today's theme is the start.
cerp_theme_roles <- c(
  primary       = "Primary — main data series",
  secondary     = "Secondary — comparison series",
  accent        = "Accent — alerts / negative",
  neutral       = "Neutral — control / muted",
  sand          = "Sand — caution / 5th category",
  sage          = "Sage — safe / go",
  text          = "Text — titles & axis text",
  subtle        = "Subtle — subtitles & labels",
  caption_muted = "Caption — footnotes",
  grid          = "Gridlines",
  box           = "Box highlight fill",
  bg            = "Background"
)

# A native OS colour picker (HTML5 <input type=color>) wired to Shiny with a tiny
# input binding (registered once in the UI head). Zero new package dependency.
# NB: the input must NOT carry the class "shiny-bound-input" itself — Shiny adds
# that class when it binds, and its bind step SKIPS any element that already has
# it (bind.ts: `if (!id || $(el).hasClass("shiny-bound-input")) continue;`).
# Hard-coding it here was the 12B bug: every picker was skipped, input$col_*
# stayed NULL, and theme_overrides() was permanently empty.
cerp_color_input <- function(id, label, value) {
  div(class = "cerp-color-row",
      tags$label(label, `for` = id, class = "cerp-color-label"),
      tags$code(class = "cerp-color-hex", tolower(value)),
      tags$input(type = "color", id = id, value = value,
                 class = "cerp-color"))
}

cerp_color_binding_js <- "
(function(){
  function register(){
    var cerpColor = new Shiny.InputBinding();
    $.extend(cerpColor, {
      find: function(scope){ return $(scope).find('input.cerp-color'); },
      getValue: function(el){ return $(el).val(); },
      setValue: function(el, v){ $(el).val(v); },
      subscribe: function(el, cb){ $(el).on('input.cerpColor change.cerpColor', function(){
        $(el).siblings('.cerp-color-hex').text($(el).val());
        cb(true);
      }); },
      unsubscribe: function(el){ $(el).off('.cerpColor'); },
      getRatePolicy: function(){ return { policy: 'debounce', delay: 250 }; }
    });
    Shiny.inputBindings.register(cerpColor, 'cerp.colorInput');
  }
  if (window.Shiny && Shiny.inputBindings) register();
  else document.addEventListener('DOMContentLoaded', register);
})();
"

cerp_make_control <- function(p, default, all_cols, col_types, mapping) {
  id <- paste0("map_", p)
  if (grepl("_vars?$", p)) {
    multiple <- grepl("_vars$", p)
    choices  <- cerp_col_choices(p, all_cols, col_types)
    suggested <- mapping[[p]]
    sel <- if (!is.null(suggested) && !all(is.na(suggested))) suggested
           else if (!multiple && length(default) == 1 && is.character(default) &&
                    default %in% all_cols) default
           else NULL
    selectInput(inputId = id, label = p, choices = choices,
                selected = sel, multiple = multiple)
  } else if (is.logical(default)) {
    checkboxInput(inputId = id, label = p, value = isTRUE(default))
  } else if (is.numeric(default)) {
    numericInput(inputId = id, label = p, value = default)
  } else {
    val <- if (is.null(default)) "" else paste(as.character(default), collapse = ", ")
    textInput(inputId = id, label = p, value = val)
  }
}

# CSS for the callouts + app chrome, keyed off the live palette so it tracks
# theme_colors.yml. Chrome rules (12B design pass) are presentational only —
# no behavioural hooks live in CSS.
app_css <- sprintf('
  .cerp-callout { padding: 0.7rem 1.05rem; margin: 0.55rem 0; border-radius: 6px;
                  border-left: 4px solid %1$s; background: %2$s; color: %3$s;
                  font-size: 0.94rem; }
  .cerp-callout.warn { border-left-color: %4$s; background: #fbf3e3; }
  .cerp-callout.flag { border-left-color: %5$s; background: #fbeae4; }
  .cerp-callout.ok   { border-left-color: %6$s; background: #eaf3ef; }
  .cerp-callout .lbl { font-weight: 600; }
  .cerp-muted { color: %7$s; font-size: 0.85rem; }
  .cerp-step-todo { color: %7$s; font-style: italic; }

  /* Tables — same look as the 0.00 report (thin borders, roomy cells). */
  .cerp-table table { border-collapse: collapse; width: 100%%; margin: 0.6rem 0 0.2rem;
                      font-size: 1.02rem; color: %3$s; }
  .cerp-table thead th { text-align: left; font-weight: 600; padding: 0.55rem 0.8rem;
                         border-bottom: 2px solid %1$s; white-space: nowrap; }
  .cerp-table tbody td { padding: 0.5rem 0.8rem; vertical-align: top;
                         border-bottom: 1px solid #e2e8ea; }
  .cerp-table tbody tr:nth-child(even) { background: %2$s; }
  .cerp-rec-table tbody td:first-child { text-align: center; color: %1$s; font-weight: 600; }

  /* Theme tab colour rows */
  .cerp-color-row { display: flex; align-items: center; gap: 0.8rem;
                    margin: 0; max-width: 420px; padding: 0.32rem 0;
                    border-bottom: 1px solid %8$s; }
  .cerp-color-label { margin: 0; font-size: 0.92rem; flex: 1; }
  .cerp-color-hex { font-size: 0.78rem; color: %7$s; background: none; padding: 0; }
  input.cerp-color { width: 46px; height: 30px; padding: 0; border: 1px solid #d8dee2;
                     border-radius: 4px; background: none; cursor: pointer; flex: none; }

  /* --- App chrome (12B design pass) --------------------------------------- */
  .navbar { border-bottom: 3px solid %1$s; }
  .navbar-brand { font-weight: 700; }
  .navbar .nav-link.active { font-weight: 600; }

  /* Section headers: quiet small-caps in the caption font, like report strips */
  h5 { font-family: "Libre Franklin", "Franklin Gothic Medium", sans-serif;
       font-size: 0.78rem; font-weight: 700; text-transform: uppercase;
       letter-spacing: 0.09em; color: %9$s; margin: 1.5rem 0 0.6rem; }
  .sidebar h5 { margin-top: 0.6rem; }

  .btn { border-radius: 6px; }
  label, .form-label { font-size: 0.9rem; }

  /* Config-entry YAML block */
  pre.shiny-text-output { background: %2$s; border: 1px solid %8$s;
                          border-radius: 6px; padding: 0.9rem 1.1rem;
                          font-size: 0.88rem; color: %3$s; }

  /* Preview chart sits on a white card so the page bg does not fight the plot bg */
  .cerp-preview-card { background: #ffffff; border: 1px solid %8$s;
                       border-radius: 8px; padding: 1.1rem 1.2rem;
                       margin-top: 0.9rem;
                       box-shadow: 0 1px 4px rgba(27, 58, 75, 0.05); }
',
  cerp_cols[["primary"]], cerp_cols[["box"]], cerp_cols[["text"]],
  cerp_cols[["sand"]], cerp_cols[["accent"]], cerp_cols[["sage"]],
  cerp_cols[["caption_muted"]], cerp_cols[["grid"]], cerp_cols[["subtle"]])

# --- Datasets available to the app: CSVs in 1_data/ (top level; geo/ excluded) -
# Scanned at launch. Data is read in place — the app never uploads or copies it,
# so field data stays in 1_data/ where the render expects it (hard rule 1).
cerp_data_dir <- here::here("1_data")
cerp_csv_choices <- sort(list.files(cerp_data_dir, pattern = "\\.csv$",
                                    full.names = FALSE))
cerp_csv_default <- if ("master_micro_survey.csv" %in% cerp_csv_choices)
  "master_micro_survey.csv" else if (length(cerp_csv_choices)) cerp_csv_choices[[1]] else ""

# --- ONE documented template lookup (id -> Rmd file, viz_*(), previewable) -----
# The single place the app maps a recommender id to its template file and chart
# function. Chart logic is NEVER duplicated here — preview calls the same viz_*()
# the reports use (Step 6), passing the mapped params by name (the Rmd wrappers
# and viz_*() share argument names, so params flow straight through).
#   preview = FALSE for the three templates that cannot render a quick ggplot:
#     3.15 needs the sf/geo assets, 3.16 fits fixest models (slow), 3.21 returns
#     a gt table, not a ggplot. Those still map + emit config; they just show a
#     "render to see" note on the Preview tab.
cerp_templates <- list(
  "3.01" = list(rmd = "3.01_baseline_endline.Rmd",  viz = "viz_dumbbell",         preview = TRUE),
  "3.02" = list(rmd = "3.02_distribution_shifts.Rmd", viz = "viz_distribution",    preview = TRUE),
  "3.03" = list(rmd = "3.03_treatment_effects.Rmd",  viz = "viz_forest",          preview = TRUE),
  "3.04" = list(rmd = "3.04_subgroup_impacts.Rmd",   viz = "viz_coefficient",     preview = TRUE),
  "3.05" = list(rmd = "3.05_waffle_chart.Rmd",       viz = "viz_waffle",          preview = TRUE),
  "3.06" = list(rmd = "3.06_slopegraph.Rmd",         viz = "viz_slopegraph",      preview = TRUE),
  "3.07" = list(rmd = "3.07_diverging_bar.Rmd",      viz = "viz_diverging",       preview = TRUE),
  "3.08" = list(rmd = "3.08_icon_array.Rmd",         viz = "viz_icon_array",      preview = TRUE),
  "3.09" = list(rmd = "3.09_waterfall_chart.Rmd",    viz = "viz_waterfall",       preview = TRUE),
  "3.10" = list(rmd = "3.10_bump_chart.Rmd",         viz = "viz_bump",            preview = TRUE),
  "3.11" = list(rmd = "3.11_bullet_chart.Rmd",       viz = "viz_bullet",          preview = TRUE),
  "3.12" = list(rmd = "3.12_deviation_bar.Rmd",      viz = "viz_deviation",       preview = TRUE),
  "3.13" = list(rmd = "3.13_ridgeline_plot.Rmd",     viz = "viz_ridgeline",       preview = TRUE),
  "3.14" = list(rmd = "3.14_calendar_heatmap.Rmd",   viz = "viz_calendar_heatmap", preview = TRUE),
  "3.15" = list(rmd = "3.15_choropleth_map.Rmd",     viz = "viz_choropleth",      preview = FALSE),
  "3.16" = list(rmd = "3.16_event_study.Rmd",        viz = "viz_event_study",     preview = FALSE),
  "3.17" = list(rmd = "3.17_small_multiples.Rmd",    viz = "viz_small_multiples", preview = TRUE),
  "3.18" = list(rmd = "3.18_heatmap_matrix.Rmd",     viz = "viz_heatmap_matrix",  preview = TRUE),
  "3.19" = list(rmd = "3.19_consort_flow.Rmd",       viz = "viz_consort_flow",    preview = TRUE),
  "3.20" = list(rmd = "3.20_balance_plot.Rmd",       viz = "viz_balance_plot",    preview = TRUE),
  "3.21" = list(rmd = "3.21_summary_table.Rmd",      viz = "viz_summary_table",   preview = FALSE),
  "3.22" = list(rmd = "3.22_scatter_quadrant.Rmd",   viz = "viz_scatter_quadrant", preview = TRUE),
  "3.23" = list(rmd = "3.23_survival_curve.Rmd",     viz = "viz_survival_curve",  preview = TRUE)
)

# --- Expected profile type(s) per *_var param, for the Map-tab dropdowns -------
# Documented column-role expectations, keyed by param name (consistent in meaning
# across templates). A dropdown surfaces matching-type columns first ("Suggested")
# and still lists the rest ("Other columns") so a mis-detected column is never
# hard-blocked — fail-loud validation on preview catches genuinely wrong picks.
# A param not listed here accepts any column.
cerp_var_types <- list(
  group_var    = "categorical",
  before_var   = "numeric",
  after_var    = "numeric",
  outcome_var  = "numeric",
  subgroup_var = "categorical",
  status_var   = "categorical",
  entity_var   = c("categorical", "id-like", "text"),
  time_var     = c("date", "numeric"),
  value_var    = "numeric",
  response_var = "categorical",
  category_var = "categorical",
  target_var   = "numeric",
  date_var     = "date",
  region_var   = c("categorical", "id-like", "text"),
  unit_var     = c("id-like", "categorical"),
  event_time_var = "numeric",
  treat_var    = c("categorical", "numeric"),
  facet_var    = "categorical",
  x_var        = c("numeric", "date"),
  y_var        = "numeric",
  row_var      = c("categorical", "date"),
  col_var      = c("categorical", "date"),
  stage_var    = "categorical",
  n_var        = "numeric",
  arm_var      = "categorical",
  note_var     = c("text", "categorical"),
  label_var    = c("text", "categorical", "id-like"),
  event_var    = c("numeric", "categorical"),
  balance_vars = "numeric",
  summary_vars = "numeric"
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "CERP Config Builder",
  theme = cerp_app_theme,
  header = tags$head(tags$style(HTML(app_css)),
                     tags$script(HTML(cerp_color_binding_js))),

  # --- Data tab ---------------------------------------------------------------
  nav_panel(
    title = "1 · Data",
    layout_sidebar(
      sidebar = sidebar(
        title = "Dataset",
        selectInput("dataset", "CSV in 1_data/",
                    choices = cerp_csv_choices, selected = cerp_csv_default),
        p(class = "cerp-muted",
          "Read in place — nothing is uploaded. Drop a new CSV into 1_data/ ",
          "and relaunch the app to see it here.")
      ),
      cerp_callout(
        span(class = "lbl", "What this is. "),
        "A local editor for render_config.yml — it profiles your data, recommends ",
        "templates, and writes a config entry for you to paste. It does not render ",
        "reports itself; run the master knit from RStudio for that.",
        kind = "info"),
      h5("Dataset flags"),
      p(class = "cerp-muted",
        "The profile reads the RAW file (like the 0.00 report) so whitespace and ",
        "empty-column issues stay visible; the render itself uses the cleaned load."),
      uiOutput("data_flags"),
      h5("Column profile"),
      uiOutput("profile_table"),
      p(class = "cerp-muted",
        "Type is a deterministic best guess (numeric / categorical / date / ",
        "id-like / text / empty). “Whitespace” counts values with stray ",
        "spaces; “Date-parse” is the share of non-missing values ",
        "cerp_parse_date() can read; “Outliers” counts points beyond ",
        "1.5×IQR.")
    )
  ),

  # --- Recommend tab ----------------------------------------------------------
  nav_panel(
    title = "2 · Recommend",
    layout_sidebar(
      sidebar = sidebar(
        title = "Pick a template",
        p(class = "cerp-muted",
          "Deterministic suggestions from the column profile — the same rule ",
          "table the 0.00 report uses. Picking one pre-fills its column mapping ",
          "on the Map tab; you confirm it there."),
        uiOutput("rec_pick_ui"),
        uiOutput("rec_selected")
      ),
      h5("Recommended visuals"),
      uiOutput("rec_table")
    )
  ),

  # --- Map tab ----------------------------------------------------------------
  nav_panel(
    title = "3 · Map columns",
    layout_sidebar(
      sidebar = sidebar(
        title = "Template",
        uiOutput("map_header"),
        p(class = "cerp-muted",
          "Column dropdowns are grouped: expected-type columns under ",
          "“Suggested”, the rest under “Other columns”. Leave a text field ",
          "blank to keep the template’s auto-generated default.")
      ),
      uiOutput("map_controls")
    )
  ),

  # --- Theme tab (optional; recolors the preview only) ------------------------
  # Sits BEFORE Preview (12B): pick colours, then render — the natural flow.
  nav_panel(
    title = "4 · Theme",
    layout_sidebar(
      sidebar = sidebar(
        title = "Chart colours",
        width = 340,
        p(class = "cerp-muted",
          "Try alternate colours on the preview. This changes the preview and its ",
          "PNG/PDF only — it never edits theme_colors.yml, so the saved theme ",
          "stays the default."),
        uiOutput("theme_controls"),
        actionButton("theme_reset", "Reset to default", class = "btn-sm")
      ),
      cerp_callout(
        span(class = "lbl", "How this works. "),
        "Pick colours, then go to the Preview tab and press Render preview — the ",
        "chart redraws with your colours. To make a change permanent for all ",
        "reports, edit ", tags$code("2_R/theme_colors.yml"), " and run ",
        tags$code("2_R/2.7_build_css.R"), " (see CHANGING_COLORS.md).", kind = "info"),
      uiOutput("theme_swatches")
    )
  ),

  # --- Preview tab ------------------------------------------------------------
  nav_panel(
    title = "5 · Preview",
    uiOutput("preview_area")
  ),

  # --- Config tab -------------------------------------------------------------
  nav_panel(
    title = "6 · Config entry",
    uiOutput("config_area")
  ),

  nav_spacer(),
  nav_item(tags$span(class = "cerp-muted", "Local · config editor, not a render engine"))
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Shared state across tabs, populated by later steps:
  #   raw       — raw read_csv() of the chosen file (feeds cerp_profile)
  #   loaded    — cerp_load() of the same file (feeds everything downstream)
  #   profile   — cerp_profile() output
  #   recs      — cerp_recommend() output
  #   template  — the chosen template id (e.g. "3.01")
  #   mapping   — named list of *_var / text-param selections
  rv <- reactiveValues(
    raw = NULL, loaded = NULL, profile = NULL,
    recs = NULL, template = NULL, mapping = NULL
  )

  # --- Load the chosen CSV twice, exactly like the 0.00 report -----------------
  # raw (trim_ws = FALSE) feeds cerp_profile() so header/value whitespace stays
  # visible; cerp_load() feeds everything downstream. Wrapped so a bad file fails
  # loud in the UI (fail-loud, hard rule 4) instead of crashing the session.
  dataset_state <- reactive({
    req(input$dataset)
    path <- file.path(cerp_data_dir, input$dataset)
    tryCatch({
      raw    <- readr::read_csv(path, show_col_types = FALSE, trim_ws = FALSE)
      loaded <- cerp_load(path)
      prof   <- cerp_profile(raw)
      list(ok = TRUE, raw = raw, loaded = loaded, profile = prof)
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  })

  # Push a good load into shared state; changing the dataset clears downstream
  # picks so a stale template/mapping can never carry over to new data.
  observeEvent(dataset_state(), {
    st <- dataset_state()
    if (isTRUE(st$ok)) {
      rv$raw <- st$raw; rv$loaded <- st$loaded; rv$profile <- st$profile
      rv$recs <- NULL;  rv$template <- NULL;    rv$mapping <- NULL
    } else {
      rv$raw <- NULL; rv$loaded <- NULL; rv$profile <- NULL
    }
  })

  # --- Dataset-level flags -----------------------------------------------------
  output$data_flags <- renderUI({
    st <- dataset_state()
    if (!isTRUE(st$ok)) {
      return(cerp_callout(span(class = "lbl", "Could not read this file. "),
                          st$msg, kind = "flag"))
    }
    ds    <- st$profile$dataset
    items <- list(p(sprintf("The dataset has %s rows and %s columns.",
                            format(ds$n_rows, big.mark = ","), ds$n_cols)))
    if (ds$single_row)
      items <- c(items, list(cerp_callout(
        span(class = "lbl", "Single row. "),
        "One data row is not enough for most charts.", kind = "flag")))
    if (ds$duplicate_rows > 0)
      items <- c(items, list(cerp_callout(
        span(class = "lbl", sprintf("%s duplicate row(s). ", ds$duplicate_rows)),
        "Confirm these are real repeats, not an export/merge artifact.", kind = "warn")))
    if (length(ds$empty_columns) > 0)
      items <- c(items, list(cerp_callout(
        span(class = "lbl", "Empty column(s): "),
        paste(ds$empty_columns, collapse = ", "),
        ". These carry no values and are ignored by the recommender.", kind = "warn")))
    if (length(ds$header_whitespace) > 0)
      items <- c(items, list(cerp_callout(
        span(class = "lbl", "Header whitespace: "),
        paste(ds$header_whitespace, collapse = ", "),
        ". cerp_load() trims this for the render.", kind = "warn")))
    if (!ds$single_row && ds$duplicate_rows == 0 &&
        length(ds$empty_columns) == 0 && length(ds$header_whitespace) == 0)
      items <- c(items, list(cerp_callout(
        span(class = "lbl", "No dataset-level flags."),
        " No duplicates, empty columns, or header whitespace.", kind = "ok")))
    tagList(items)
  })

  # --- Per-column profile table (styled like the report) -----------------------
  output$profile_table <- renderUI({
    st <- dataset_state()
    req(isTRUE(st$ok))
    cols <- st$profile$columns
    disp <- data.frame(
      Column        = cols$column,
      Type          = cols$type,
      `Missing %`   = cols$missing_pct,
      Distinct      = cols$n_distinct,
      Whitespace    = cols$whitespace_issues,
      `Date-parse`  = ifelse(is.na(cols$date_parse_rate), "—",
                             scales::percent(cols$date_parse_rate, accuracy = 1)),
      Outliers      = ifelse(is.na(cols$outlier_count), "—",
                             as.character(cols$outlier_count)),
      check.names = FALSE, stringsAsFactors = FALSE)
    HTML(paste0('<div class="cerp-table">',
                knitr::kable(disp, format = "html", align = "llrrrrr",
                             row.names = FALSE),
                '</div>'))
  })

  # --- Recommendations ---------------------------------------------------------
  # cerp_recommend() over the loaded profile + raw data (one shared rule table
  # with the 0.00 report). Recomputed when the dataset changes.
  recommendations <- reactive({
    req(rv$profile, rv$raw)
    cerp_recommend(rv$profile, rv$raw)
  })

  # Ranked table, styled like the report.
  output$rec_table <- renderUI({
    if (is.null(rv$profile))
      return(cerp_callout("Load a dataset on the Data tab first.", kind = "info"))
    recs <- recommendations()
    if (nrow(recs) == 0)
      return(cerp_callout(
        span(class = "lbl", "No confident recommendation. "),
        "The column profile did not match any template rule cleanly — check the ",
        "flags on the Data tab (empty columns, all-text data, single row) and ",
        "confirm the right file was profiled.", kind = "flag"))
    disp <- data.frame(
      Rank                = seq_len(nrow(recs)),
      Template            = sprintf("%s — %s", recs$id, recs$template),
      `Why it fits`       = recs$why,
      `Suggested mapping` = recs$mapping,
      check.names = FALSE, stringsAsFactors = FALSE)
    HTML(paste0('<div class="cerp-table cerp-rec-table">',
                knitr::kable(disp, format = "html", align = "clll",
                             row.names = FALSE),
                '</div>'))
  })

  # Radio picker of the ranked templates; selecting one drives the mapping.
  output$rec_pick_ui <- renderUI({
    req(rv$profile)
    recs <- recommendations()
    if (nrow(recs) == 0) return(NULL)
    choices <- setNames(recs$id, sprintf("%s — %s", recs$id, recs$template))
    radioButtons("rec_pick", NULL, choices = choices,
                 selected = rv$template %||% recs$id[[1]])
  })

  # Store the chosen template + its pre-filled *_var mapping into shared state.
  observeEvent(input$rec_pick, {
    recs <- recommendations()
    row  <- recs[recs$id == input$rec_pick, , drop = FALSE]
    if (nrow(row) == 0) return()
    rv$template <- row$id[[1]]
    rv$mapping  <- cerp_parse_mapping(row$mapping[[1]])
  })

  # Confirmation of the current pick (fed forward to the Map tab).
  output$rec_selected <- renderUI({
    req(rv$template)
    recs <- recommendations()
    row  <- recs[recs$id == rv$template, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    cerp_callout(
      span(class = "lbl", sprintf("Selected: %s — %s. ", row$id[[1]], row$template[[1]])),
      "Suggested column mapping pre-filled — adjust and preview it on the Map tab.",
      kind = "ok")
  })

  # --- Map tab -----------------------------------------------------------------
  # The chosen template's editable params, read from its YAML header (combined_mode
  # and data_path are engine-managed and dropped). One reactive, reused by the Map
  # controls, the preview, and the config emitter.
  template_params <- reactive({
    req(rv$template)
    tmpl <- cerp_templates[[rv$template]]
    req(!is.null(tmpl))
    fm  <- rmarkdown::yaml_front_matter(here::here("3_templates", tmpl$rmd))
    prm <- fm$params
    prm[["combined_mode"]] <- NULL
    prm[["data_path"]]     <- NULL
    prm
  })

  # Columns offered to the dropdowns come from the cerp_load()'d data (what the
  # chart actually sees); their type comes from the profile (matched on trimmed
  # name so header whitespace can't misalign them).
  map_cols <- reactive({
    req(rv$loaded, rv$profile)
    all_cols  <- names(rv$loaded)
    prof_type <- setNames(rv$profile$columns$type,
                          stringr::str_squish(rv$profile$columns$column))
    col_types <- setNames(unname(prof_type[stringr::str_squish(all_cols)]), all_cols)
    list(all_cols = all_cols, col_types = col_types)
  })

  output$map_header <- renderUI({
    if (is.null(rv$template))
      return(cerp_callout("Pick a template on the Recommend tab first.", kind = "info"))
    tmpl <- cerp_templates[[rv$template]]
    tagList(
      strong(sprintf("%s — %s", rv$template, sub("^viz_", "", tmpl$viz))),
      if (!isTRUE(tmpl$preview))
        p(class = "cerp-muted",
          "No live preview for this one (geo/model/table template); map it, then ",
          "render to see the result.")
    )
  })

  output$map_controls <- renderUI({
    if (is.null(rv$template))
      return(NULL)
    req(rv$loaded, rv$profile)
    prm <- template_params()
    mc  <- map_cols()
    is_col <- grepl("_vars?$", names(prm))
    is_opt <- vapply(prm, function(d) is.numeric(d) || is.logical(d), logical(1)) & !is_col
    is_txt <- !is_col & !is_opt
    mk <- function(p) cerp_make_control(p, prm[[p]], mc$all_cols, mc$col_types, rv$mapping)
    tagList(
      h5("Column mapping"),
      lapply(names(prm)[is_col], mk),
      if (any(is_opt)) tagList(h5("Options"), lapply(names(prm)[is_opt], mk)),
      h5("Text & labels"),
      lapply(names(prm)[is_txt], mk),
      p(class = "cerp-muted", "Blank text = the template's auto-generated default.")
    )
  })

  # --- Current param values (Map inputs, falling back to defaults) --------------
  # Reads each map_* input; if the Map tab hasn't been rendered yet, falls back to
  # the recommender suggestion (for columns) or the template's YAML default. Shared
  # by the preview and the config emitter so both see exactly what the user set.
  current_values <- reactive({
    prm <- template_params()
    setNames(lapply(names(prm), function(p) {
      iv <- input[[paste0("map_", p)]]
      if (!is.null(iv)) return(iv)
      if (grepl("_vars?$", p) && !is.null(rv$mapping[[p]]) &&
          !all(is.na(rv$mapping[[p]]))) rv$mapping[[p]] else prm[[p]]
    }), names(prm))
  })

  # --- Preview: validate + call the SAME viz_*() the reports use ----------------
  # Fires only on the button (not per keystroke). Params flow to viz_*() by name;
  # only the function's own formals are passed (so document-only params like
  # report_title are ignored). Everything is wrapped so a bad mapping shows its
  # error verbatim instead of crashing the app.
  preview_state <- eventReactive(input$do_preview, {
    req(rv$template, rv$loaded)
    tmpl <- cerp_templates[[rv$template]]
    vals <- current_values()
    ov   <- theme_overrides()
    tryCatch({
      cerp_with_palette(ov, {
        var_vals <- vals[grepl("_var$", names(vals))]
        cerp_validate(rv$loaded, var_vals)
        fn   <- get(tmpl$viz, mode = "function")
        args <- vals[names(vals) %in% names(formals(fn))]
        args$data <- rv$loaded
        p <- do.call(fn, args)
        cerp_force_draw(p)        # surface print-time errors here, not in renderPlot
        list(ok = TRUE, plot = p, overrides = ov)
      })
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  })

  output$preview_area <- renderUI({
    if (is.null(rv$template))
      return(cerp_callout("Pick a template and map its columns first.", kind = "info"))
    tmpl <- cerp_templates[[rv$template]]
    if (!isTRUE(tmpl$preview))
      return(cerp_callout(
        span(class = "lbl", sprintf("No live preview for %s. ", rv$template)),
        "This template needs the geo assets, fits a model, or returns a table ",
        "rather than a quick chart. Map its columns, emit the config entry, and ",
        "render it from RStudio to see the result.", kind = "info"))
    tagList(
      div(class = "cerp-muted",
          sprintf("Previews %s with the current Map-tab settings.", rv$template)),
      actionButton("do_preview", "Render preview", class = "btn-primary"),
      br(), br(),
      uiOutput("preview_err"),
      div(class = "cerp-preview-card",
          plotOutput("preview_plot", height = "460px")),
      uiOutput("preview_downloads")
    )
  })

  # Download the previewed chart. PNG (raster) and PDF (vector, for print) come
  # from the exact plot object shown; only offered after a successful preview.
  # The full HTML report is a different artifact — produced by the master knit
  # from the config entry (Config tab), not by this preview.
  output$preview_downloads <- renderUI({
    st <- preview_state()
    if (!isTRUE(st$ok)) return(NULL)
    tagList(
      hr(),
      downloadButton("dl_png", "Download PNG", class = "btn-sm"),
      downloadButton("dl_pdf", "Download PDF", class = "btn-sm"),
      p(class = "cerp-muted",
        "For the full HTML report, emit the config entry (next tab) and run the ",
        "master knit from RStudio.")
    )
  })

  output$dl_png <- downloadHandler(
    filename = function() sprintf("%s_preview.png", rv$template),
    content  = function(file) {
      st <- preview_state(); req(isTRUE(st$ok))
      bgc <- st$overrides$bg %||% cerp_cols[["bg"]]
      cerp_with_palette(st$overrides,
        ggplot2::ggsave(file, plot = st$plot, width = 9, height = 4.5,
                        dpi = 300, bg = bgc))
    }
  )
  output$dl_pdf <- downloadHandler(
    filename = function() sprintf("%s_preview.pdf", rv$template),
    content  = function(file) {
      st <- preview_state(); req(isTRUE(st$ok))
      bgc <- st$overrides$bg %||% cerp_cols[["bg"]]
      cerp_with_palette(st$overrides,
        ggplot2::ggsave(file, plot = st$plot, width = 9, height = 4.5,
                        device = grDevices::cairo_pdf, bg = bgc))
    }
  )

  output$preview_err <- renderUI({
    st <- preview_state()
    if (isTRUE(st$ok)) return(NULL)
    cerp_callout(span(class = "lbl", "Preview failed. "), st$msg, kind = "flag")
  })

  output$preview_plot <- renderPlot({
    st <- preview_state()
    req(isTRUE(st$ok))
    cerp_with_palette(st$overrides, print(st$plot))
  }, bg = cerp_cols[["bg"]])

  # --- Config entry: the copy-paste YAML (Q2 answer: never rewrite the file) ----
  # Builds one override-only `reports:` entry from the current Map-tab settings.
  config_text <- reactive({
    req(rv$template, rv$loaded)
    tmpl <- cerp_templates[[rv$template]]
    stem <- sub("^3\\.[0-9]+_", "", sub("\\.Rmd$", "", tmpl$rmd))
    id   <- if (!is.null(input$cfg_id) && nzchar(input$cfg_id)) input$cfg_id else stem
    pl   <- cerp_config_params(current_values(), template_params())
    cerp_config_entry(id, tmpl$rmd, input$dataset, pl)
  })

  output$config_area <- renderUI({
    if (is.null(rv$template) || is.null(rv$loaded))
      return(cerp_callout("Pick a template and dataset first.", kind = "info"))
    tmpl <- cerp_templates[[rv$template]]
    stem <- sub("^3\\.[0-9]+_", "", sub("\\.Rmd$", "", tmpl$rmd))
    tagList(
      p("Paste this under the ", tags$code("reports:"), " block in ",
        tags$code("render_config.yml"), ", then render with the master knit ",
        "(", tags$code("source(here::here(\"2_R\", \"2.2_master_knit.R\"))"), "). ",
        "Only your column mapping and changed settings are written — the config ",
        "is override-only, so anything left at its default falls back automatically."),
      textInput("cfg_id", "id (also the output filename stem)", value = stem),
      verbatimTextOutput("config_yaml"),
      downloadButton("dl_config", "Download .yml snippet", class = "btn-sm"),
      cerp_callout(
        span(class = "lbl", "Chart title & subtitle. "),
        "Set ", tags$code("chart_title"), " / ", tags$code("chart_subtitle"),
        " on the Map tab — changing them writes them into this entry.", kind = "info")
    )
  })

  output$config_yaml <- renderText(config_text())

  output$dl_config <- downloadHandler(
    filename = function() sprintf("%s_config_entry.yml", rv$template),
    content  = function(file) writeLines(config_text(), file)
  )

  # --- Theme tab: session-only colour overrides --------------------------------
  # Pickers default to the live palette (cerp_cols). Reset re-renders them, which
  # snaps every picker back to the theme_colors.yml default. The overrides feed
  # only the preview + its downloads (see preview_state / renderPlot / ggsave);
  # theme_colors.yml is never touched.
  theme_reset_n <- reactiveVal(0)
  observeEvent(input$theme_reset, theme_reset_n(theme_reset_n() + 1))

  output$theme_controls <- renderUI({
    theme_reset_n()  # dependency: re-render (reset to defaults) when bumped
    lapply(names(cerp_theme_roles), function(role) {
      cerp_color_input(paste0("col_", role), cerp_theme_roles[[role]],
                       value = unname(cerp_cols[[role]]))
    })
  })

  theme_overrides <- reactive({
    ov <- list()
    for (role in names(cerp_theme_roles)) {
      v <- input[[paste0("col_", role)]]
      if (!is.null(v) && nzchar(v) &&
          !identical(tolower(v), tolower(unname(cerp_cols[[role]])))) ov[[role]] <- v
    }
    ov
  })

  # Live swatches of the currently-chosen palette (updates as pickers change).
  output$theme_swatches <- renderUI({
    theme_reset_n()
    vals <- vapply(names(cerp_theme_roles), function(r) {
      v <- input[[paste0("col_", r)]]
      if (is.null(v) || !nzchar(v)) unname(cerp_cols[[r]]) else v
    }, character(1))
    tagList(
      h5("Current palette"),
      div(style = "display:flex; flex-wrap:wrap; gap:12px; margin-top:6px;",
          lapply(names(vals), function(r) {
            div(style = "display:flex; align-items:center; gap:6px;",
                div(style = sprintf(paste0("width:22px; height:22px; border-radius:4px;",
                                           " border:1px solid #ccc; background:%s;"), vals[[r]])),
                span(class = "cerp-muted", r))
          }))
    )
  })
}

shinyApp(ui, server)
