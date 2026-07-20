# ==============================================================================
# Script Name: 2.0_setup_theme.R
# Author:      Ibraheem Saqib Ellahi <ibraheemsaqib90@gmail.com>
# Purpose:     Load packages and define the CERP palette, fonts, and ggplot theme
# Design spec: Design.md — text #1b3a4b | graphs #457b9d, #a8dadc | boxes #e8f4f5
#              bg #FAF9F6 | body font Charter/Georgia | captions Libre Franklin
# Colors:      All brand hexes live in 2_R/theme_colors.yml (single source of
#              truth). Edit hexes THERE, never here. This script only maps those
#              named colors onto roles/palettes and fixes fonts + layout.
# ==============================================================================

# 1. Package Management --------------------------------------------------------
# renv owns the environment — NEVER install at render time (hard rule 5).
# This block is self-contained (no helpers dependency) because this script is
# also sourced standalone, e.g. for the palette quick-check in CHANGING_COLORS.md.
local({
  pkgs <- c(
    "tidyverse",   # Core data manipulation and ggplot2
    "janitor",     # Clean column names
    "scales",      # Number and percentage formatting for axes
    "here",        # Safe relative file paths
    "ggtext",      # Markdown text rendering in plots
    "patchwork",   # Stitching multiple plots together
    "systemfonts", # Detect which fonts are installed on this machine
    "yaml"         # Read the externalized brand palette (theme_colors.yml)
  )
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(!ok)) {
    stop(
      "Missing package(s): ", paste(pkgs[!ok], collapse = ", "),
      "\nThis project uses renv. Run renv::restore() from the project root, ",
      "then re-render.",
      call. = FALSE
    )
  }
  for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
})

# 2. Institutional Colors (Design.md) ------------------------------------------
# Load the brand hexes from the one editable palette file, then map them onto
# named roles. `positive`/`negative` are semantic aliases; everything is a
# reference to a loaded hex, so a change in theme_colors.yml propagates here.
#
# The palette path defaults to theme_colors.yml (the single source of truth). The
# `cerp.palette_path` option lets a caller point this at a DIFFERENT palette file
# WITHOUT editing theme_colors.yml — used only by the Shiny config-builder to
# preview alternative colors from a throwaway temp file. Unset (the normal case)
# it is exactly the original read, so every render is unaffected.
cerp_palette_path <- getOption("cerp.palette_path",
                               here::here("2_R", "theme_colors.yml"))
cerp_hex <- yaml::read_yaml(cerp_palette_path)

cerp_cols <- c(
  text          = cerp_hex$text,      # Deep slate — titles, axis text, anchor lines
  primary       = cerp_hex$primary,   # Steel blue — main data series, "good"/positive
  secondary     = cerp_hex$secondary, # Pale teal — comparison series, light fills
  box           = cerp_hex$box,       # Text-box / panel highlight fill
  bg            = cerp_hex$bg,         # Warm off-white page background
  neutral       = cerp_hex$neutral,   # Muted slate — control groups, de-emphasized series
  accent        = cerp_hex$accent,    # Terracotta — warnings, negative deviations
  positive      = cerp_hex$primary,   # Semantic alias → primary
  negative      = cerp_hex$accent,    # Semantic alias → accent
  subtle        = cerp_hex$subtle,    # Subtitles, axis labels, secondary text
  sand          = cerp_hex$sand,      # Sand — caution / 5th categorical
  sage          = cerp_hex$sage,      # Muted sage — safe / go
  grid          = cerp_hex$grid,      # Horizontal gridlines
  caption_muted = cerp_hex$caption_muted # Plot caption / footnote text
)

# Categorical palette: ordered for maximum adjacent contrast.
# Built by name reference (not re-typed hexes) so it always tracks cerp_cols.
cerp_palette <- unname(cerp_cols[c("primary", "secondary", "accent", "text", "sand", "neutral")])

# Return n categorical colors (interpolates beyond 6)
cerp_fill_n <- function(n) {
  if (n <= length(cerp_palette)) return(cerp_palette[seq_len(n)])
  colorRampPalette(cerp_palette)(n)
}

# 3. Semantic Colors ------------------------------------------------------------
# When category names carry universal meaning (triage results, risk levels,
# yes/no), color should reinforce the meaning — muted to stay on-brand.
# Mapped by name reference to cerp_cols, so triage/risk colors also follow the
# palette file rather than re-hardcoding hexes.
cerp_semantic <- c(
  "green"       = cerp_cols[["sage"]],    # Muted sage — safe / go
  "yellow"      = cerp_cols[["sand"]],    # Sand — caution
  "amber"       = cerp_cols[["sand"]],
  "red"         = cerp_cols[["accent"]],  # Terracotta — alert
  "yes"         = cerp_cols[["primary"]],
  "no"          = cerp_cols[["neutral"]],
  "high risk"   = cerp_cols[["accent"]],
  "medium risk" = cerp_cols[["sand"]],
  "low risk"    = cerp_cols[["sage"]]
)

# Given category levels, return a named color vector if ALL levels have a
# semantic meaning; otherwise NULL (caller falls back to cerp_fill_n()).
cerp_match_semantic <- function(levels) {
  keys <- tolower(trimws(levels))
  if (all(keys %in% names(cerp_semantic))) {
    setNames(unname(cerp_semantic[keys]), levels)
  } else {
    NULL
  }
}

# 4. Fonts ----------------------------------------------------------------------
# First installed candidate wins; graceful fallback keeps renders portable.
cerp_pick_font <- function(candidates, fallback) {
  installed <- unique(systemfonts::system_fonts()$family)
  hit <- candidates[candidates %in% installed]
  if (length(hit) > 0) hit[1] else fallback
}

cerp_font_body    <- cerp_pick_font(c("Charter", "Georgia"), "serif")
cerp_font_caption <- cerp_pick_font(c("Libre Franklin", "Franklin Gothic Medium"), "sans")

# 5. Global ggplot2 Theme --------------------------------------------------------
theme_cerp <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = cerp_font_body) +
    theme(
      # --- Text hierarchy ---
      text = element_text(color = cerp_cols[["text"]]),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        face = "bold",
        size = rel(1.35),
        color = cerp_cols[["text"]],
        lineheight = 1.15,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = rel(1.02),
        color = cerp_cols[["subtle"]],
        lineheight = 1.25,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        family = cerp_font_caption,
        size = rel(0.78),
        color = cerp_cols[["caption_muted"]],
        hjust = 0,
        margin = margin(t = 16)
      ),

      # --- Grid and Axes: only what aids reading ---
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = cerp_cols[["grid"]], linewidth = 0.45),
      axis.title.x = element_text(
        family = cerp_font_caption, size = rel(0.85),
        color = cerp_cols[["subtle"]], margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        family = cerp_font_caption, size = rel(0.85),
        color = cerp_cols[["subtle"]], margin = margin(r = 10)
      ),
      axis.text = element_text(
        family = cerp_font_caption, size = rel(0.85),
        color = cerp_cols[["subtle"]]
      ),
      axis.ticks = element_blank(),

      # --- Legend: top-left, quiet ---
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(
        family = cerp_font_caption, size = rel(0.9),
        color = cerp_cols[["text"]]
      ),
      legend.key.size = unit(0.9, "lines"),

      # --- Facets ---
      strip.text = element_text(
        family = cerp_font_caption, face = "bold",
        size = rel(0.95), color = cerp_cols[["text"]],
        hjust = 0, margin = margin(b = 6)
      ),
      strip.background = element_blank(),

      # --- Canvas ---
      plot.background = element_rect(fill = cerp_cols[["bg"]], color = NA),
      panel.background = element_rect(fill = cerp_cols[["bg"]], color = NA),
      plot.margin = margin(18, 22, 14, 18)
    )
}

# 6. Geom Defaults ---------------------------------------------------------------
update_geom_defaults("bar",   list(fill  = cerp_cols[["primary"]]))
update_geom_defaults("col",   list(fill  = cerp_cols[["primary"]]))
update_geom_defaults("point", list(color = cerp_cols[["primary"]]))
update_geom_defaults("line",  list(color = cerp_cols[["primary"]]))
update_geom_defaults("text",  list(color = cerp_cols[["text"]], family = cerp_font_caption))
