# ==============================================================================
# Script Name: 2.0_setup_theme.R
# Purpose:     Load packages and define the CERP palette, fonts, and ggplot theme
# Design spec: Design.md — text #1b3a4b | graphs #457b9d, #a8dadc | boxes #e8f4f5
#              bg #FAF9F6 | body font Charter/Georgia | captions Libre Franklin
# ==============================================================================

# 1. Package Management --------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,   # Core data manipulation and ggplot2
  janitor,     # Clean column names
  scales,      # Number and percentage formatting for axes
  here,        # Safe relative file paths
  ggtext,      # Markdown text rendering in plots
  patchwork,   # Stitching multiple plots together
  systemfonts  # Detect which fonts are installed on this machine
)

# 2. Institutional Colors (Design.md) ------------------------------------------
cerp_cols <- c(
  text      = "#1b3a4b", # Deep slate — titles, axis text, anchor lines
  primary   = "#457b9d", # Steel blue — main data series, "good"/positive
  secondary = "#a8dadc", # Pale teal — comparison series, light fills
  box       = "#e8f4f5", # Text-box / panel highlight fill
  bg        = "#FAF9F6", # Warm off-white page background
  neutral   = "#b7c9d3", # Muted slate — control groups, de-emphasized series
  accent    = "#e07a5f", # Terracotta — warnings, negative deviations
  positive  = "#457b9d", # Semantic alias
  negative  = "#e07a5f", # Semantic alias
  subtle    = "#54707f"  # Subtitles, axis labels, secondary text
)

# Categorical palette: ordered for maximum adjacent contrast
cerp_palette <- c("#457b9d", "#a8dadc", "#e07a5f", "#1b3a4b", "#f2cc8f", "#b7c9d3")

# Return n categorical colors (interpolates beyond 6)
cerp_fill_n <- function(n) {
  if (n <= length(cerp_palette)) return(cerp_palette[seq_len(n)])
  colorRampPalette(cerp_palette)(n)
}

# 3. Semantic Colors ------------------------------------------------------------
# When category names carry universal meaning (triage results, risk levels,
# yes/no), color should reinforce the meaning — muted to stay on-brand.
cerp_semantic <- c(
  "green"       = "#74a892", # Muted sage — safe / go
  "yellow"      = "#f2cc8f", # Sand — caution
  "amber"       = "#f2cc8f",
  "red"         = "#e07a5f", # Terracotta — alert
  "yes"         = "#457b9d",
  "no"          = "#b7c9d3",
  "high risk"   = "#e07a5f",
  "medium risk" = "#f2cc8f",
  "low risk"    = "#74a892"
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
        color = "#8a9aa3",
        hjust = 0,
        margin = margin(t = 16)
      ),

      # --- Grid and Axes: only what aids reading ---
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#e5e1d8", linewidth = 0.45),
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
