# ==============================================================================
# Script Name: 00_setup_theme.R
# Purpose:     Install/load required packages and define the global CERP ggplot theme
# ==============================================================================

# 1. Package Management --------------------------------------------------------
# pacman automatically installs missing packages and loads them
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,   # Core data manipulation and ggplot2
  janitor,     # Clean column names
  scales,      # Number and percentage formatting for axes
  here,        # Safe relative file paths
  ggtext,      # Advanced markdown text rendering in plots
  patchwork    # Stitching multiple plots together seamlessly
)

# 2. Institutional Colors ------------------------------------------------------
# Utilizing standard policy-research blues and teals
cerp_cols <- c(
  primary   = "#002B5E", # Deep Navy Blue (Headers, main text)
  secondary = "#00A9A5", # Teal (Treatment group or highlights)
  neutral   = "#B0BEC5", # Grey (Control group or baseline data)
  accent    = "#F2A900", # Warm Gold (Warning or secondary highlight)
  text      = "#333333", # Dark Grey for standard text readability
  bg        = "#FFFFFF"  # Pure white background
)

# 3. Global ggplot2 Theme ------------------------------------------------------
theme_cerp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text elements
      text = element_text(color = cerp_cols["text"]),
      plot.title = element_text(
        face = "bold", 
        size = rel(1.4), 
        color = cerp_cols["primary"], 
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = rel(1.1), 
        color = "grey40", 
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = rel(0.8), 
        color = "grey50", 
        hjust = 0, 
        margin = margin(t = 15)
      ),
      
      # Grid and Axes
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#EAEAEA", linewidth = 0.5),
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
      axis.text = element_text(color = "grey30"),
      
      # Legend and Facets
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(1)),
      strip.text = element_text(
        face = "bold", 
        size = rel(1.1), 
        color = cerp_cols["primary"]
      ),
      strip.background = element_rect(fill = "#F5F5F5", color = NA),
      
      # Plot background
      plot.background = element_rect(fill = cerp_cols["bg"], color = NA),
      panel.background = element_rect(fill = cerp_cols["bg"], color = NA)
    )
}

# 4. Global Scale Defaults (Optional but helpful) ------------------------------
# Forces ggplot to use your colors by default when filling or coloring
update_geom_defaults("bar", list(fill = cerp_cols["primary"]))
update_geom_defaults("point", list(color = cerp_cols["primary"]))
update_geom_defaults("line", list(color = cerp_cols["primary"]))