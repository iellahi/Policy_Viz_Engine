# ==============================================================================
# Script Name: 2.7_build_css.R
# Purpose:     Generate 2_R/cerp_style.css (the HTML page chrome) FROM the brand
#              palette in 2_R/theme_colors.yml, so the five brand hexes used by
#              the page chrome (background, text, code boxes, subtitles, links)
#              can never drift from the single source of truth.
# When to run: after editing a color in theme_colors.yml (see CHANGING_COLORS.md).
# Output:      2_R/cerp_style.css — tracked (generated + committed). Do NOT edit
#              that file by hand; edit COLOR in theme_colors.yml and STRUCTURE
#              here, then re-run this script.
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
  for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
})

# 1. Read the brand palette (single source of truth) ---------------------------
cerp_hex <- yaml::read_yaml(here::here("2_R", "theme_colors.yml"))

# 2. CSS template ---------------------------------------------------------------
# The style rules below ARE the page chrome, kept exactly as hand-authored; the
# five brand hexes are left as {{PLACEHOLDER}} tokens and substituted from
# theme_colors.yml so they can never drift. Only the header comment differs from
# a hand-written file: it flags this file as generated.
css_template <- '/* CERP report styling — injected into every template\47s HTML output.
   GENERATED — edit theme_colors.yml and run 2.7_build_css.R.

   This file is the HTML page chrome (page background, text, code boxes, links).
   It is regenerated from 2_R/theme_colors.yml (the single source of truth for
   brand color) by 2_R/2.7_build_css.R — do NOT edit it by hand; changes are
   overwritten on the next build. Change a color in theme_colors.yml, then run
   2.7_build_css.R. Fonts/layout stay fixed in the theme (design is centralized).
   Design.md: Charter/Georgia body, Libre Franklin headings & captions. */

body {
  background-color: {{BG}};
  color: {{TEXT}};
  font-family: Charter, Georgia, "Times New Roman", serif;
  font-size: 16px;
  line-height: 1.55;
}

.main-container {
  max-width: 880px;
}

h1, h2, h3, h4, h5 {
  font-family: "Libre Franklin", "Helvetica Neue", Arial, sans-serif;
  color: {{TEXT}};
  font-weight: 600;
}

h1.title {
  font-size: 1.85rem;
  margin-bottom: 0.25rem;
}

h4.author, h4.date {
  font-family: "Libre Franklin", "Helvetica Neue", Arial, sans-serif;
  font-weight: 400;
  color: {{SUBTLE}};
  font-size: 0.9rem;
}

pre, code {
  background-color: {{BOX}};
  border: none;
  border-radius: 4px;
  color: {{TEXT}};
}

blockquote {
  background-color: {{BOX}};
  border-left: 4px solid {{PRIMARY}};
  color: {{TEXT}};
  padding: 10px 16px;
}

a { color: {{PRIMARY}}; }

.btn-default, .btn {
  font-family: "Libre Franklin", Arial, sans-serif;
  font-size: 0.8rem;
}

img { background-color: {{BG}}; }
'

# 3. Substitute the five brand hexes -------------------------------------------
css <- css_template
css <- gsub("{{BG}}",      cerp_hex$bg,      css, fixed = TRUE)
css <- gsub("{{TEXT}}",    cerp_hex$text,    css, fixed = TRUE)
css <- gsub("{{BOX}}",     cerp_hex$box,     css, fixed = TRUE)
css <- gsub("{{SUBTLE}}",  cerp_hex$subtle,  css, fixed = TRUE)
css <- gsub("{{PRIMARY}}", cerp_hex$primary, css, fixed = TRUE)

# 4. Write it (cat, not writeLines: preserve exact bytes / single trailing \n) --
out_path <- here::here("2_R", "cerp_style.css")
cat(css, file = out_path)
message("Wrote ", out_path)
