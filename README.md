# CERP Analytics: Automated Visualization Suite

A modular, parameterized R Markdown reporting engine. It turns raw CSV/Excel data
into policy-ready **HTML** reports without requiring the user to write any R code.
You edit one config file (`render_config.yml`); the engine renders every visual and,
optionally, stitches selected ones into a single combined report.

## Repository Architecture

* **/1_data** — **The Drop Zone:** Place raw `.csv` or `.xlsx` files here. Only the
  synthetic `master_*.csv` demo data and `geo/` are tracked in git; all other field
  data is gitignored and never leaves the machine.
* **/2_R** — **The Engine Room:** setup theme (`2.0_setup_theme.R`), brand palette
  (`theme_colors.yml`), Excel→CSV converter (`2.3_excel_to_csv.R`), shared helpers
  (`2.5_helpers.R`), **all chart logic as `viz_*()` functions** (`2.6_viz_functions.R`),
  the config-driven production render (`2.2_master_knit.R`), the testing render
  (`2.4_test_knit.R`), and the CSS generator (`2.7_build_css.R`).
* **/3_templates** — **The Factory:** 18 parameterized `.Rmd` templates (3.01–3.18),
  one visualization each, plus the combined-report parent (`3.00_combined_report.Rmd`).
  Each template is a thin wrapper: it loads and validates the data, then calls its
  `viz_*()` function from `2_R/2.6_viz_functions.R` (chart code lives there, never in
  the Rmd). You never edit these — you point them at data and set labels via
  `render_config.yml`.
* **/3_templates_testing** — the same template bodies pointed at messy real data, for
  stress-testing. Rendered by `2.4_test_knit.R`.
* **/4_output** — **The Deliverables:** generated HTML reports and figures.
* **/5_tests** — **The Safety Net:** `snapshot.R`, a golden-figure regression test that
  hashes every rendered figure so a refactor can be proven to change no pixels. See
  `5_tests/README.md`.

## Prerequisites & Setup

1. Install **R** and **RStudio**.
2. Download the repository and **always open `cerp_viz_repo.Rproj` first** so paths
   resolve from the project root (every script uses `here::here()`).
3. Restore the reproducible package environment from the R console:
   ```r
   renv::restore()
   ```
   `renv` owns the environment. Templates **never** install packages at render time —
   a missing package fails loudly and tells you to run `renv::restore()`. Output is
   HTML; no LaTeX/tinytex is required.

## Reproducing on another computer

The repo is self-contained; a fresh machine needs three things beyond the files:

1. **R (matching version).** `renv.lock` pins **R 4.4.1** and every package. A nearby
   4.4.x is normally fine. Opening `cerp_viz_repo.Rproj` runs `.Rprofile`, which
   auto-activates `renv` (bootstrapping it if absent) — the `renv/library/` folder is
   **not** in git, so the packages themselves come from `renv::restore()`:
   ```r
   renv::restore()   # installs the exact package versions from renv.lock
   ```
2. **Fonts (for faithful output).** The house style uses **Charter** (body, falls back
   to Georgia → generic serif) and **Libre Franklin** (titles/captions, falls back to
   Franklin Gothic Medium → generic sans). Renders succeed without them, but to
   reproduce the intended look install those two fonts system-wide before rendering.
   Because font rasterization differs across machines, figures will not be *pixel*-identical
   to another computer's — see the snapshot note below.
3. **System libraries for `sf`** (only for the 3.15 choropleth): **GDAL, GEOS, PROJ**
   must be installed at the OS level (e.g. `brew install gdal` on macOS, or the
   `libgdal-dev libgeos-dev libproj-dev` packages on Debian/Ubuntu). Every other
   template is pure R.

What travels in git: the code, `renv.lock`, the synthetic `master_*.csv` demo data, and
the spatial assets under `1_data/geo/`. What does **not**: installed packages
(`renv/library/`), any field data in `1_data/`, and everything generated in `4_output/`.
So on a new machine you clone, `renv::restore()`, then render (below) to regenerate the
outputs.

**Snapshot manifest is machine-specific.** `5_tests/snapshots/manifest.csv` records
figure hashes, and those depend on the local font rasterizer, so the committed manifest
from one computer will *not* match another. On a new machine, re-baseline before using
the harness: delete `5_tests/snapshots/manifest.csv`, run
`source(here::here("5_tests", "snapshot.R"))` once to record a fresh baseline, then use
compare runs after that. (Details in `5_tests/README.md`.)

## Quick Start

1. **Drop data:** put your `.csv` (or `.xlsx`, auto-converted) into `/1_data`.
2. **Edit `render_config.yml`** (the only file you touch — never the R or the `.Rmd`):
   * Each entry under `reports:` renders one HTML into `/4_output`. Give it a unique
     `id`, the `template:` filename, and (optionally) a `data:` filename resolved
     under `1_data/`.
   * Under `params:` list **only** what you want to override — the column-referencing
     `*_var` params (e.g. `group_var: treatment_group`), labels, and report text.
     Anything you omit falls back to the template's own default.
   * Set `enabled: false` to skip an entry without deleting it.
   * The `combined:` block stitches selected `id`s (in the order listed under
     `include:`) into one self-contained HTML report.
3. **Generate:** open `2_R/2.2_master_knit.R` and click **Source** (or run
   `source(here::here("2_R", "2.2_master_knit.R"))`). The engine validates the whole
   config up front, renders each enabled entry (one failure never stops the batch),
   then builds the combined report. Collect everything from `/4_output`.

Column names in `*_var` params are validated against the actual data on load: a typo
produces a clear error naming the missing column and the closest match, not a
silently-wrong chart.

## Template Dictionary

Choose the right template for your policy narrative (set it as `template:` in the config):

* **3.01 Dumbbell Plot** — absolute magnitude of change over time (Before vs. After).
* **3.02 Distribution Shift** — compare the spread of a continuous outcome between groups.
* **3.03 Forest Plot** — statistical significance of impact (Treatment vs. Control CIs).
* **3.04 Subgroup Impacts** — differential effects across population segments/cohorts.
* **3.05 Waffle Chart** — proportional adoption rates (out of 100).
* **3.06 Slopegraph** — clean longitudinal trajectory and ranking changes.
* **3.07 Diverging Stacked Bar** — household survey / Likert sentiment.
* **3.08 Icon Array** — humanize sample attrition counts and population sizes.
* **3.09 Waterfall Chart** — budget pipelines, additions, and systemic losses.
* **3.10 Bump Chart** — regional rank changes over time (league tables).
* **3.11 Bullet Chart** — entity performance against KPIs and target zones.
* **3.12 Deviation Bar Chart** — outlier performance relative to a baseline average.
* **3.13 Ridgeline Plot** — distribution shifts across a population over time.
* **3.14 Calendar Heatmap** — high-frequency daily activity and operational consistency.
* **3.15 Choropleth Map** — a district-level map (uses `sf`, a geojson, and a versioned
  district name crosswalk in `1_data/geo/`; `sf` needs system GDAL/GEOS/PROJ).
* **3.16 Event-Study Plot** — dynamic treatment effects around an event time (`fixest`).
* **3.17 Small Multiples** — one small faceted panel per unit for at-a-glance comparison.
* **3.18 Heatmap Matrix** — a row × column value grid (e.g. district × year).

`3.00_combined_report.Rmd` is the parent that stitches the visuals you list under
`combined.include` in the config into a single HTML report — you don't edit it directly.

## Changing Brand Colors

All brand colors live in one file: **`2_R/theme_colors.yml`**. Edit a hex there and
every chart across all 18 templates updates on the next render — you never touch R code.
The five colors that also style the HTML page chrome (`bg`, `text`, `box`, `subtle`,
`primary`) drive `2_R/cerp_style.css`, which is **generated** from the palette: after
changing one of those, run `source(here::here("2_R", "2.7_build_css.R"))` once to
regenerate the CSS, then re-knit. Full instructions and how to revert: see
[CHANGING_COLORS.md](CHANGING_COLORS.md).

## Troubleshooting

* **A template fails during render.** Read the error — it usually names the exact
  column or parameter at fault. Check that your `*_var` names in `render_config.yml`
  match your data headers (case-sensitive) and that key columns aren't all `NA`.
* **"Missing package … run `renv::restore()`".** The environment is out of sync with
  `renv.lock`; run `renv::restore()` from the project root, then re-render.
* **Testing vs. production outputs.** `2.4_test_knit.R` writes to the same `/4_output`
  folder and shares filenames with production (e.g. `3.06_slopegraph.html`); a testing
  render overwrites the production HTML of the same id, and vice versa.
