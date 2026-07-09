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
* **/3_templates** — **The Factory:** 23 parameterized `.Rmd` templates (3.01–3.23),
  one visualization each, plus the combined-report parent (`3.00_combined_report.Rmd`).
  Each template is a thin wrapper: it loads and validates the data, then calls its
  `viz_*()` function from `2_R/2.6_viz_functions.R` (chart code lives there, never in
  the Rmd). You never edit these — you point them at data and set labels via
  `render_config.yml`.
* **/3_templates_testing** — the same template bodies pointed at messy real data, for
  stress-testing. Rendered by `2.4_test_knit.R`.
* **/4_output** — **The Deliverables:** generated HTML reports and figures, plus
  `index.html` — a static gallery (thumbnail card per report) built automatically at
  the end of every production knit. Open it first to browse everything rendered.
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

Clone the repo:

```bash
git clone https://github.com/iellahi/Cerp_Viz_Repo.git
cd Cerp_Viz_Repo
```

Then, in RStudio:

1. Open **`cerp_viz_repo.Rproj`** — this runs `.Rprofile`, which auto-activates `renv`
   (bootstrapping it if absent) and sets the project root for all `here::here()` paths.
2. In the R console, restore the exact package versions from `renv.lock`:
   ```r
   renv::restore()
   ```
3. Render everything:
   ```r
   source(here::here("2_R", "2.2_master_knit.R"))
   ```
   The configured reports + the combined report land in `4_output/`.

### What the machine needs

- **R (matching version).** `renv.lock` pins **R 4.4.1**; a nearby 4.4.x is normally
  fine. `renv/library/` is **not** in git, so packages come from `renv::restore()`.
- **Fonts (for faithful output).** The house style uses **Charter** (body, falls back
  to Georgia → generic serif) and **Libre Franklin** (titles/captions, falls back to
  Franklin Gothic Medium → generic sans). Renders succeed without them, but install
  both system-wide to reproduce the intended look. Font rasterization differs across
  machines, so figures will not be *pixel*-identical to another computer's.
- **System libraries for `sf`** (only the 3.15 choropleth): **GDAL, GEOS, PROJ** at the
  OS level — `brew install gdal geos proj` (macOS) or `libgdal-dev libgeos-dev
  libproj-dev` (Debian/Ubuntu). If `2.2_master_knit.R` errors *only* on the choropleth
  entries, this is why. Every other template is pure R.

What travels in git: the code, `renv.lock`, the synthetic `master_*.csv` demo data, the
spatial assets under `1_data/geo/`, and the test manifest in `5_tests/`. What does
**not**: installed packages (`renv/library/`), any field data in `1_data/`, and
everything generated in `4_output/`.

## Regression tests (`5_tests`)

`5_tests/snapshot.R` proves a refactor changed no pixels: it renders the full production
suite and SHA-256-hashes every figure in `4_output/figures/`, comparing against the
golden manifest `5_tests/snapshots/manifest.csv` (tracked in git). Run it with:

```r
source(here::here("5_tests", "snapshot.R"))
```

- With `manifest.csv` present (it is committed), this runs in **compare** mode and
  reports `0 mismatches ...` when nothing changed, or lists any figure whose hash moved.
- With `manifest.csv` absent, it runs in **record** mode and writes a fresh baseline.

**On a different computer, re-baseline first.** Figure hashes depend on the local font
rasterizer, so the committed manifest will not match another machine. Delete
`5_tests/snapshots/manifest.csv`, run the line above once to record a new baseline, then
use compare runs after that. Full details: `5_tests/README.md`.

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
4. **Browse:** open `4_output/index.html` — a static gallery with one thumbnail card
   per rendered report (first figure PNG; a palette placeholder tile for table-only
   outputs like 3.21) plus a featured card for the combined report. It's rebuilt on
   every knit, opens straight from disk, and needs no server or extra dependencies.

Column names in `*_var` params are validated against the actual data on load: a typo
produces a clear error naming the missing column and the closest match, not a
silently-wrong chart.

## Pre-flight Data Quality Report (`0.00`)

Before picking a visual, profile a freshly dropped CSV with the pre-flight QA report.
It is **per-dataset, not per-config** — deliberately *not* part of `2.2_master_knit.R` —
so render it on its own from the R console (project root):

```r
rmarkdown::render(
  here::here("3_templates", "0.00_data_quality_report.Rmd"),
  params     = list(data_path = here::here("1_data", "master_micro_survey.csv")),
  output_dir = here::here("4_output")
)
```

Point `data_path` at any CSV in `1_data/`. The report shows dataset dimensions and
duplicate/empty/single-row flags, a per-column profile (detected type, missingness,
distinct count, stray whitespace, date-parse rate, 1.5×IQR outliers), a missingness
chart, house-style per-column issue callouts, and a **deterministic template
recommender** — a ranked list of which templates (3.01–3.23) the data can feed and which
column maps to which `*_var` param. It only describes; it never changes your data. Field
data stays local (`4_output/` and `3_templates/*.html` are gitignored; nothing calls an API).

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
* **3.19 CONSORT Flow Diagram** — participant flow through trial stages (enrolled →
  randomized → allocated → followed up → analyzed), with per-arm columns and
  exclusion/attrition notes. Pure ggplot boxes + arrows (no DiagrammeR).
* **3.20 Covariate Balance (Love Plot)** — standardized mean differences between
  treatment and control across covariates, with |SMD| threshold lines.
* **3.21 Summary Table (Table 1)** — grouped summary statistics via `gt`: numeric →
  mean (SD), categorical → n (%), with an Overall column.
* **3.22 Quadrant Scatter** — labeled two-axis scatter split into four quadrants at
  each axis's median (or a fixed cut), points colored by quadrant (`ggrepel` labels).
* **3.23 Kaplan-Meier Survival Curve** — time-to-event retention by group (`survival`),
  step curves with optional 95% confidence ribbons.

`3.00_combined_report.Rmd` is the parent that stitches the visuals you list under
`combined.include` in the config into a single HTML report — you don't edit it directly.

## Changing Brand Colors

All brand colors live in one file: **`2_R/theme_colors.yml`**. Edit a hex there and
every chart across all 23 templates updates on the next render — you never touch R code.
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
