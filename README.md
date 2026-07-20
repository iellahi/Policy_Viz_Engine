# CERP Analytics: Automated Visualization Suite

**Author:** Ibraheem Saqib Ellahi ([ibraheemsaqib90@gmail.com](mailto:ibraheemsaqib90@gmail.com)) · MIT License

A modular, parameterized R Markdown reporting engine. It turns raw CSV/Excel data
into policy-ready **HTML** reports without requiring the user to write any R code.
You edit one config file (`render_config.yml`); the engine renders every visual and,
optionally, stitches selected ones into a single combined report.

**▶ [Browse the live demo gallery](https://iellahi.github.io/Policy_Viz_Engine/)** —
every template rendered against the synthetic demo data; click any card to open
the full report.

## Repository Architecture

* **/1_data** — **The Drop Zone:** place raw `.csv` or `.xlsx` files here. Only the
  synthetic `master_*.csv` demo data and `geo/` are tracked in git; all other field
  data is gitignored and never leaves the machine.
* **/2_R** — **The Engine Room:** setup theme (`2.0_setup_theme.R`), brand palette
  (`theme_colors.yml`), Excel→CSV converter (`2.3_excel_to_csv.R`), shared helpers
  (`2.5_helpers.R`), **all chart logic as `viz_*()` functions** (`2.6_viz_functions.R`),
  the config-driven production render (`2.2_master_knit.R`), the testing render
  (`2.4_test_knit.R`), the CSS generator (`2.7_build_css.R`), the gallery builder
  (`2.8_build_index.R`), and the guarded gallery publisher (`2.9_publish_docs.R`).
* **/3_templates** — **The Factory:** 23 parameterized `.Rmd` templates (3.01–3.23),
  one visualization each, plus the combined-report parent (`3.00_combined_report.Rmd`)
  and the pre-flight QA report (`0.00_data_quality_report.Rmd`). Each template is a
  thin wrapper around its `viz_*()` function — you never edit these; you point them
  at data via `render_config.yml`.
* **/3_templates_testing** — the same template bodies with YAML headers pointed
  at messy *field* data (e.g. `babychecker.csv`), for manual stress-testing via
  `2.4_test_knit.R`. Those data files are gitignored, so on a fresh clone these
  renders fail by design — that's expected, not broken. The maintained,
  reproducible stress path is the suite in `/5_tests`.
* **/4_output** — **The Deliverables:** generated HTML reports and figures, plus
  `index.html` — a static gallery rebuilt at the end of every production knit.
  Open it first to browse everything rendered. Gitignored (regenerated locally).
* **/5_tests** — **The Safety Net:** golden-figure regression test (`snapshot.R`)
  and the stress-test suite. See `5_tests/README.md`.
* **/6_shiny** — **The Config Builder:** a local Shiny app (`app.R`) that helps you
  *write* `render_config.yml` — profile a CSV, get template recommendations, map
  columns, preview the chart, and copy out a config entry. It is a front end to the
  config, not a second render engine (see [Config Builder app](#config-builder-app-6_shiny)).
* **/docs** — the published copy of the demo gallery, served by GitHub Pages.
  Written only by `2_R/2.9_publish_docs.R` — never by hand (see
  [Publishing the gallery](#publishing-the-gallery)).

## Setup & Replication

Full plain-language instructions — install R/RStudio, clone, restore packages,
render — live in **[REPLICATION.md](REPLICATION.md)**, together with the known
issues and their fixes (build tools, `sf` system libraries, fonts, pandoc).
The short version:

```r
# after opening policy_viz_engine.Rproj in RStudio:
renv::restore()                                   # once per machine
source(here::here("2_R", "2.2_master_knit.R"))    # render everything
```

Working with an AI assistant (Claude etc.)? **[USING-AI.md](USING-AI.md)** covers
how to point one at this repo safely and what to ask it for.

`renv` owns the package environment. Templates **never** install packages at
render time — a missing package fails loudly and tells you to run
`renv::restore()`. Output is HTML; no LaTeX is required. **Always open
`policy_viz_engine.Rproj` first** so paths resolve from the project root.

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
3. **Generate:** `source(here::here("2_R", "2.2_master_knit.R"))`. The engine
   validates the whole config up front, renders each enabled entry (one failure
   never stops the batch), builds the combined report, then rebuilds the gallery.
4. **Browse:** open `4_output/index.html` — one thumbnail card per rendered report
   plus a featured card for the combined report. Opens straight from disk.

Column names in `*_var` params are validated against the actual data on load: a typo
produces a clear error naming the missing column and the closest match, not a
silently-wrong chart.

## Pre-flight Data Quality Report (`0.00`)

Before picking a visual, profile a freshly dropped CSV. It is **per-dataset, not
per-config** — deliberately *not* part of the master knit — so render it on its own:

```r
rmarkdown::render(
  here::here("3_templates", "0.00_data_quality_report.Rmd"),
  params     = list(data_path = here::here("1_data", "master_micro_survey.csv")),
  output_dir = here::here("4_output")
)
```

The report shows dataset dimensions and duplicate/empty/single-row flags, a
per-column profile (detected type, missingness, distinct count, stray whitespace,
date-parse rate, outliers), per-column issue callouts, and a **deterministic
template recommender** — a ranked list of which templates the data can feed and
which column maps to which `*_var` param. It only describes; it never changes your
data, and nothing calls an external API.

## Config Builder app (`6_shiny`)

A local, point-and-click front end for building `render_config.yml` entries —
useful if you would rather not hand-edit YAML. Launch it from the project root in
RStudio (after `renv::restore()`):

```r
shiny::runApp(here::here("6_shiny"))
```

Want to see it without launching anything? **[6_shiny/WALKTHROUGH.md](6_shiny/WALKTHROUGH.md)**
is an annotated screenshot tour of all six tabs, including the session-only
theme recolour.

It walks the same path as the pre-flight report, one tab at a time: **Data**
(pick a CSV from `1_data/`, see its profile and flags) → **Recommend** (the same
deterministic recommender, ranked) → **Map columns** (dropdowns for each `*_var`,
filtered by expected type, plus the label/text/option params) → **Theme**
(optional: try alternate chart colours before rendering) → **Preview** (the
chart, drawn by the very same `viz_*()` function the report uses; download it as
PNG or PDF) → **Config entry** (a ready-to-paste `reports:` block). Paste that
entry under `reports:` in `render_config.yml`, then render as usual with
`2_R/2.2_master_knit.R`.

The **Theme** tab uses native colour pickers seeded from the live palette; your
picks recolour the preview (and its PNG/PDF). This is **preview-only** — it
never edits `theme_colors.yml`, so the saved theme stays the default. To change
colours everywhere, edit `theme_colors.yml` and run `2_R/2.7_build_css.R`, as in
[Changing Brand Colors](#changing-brand-colors).

**What it is — and is not.** It is a *config editor*: it reads your data and the
templates, and it emits YAML. It is **not** a render engine and does not write to
`render_config.yml` for you — it shows the entry to copy, leaving your commented
config untouched. It runs **locally only**: field data is read in place from
`1_data/`, never uploaded, and nothing is sent to an external service. It reads the
brand palette live from `theme_colors.yml`, so it always matches the reports.
Three templates (3.15 choropleth, 3.16 event study, 3.21 summary table) can still
be mapped and emitted, but show a "render to see" note instead of a live preview —
they need geo assets, fit a model, or return a table rather than a quick chart.

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
* **3.19 CONSORT Flow Diagram** — participant flow through trial stages, with per-arm
  columns and exclusion/attrition notes. Pure ggplot boxes + arrows (no DiagrammeR).
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
The five colors that also style the HTML page chrome drive `2_R/cerp_style.css`, which
is **generated** from the palette: after changing one of those, run
`source(here::here("2_R", "2.7_build_css.R"))` once, then re-knit. Full instructions
and how to revert: [CHANGING_COLORS.md](CHANGING_COLORS.md).

## Publishing the gallery

The [live gallery](https://iellahi.github.io/Policy_Viz_Engine/) is a copy of the demo
render served by GitHub Pages from `/docs`. To refresh it after a change,
run a full master knit, then:

```r
source(here::here("2_R", "2.9_publish_docs.R"))
```

then commit and push the `docs/` folder. The script is **guarded**: it aborts
unless every enabled config entry reads synthetic `master_*.csv` data, copies only
the files the gallery references (stale or testing renders in `4_output/` are
never picked up), and verifies figure hashes against the golden manifest where
covered. Field data can never reach the public site through it.

## Regression tests (`5_tests`)

`5_tests/snapshot.R` proves a refactor changed no pixels: it re-renders the suite
and compares a SHA-256 hash of every figure against the golden manifest
(`5_tests/snapshots/manifest.csv`, tracked). Hashes are machine-specific (font
rasterization), so on a new computer delete the manifest and record a fresh
baseline first. Details: `5_tests/README.md`.

## Troubleshooting

* **Setup or package problems** (restore fails, maps fail, fonts, pandoc): see the
  fixes in [REPLICATION.md](REPLICATION.md).
* **A template fails during render.** Read the error — it names the exact column or
  parameter at fault. Check that your `*_var` names in `render_config.yml` match
  your data headers (case-sensitive) and that key columns aren't all `NA`.
* **Testing vs. production outputs.** `2.4_test_knit.R` writes to the same `/4_output`
  folder and can overwrite production HTML and figures of the same name (and vice
  versa). Re-run the production knit before trusting `4_output/` — and always
  before publishing the gallery.
