# 5_tests — regression + stress harness

## `snapshot.R` — golden-figure regression test (phase 8)

Proves that extracting plot code out of the `.Rmd` chunks into `viz_*()` functions
changes **no pixels**. It renders the production suite on master data via the shared
core (`2_R/2.2_master_knit.R`), then SHA-256-hashes every PNG in `4_output/figures/`.

### Workflow

1. **Record the baseline — before touching any template:**
   ```r
   source(here::here("5_tests", "snapshot.R"))
   ```
   With no manifest present, this renders and writes the golden hashes to
   `5_tests/snapshots/manifest.csv` (columns: `template_id, figure_file, sha256,
   rendered_at`). Commit that file.

2. **Extract functions in small batches** (~4–5 templates), then re-run the same
   command. With the manifest present, it re-renders, re-hashes, and reports:
   ```
   0 mismatches across 18 figures. Refactor is a no-op.
   ```
   Any `changed hash` / `not reproduced` / `unexpected new` line means the refactor
   altered output — stop and fix before continuing. The latest run is written to
   `5_tests/snapshots/manifest_latest.csv` for inspection.

3. Repeat step 2 until all templates are extracted and the compare is clean.

### Notes

- **Same machine only.** Font rasterization differs across systems, so hashes are
  only comparable when recorded and compared on the same machine/environment.
- The harness **clears `4_output/figures/*.png` before each render** so orphaned
  PNGs from renamed/removed chunks can't pollute the manifest.
- To deliberately re-baseline (e.g. after an intended visual change), delete
  `5_tests/snapshots/manifest.csv` and re-source.
- `renv` owns packages — the harness installs nothing (`digest` + `cli`, both
  already in `renv.lock`).

## Stress suite (phase 4) — break the engine on purpose

Where `snapshot.R` proves *nothing changed*, the stress suite proves the engine
either **absorbs bad data cleanly or fails loudly** — it must never render a
silently-wrong chart (hard rule 4). It runs against the `viz_*()` functions
directly (seconds), with a thin render-level pass on top.

### Files

- `stress_spec.R` — the single source of truth: each template's `viz_*()`
  function, its master CSV, and the exact non-text params it passes, plus each
  master file's column roles. **Keep it in sync** when a template's YAML changes.
- `stress_data_generator.R` — writes corrupted variants of every master CSV into
  `5_tests/stress_data/` (gitignored) and an index `stress_data/_manifest.csv`.
- `stress_harness.R` — calls each `viz_*()` on its variants (mirroring the Rmd:
  `cerp_load` → `cerp_validate` → `viz_*()` → **force the plot to build**),
  classifies every outcome, and writes `5_tests/stress_results.csv` (gitignored).
- `stress_render.R` — knits each template once against its single nastiest
  variant to catch Rmd-layer breakage (YAML/pandoc/CSS). HTML → `stress_output/`.

### Workflow

```r
source(here::here("5_tests", "stress_data_generator.R"))  # 1. build variants
source(here::here("5_tests", "stress_harness.R"))         # 2. run + classify
source(here::here("5_tests", "stress_render.R"))          # 3. (optional) knit pass
```

### Reading the results

Each row in `stress_results.csv` gets one `status`:

| status | meaning | action |
|--------|---------|--------|
| `pass` | succeeded on input we expect the engine to absorb | none |
| `clean-error` | failed loudly, message names the column/problem | none — this is the goal |
| `bad-error` | failed with an opaque, low-level message | **fix**: add a clear guard |
| `silent-success` | succeeded on input that should be rejected or is ambiguous | **eyeball** the output |

`review = TRUE` flags every row needing a human eye — all `bad-error` and
`silent-success` rows, plus any case that errored where we expected it to cope.
The `expect` column (`ok` / `error` / `eyeball`) records what the generator
intended, so a `silent-success` on an `error`-expected variant is the loud alarm,
while one on an `eyeball` variant is just "confirm this looks right".

### Fixing what it surfaces

Prefer strengthening `2_R/2.5_helpers.R` over per-template patches. **Any guard
added to a `viz_*()` function must be a no-op on the master data** — re-run
`snapshot.R` in compare mode after fixing and confirm **0 mismatches** so the
golden figures are untouched.

### Notes

- **Deterministic** — `stress_data_generator.R` sets a fixed seed, so a changed
  result points at the code, not the dice.
- **No field data, no git** — variants derive only from the synthetic
  `master_*.csv`; `stress_data/`, `stress_output/`, and `stress_results.csv` are
  all gitignored. The `stress_*.R` scripts are tracked.
- `stress_render.R` overwrites `4_output/figures` with throwaway figures (hard-
  coded `fig.path`); it's gitignored and regenerated on the next production knit.
- `renv` owns packages — the suite installs nothing (`ggrepel`, `ggridges`, `sf`,
  `fixest`, `cli` are already in `renv.lock`).
