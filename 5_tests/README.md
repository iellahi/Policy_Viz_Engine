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
