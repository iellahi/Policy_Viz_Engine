# How to Reproduce the Reports on Your Computer

No R knowledge needed. Follow the steps in order; each takes a few minutes. If
anything goes wrong, jump to [Possible issues & fixes](#possible-issues--fixes).

## Steps

1. **Install R** (version 4.4.x) from [cran.r-project.org](https://cran.r-project.org/)
   — the project was built on R 4.4.1, so pick a 4.4 release.
2. **Install RStudio Desktop** (free) from
   [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/).
3. **Download this repository.** Either click the green **Code** button on GitHub →
   **Download ZIP** and unzip it, or, if you have git:
   ```
   git clone https://github.com/iellahi/Cerp_Viz_Repo.git
   ```
4. **Open the project file** — double-click **`cerp_viz_repo.Rproj`** inside the
   folder. This opens RStudio *and* sets everything up (paths, package system).
   Always start from this file, never by opening RStudio first.
   The first launch prints some setup messages — that's normal.
5. **Install the packages.** In the RStudio *Console* (the panel where you can
   type), type this and press Enter:
   ```r
   renv::restore()
   ```
   Answer `y` if asked to proceed. This downloads the exact package versions the
   project was built with. It can take 10–30 minutes the first time. You only do
   this once per computer.
6. **Render everything.** In the same Console:
   ```r
   source(here::here("2_R", "2.2_master_knit.R"))
   ```
   This renders every configured report. A summary prints at the end telling you
   what succeeded.
7. **Look at the results.** Open the folder **`4_output`** inside the project and
   double-click **`index.html`** — a gallery page with one card per report. Click
   any card to open the full report. That's it.

To point the reports at *your own* data instead of the demo data: drop your
`.csv` into `1_data/`, edit `render_config.yml` (the only file you ever edit),
and repeat step 6. The README's *Quick Start* explains the config file.

## Possible issues & fixes

**Step 5 fails or asks about compiling packages.**
Package installation sometimes needs build tools.

- *Windows:* install **Rtools 4.4** from
  [cran.r-project.org/bin/windows/Rtools](https://cran.r-project.org/bin/windows/Rtools/),
  then re-run `renv::restore()`.
- *Mac:* run `xcode-select --install` in the Terminal app, then re-run
  `renv::restore()`.
- It's safe to re-run `renv::restore()` as many times as needed — it picks up
  where it left off.

**"This project requires R 4.4.x" or restore fails immediately.**
Your R version is too different. Check yours by typing `R.version.string` in the
Console; install an R 4.4 release (step 1) and reopen the `.Rproj` file.

**Only the two map reports (choropleth) fail; everything else works.**
Maps need three system libraries (GDAL, GEOS, PROJ) that live outside R.

- *Mac (with [Homebrew](https://brew.sh)):* `brew install gdal geos proj`
- *Ubuntu/Debian:* `sudo apt install libgdal-dev libgeos-dev libproj-dev`
- *Windows:* usually works out of the box (the libraries come bundled).
- Or skip the maps: in `render_config.yml`, set `enabled: false` on the two
  `choropleth_*` entries. Every other report is unaffected.

**"Missing package … run renv::restore()".**
Exactly what it says: type `renv::restore()` in the Console, then re-run step 6.
The project never installs packages by itself — this message is the built-in
signal that the environment is out of sync.

**Error mentions "pandoc".**
You're rendering outside RStudio (e.g. plain R or a terminal). RStudio bundles
pandoc; the simplest fix is to run step 6 inside RStudio.

**Charts render but fonts look different from the originals.**
The house fonts aren't installed. Install **Charter** and
**Libre Franklin** (free on [Google Fonts](https://fonts.google.com/specimen/Libre+Franklin))
system-wide and re-render. Without them the reports still work — they just fall
back to similar fonts. Note that even with the right fonts, figures are never
*pixel*-identical across computers (font rendering differs by machine), which is
also why the regression-test manifest in `5_tests/` must be re-baselined on a new
computer — see `5_tests/README.md`. This matters only if you run the tests.

**Errors about paths, or "could not find file …".**
Almost always caused by skipping step 4. Close RStudio and reopen the project by
double-clicking **`cerp_viz_repo.Rproj`** — the project file is what makes all
paths resolve correctly.

**A report fails with a message naming a column.**
Not a setup problem — the config points at a column that isn't in the data. The
error names the missing column and the closest match it found; fix the `*_var`
line in `render_config.yml` (column names are case-sensitive) and re-run step 6.

Still stuck? Open an issue on the GitHub repository with the full error message.
