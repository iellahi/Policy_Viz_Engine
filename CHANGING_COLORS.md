# Changing chart colors

All brand colors live in one file: **`2_R/theme_colors.yml`**. Edit a hex there and
every chart across all 18 templates updates on the next render. You never touch R code.

## Change a color

1. Open the project via `cerp_viz_repo.Rproj` in RStudio.
2. Open `2_R/theme_colors.yml` and edit a value, keeping the quotes — e.g.:

   ```yaml
   primary:   "#d62828"   # original: #457b9d
   ```

   Leave the `# original:` comment so you can always revert.
3. (Optional) To pick a color visually instead of typing a hex, install the picker
   once — `install.packages("colourpicker")` — then in RStudio use
   **Addins ▸ Colour Picker**, and paste the hex it gives you into the file.
4. Re-render to see it: open any template (e.g. `3_templates/3.01_baseline_endline.Rmd`)
   and click **Knit**, or run `source(here::here("2_R", "2.2_master_knit.R"))` to
   rebuild everything.

Quick check that the file is being read (R console):

```r
source(here::here("2_R", "2.0_setup_theme.R"))
cerp_cols[["primary"]]   # prints your new hex
```

## Revert to the CERP theme

- **One color:** set the value back to the `# original:` hex shown on that line, save, re-knit.
- **All changes at once:** from your terminal, `git restore 2_R/theme_colors.yml` (works once
  the file is committed — the committed version is the CERP default).

## Notes

- Editing colors recolors **future** renders only; HTML already in `4_output/` changes when you re-knit it.
- The color roles: `text`, `primary`/`secondary` (main graph colors), `box`, `bg`,
  `neutral`, `accent`, `subtle`, `sand`, `sage`, plus chart furniture `grid` and `caption_muted`.
- The HTML page styling (`2_R/cerp_style.css`) is **generated** from this file. If you change
  `bg`, `text`, `box`, `subtle`, or `primary` (the five colors used by the page chrome), run
  `source(here::here("2_R", "2.7_build_css.R"))` once to regenerate it, then re-knit. Never edit
  `cerp_style.css` by hand — it is overwritten on the next build.
- `colourpicker` is just a picking tool — install it globally, don't add it to `renv.lock`.
