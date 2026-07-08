# ==============================================================================
# Script Name: 5_tests/stress_data_generator.R
# Purpose:     Manufacture corrupted variants of every master CSV so the stress
#              harness (5_tests/stress_harness.R) can prove the engine either
#              absorbs bad data cleanly OR fails LOUDLY — never renders a
#              silently-wrong chart (hard rule 4).
#
# Output:      5_tests/stress_data/ (GITIGNORED — regenerable, and although it is
#              derived only from the synthetic master_*.csv it is treated like any
#              other 1_data drop and kept out of git). Also writes a machine-
#              readable index, 5_tests/stress_data/_manifest.csv, that the harness
#              reads to know each variant's file, target templates, and the
#              behavior we EXPECT (ok = absorb / error = reject loudly /
#              eyeball = ambiguous, route to human review).
#
# Determinism: a fixed seed makes the corruption reproducible run to run, so a
#              result that changes points at the code, not the dice.
#
# Run:         source(here::here("5_tests", "stress_data_generator.R"))
# renv owns packages — nothing is installed here.
# ==============================================================================

library(here)
source(here::here("2_R", "2.5_helpers.R"))   # cerp_require + house helpers
cerp_require(c("readr", "dplyr", "stringr", "tidyr", "cli"))
source(here::here("5_tests", "stress_spec.R"))   # stress_files (column roles)

set.seed(4)   # phase 4 — deterministic corruption

data_dir <- here::here("1_data")
out_dir  <- here::here("5_tests", "stress_data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Wipe any prior run so renamed/removed variants can't linger and mislead the harness.
old <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(old) > 0) file.remove(old)

# ------------------------------------------------------------------------------
# Small helpers ----------------------------------------------------------------
# pick_idx(): safe random index sample that never errors on tiny/empty inputs.
# add_variant(): write one corrupted CSV as "<stem>__<variant>.csv" and record a
#   manifest row. `targets` is "*" (whole common battery — every template on this
#   file) or a semicolon list of template ids the case is aimed at. `expect` is
#   the behavior the engine SHOULD show on this input.
# ------------------------------------------------------------------------------
pick_idx <- function(n_total, k) {
  if (n_total <= 0) return(integer(0))
  sample.int(n_total, min(k, n_total))
}

manifest <- list()
add_variant <- function(data_file, variant, df, expect,
                        targets = "*", note = "") {
  stem <- sub("\\.csv$", "", data_file)
  fname <- paste0(stem, "__", variant, ".csv")
  readr::write_csv(df, file.path(out_dir, fname), na = "")
  manifest[[length(manifest) + 1]] <<- data.frame(
    data = data_file, variant = variant, file = fname,
    expect = expect, targets = targets, note = note,
    stringsAsFactors = FALSE
  )
  invisible(fname)
}

# ------------------------------------------------------------------------------
# Corruption primitives --------------------------------------------------------
# Each takes a clean data.frame (+ the file's column roles) and returns a broken
# one. Deliberately blunt: the point is to hit the engine, not to be subtle.
# ------------------------------------------------------------------------------

# NAs sprinkled through the analytic ("key") columns.
inject_na <- function(df, cols, frac = 0.15) {
  for (c in intersect(cols, names(df))) {
    df[[c]][pick_idx(nrow(df), floor(nrow(df) * frac))] <- NA
  }
  df
}

# Leading/trailing whitespace in BOTH headers and character values — the exact
# landmine cerp_load()'s str_squish is meant to defuse.
add_whitespace <- function(df) {
  names(df) <- paste0("  ", names(df), " ")
  dplyr::mutate(df, dplyr::across(
    where(is.character), ~ paste0("  ", .x, " ")))
}

# Numbers arriving as text: "N/A", thousands-comma "1,200", trailing-"%".
# cerp_numeric()/parse_number should recover the value and WARN on the garbage.
messify_numbers <- function(df, cols) {
  for (c in intersect(cols, names(df))) {
    x   <- as.character(df[[c]])
    num <- suppressWarnings(as.numeric(x))
    n   <- length(x)
    na_idx  <- pick_idx(n, ceiling(n * 0.08))                 # 8% -> literal "N/A"
    x[na_idx] <- "N/A"
    big_idx <- which(!is.na(num) & abs(num) >= 1000)      # big numbers -> commas
    if (length(big_idx) > 0)
      x[big_idx] <- formatC(num[big_idx], format = "d", big.mark = ",")
    pct_pool <- setdiff(which(!is.na(num)), c(na_idx, big_idx))
    pct_idx  <- pct_pool[pick_idx(length(pct_pool), 5)]       # a few -> "45%"
    if (length(pct_idx) > 0) x[pct_idx] <- paste0(round(num[pct_idx]), "%")
    df[[c]] <- x
  }
  df
}

# Non-ASCII in the first text column: accents, RTL script, an emoji.
add_unicode <- function(df, text_cols) {
  if (length(text_cols) == 0) return(df)
  c <- text_cols[1]
  idx <- pick_idx(nrow(df), 6)
  df[[c]][idx] <- paste0(df[[c]][idx], " — لاہور ✓")
  df
}

# Push a few cells to absurd magnitudes to test axis/scale robustness.
add_outliers <- function(df, num_cols) {
  for (c in intersect(num_cols, names(df))) {
    idx <- pick_idx(nrow(df), 3)
    if (length(idx) > 0) df[[c]][idx] <- df[[c]][idx] * 1e6 + 1e9
  }
  df
}

# ------------------------------------------------------------------------------
# 1. COMMON BATTERY (applied to every master file) -----------------------------
# Aimed at all templates that read the file (targets = "*").
# ------------------------------------------------------------------------------
build_common_battery <- function(data_file, df, roles) {
  add_variant(data_file, "nas_key", inject_na(df, roles$key), expect = "ok",
              note = "~15% NA in analytic columns; drop_na should cope")
  add_variant(data_file, "header_ws", add_whitespace(df), expect = "ok",
              note = "whitespace in headers + values; cerp_load must squish")
  add_variant(data_file, "nums_as_text",
              messify_numbers(df, roles$numeric), expect = "eyeball",
              note = "N/A, 1,200, 45% in numeric cols; confirm coerced not dropped")
  add_variant(data_file, "dup_rows", dplyr::bind_rows(df, df), expect = "eyeball",
              note = "every row duplicated; some templates should reject (aggregate none)")
  add_variant(data_file, "empty", df[0, , drop = FALSE], expect = "error",
              note = "header only, zero rows; must reject, not draw an empty chart")
  add_variant(data_file, "single_row", df[1, , drop = FALSE], expect = "eyeball",
              note = "one data row; degenerate — confirm no silently-wrong chart")
  add_variant(data_file, "unicode", add_unicode(df, roles$text), expect = "ok",
              note = "non-ASCII in labels; should render as-is")
  add_variant(data_file, "outliers", add_outliers(df, roles$numeric),
              expect = "eyeball", note = "extreme magnitudes; confirm axis not broken")
}

# Load every master file once, run the common battery, stash for the specials.
loaded <- list()
for (data_file in names(stress_files)) {
  path <- file.path(data_dir, data_file)
  if (!file.exists(path)) {
    cli::cli_alert_warning("Master file missing, skipping: {data_file}")
    next
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  loaded[[data_file]] <- df
  build_common_battery(data_file, df, stress_files[[data_file]])
}

# ------------------------------------------------------------------------------
# 2. PER-TEMPLATE CASES --------------------------------------------------------
# Targeted corruptions from PLAN.md's phase-4 spec, each aimed at the template(s)
# whose logic it probes. Guarded so a missing master file just skips its cases.
# ------------------------------------------------------------------------------
has <- function(f) !is.null(loaded[[f]])

# --- master_micro_survey.csv --------------------------------------------------
if (has("master_micro_survey.csv")) {
  ms <- loaded[["master_micro_survey.csv"]]

  # 3.05 waffle / 3.08 icon array — 3+ categories, and an absent category.
  ms_many_tech <- ms %>% mutate(tech_adopted = {
    v <- tech_adopted
    v[pick_idx(n(), 40)] <- rep(paste0("Device_", LETTERS[1:9]), length.out = 40)
    v
  })
  add_variant("master_micro_survey.csv", "manycat_tech_adopted", ms_many_tech,
              expect = "error", targets = "3.05",
              note = ">8 categories; waffle must stop with a clear message")

  ms_absent_tech <- ms %>% mutate(tech_adopted = "Yes")   # only one category left
  add_variant("master_micro_survey.csv", "absentcat_tech_adopted", ms_absent_tech,
              expect = "ok", targets = "3.05",
              note = "single surviving category; waffle should show 100%")

  ms_many_attr <- ms %>% mutate(attrition_status = {
    v <- attrition_status
    v[pick_idx(n(), 60)] <- rep(paste0("Reason_", 1:7), length.out = 60)
    v
  })
  add_variant("master_micro_survey.csv", "manycat_attrition_status", ms_many_attr,
              expect = "ok", targets = "3.08",
              note = "many raw statuses; icon array should bucket unknowns to Other")

  ms_absent_attr <- ms %>% filter(attrition_status == "Surveyed")
  add_variant("master_micro_survey.csv", "absentcat_attrition_status", ms_absent_attr,
              expect = "ok", targets = "3.08",
              note = "only one attrition category present; unused levels should be fine")

  # 3.02 distribution / 3.03 forest / 3.04 coefficient — a group with n < 3.
  ms_tiny <- ms %>%
    mutate(rn = row_number(),
           treatment_group = if_else(treatment_group == "Control" & rn > 2,
                                     "Treatment", treatment_group)) %>%
    select(-rn)
  add_variant("master_micro_survey.csv", "groups_tiny", ms_tiny,
              expect = "eyeball", targets = "3.02;3.03;3.04",
              note = "Control group reduced to 2 rows; density/SD near-degenerate")

  # 3.13 ridgeline / 3.04 coefficient — 20+ groups.
  ms_manygrp <- ms %>%
    mutate(income_tier = paste0("Tier_", sample(1:24, n(), replace = TRUE)))
  add_variant("master_micro_survey.csv", "groups_many", ms_manygrp,
              expect = "eyeball", targets = "3.13;3.04",
              note = "24 group levels; ridgeline should lump, coefficient may thin out")

  # 3.03 forest / 3.04 coefficient — zero-variance outcome within a group.
  ms_zerovar <- ms %>%
    mutate(endline_score = if_else(treatment_group == "Treatment", 50, endline_score))
  add_variant("master_micro_survey.csv", "zero_variance", ms_zerovar,
              expect = "eyeball", targets = "3.03;3.04",
              note = "Treatment outcome constant; SE collapses to zero")

  # 3.07 diverging — 3/5/7-point Likert scales in place of the native 4-point.
  likert3 <- c("Disagree", "Neutral", "Agree")
  likert5 <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
  likert7 <- c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral",
               "Somewhat Agree", "Agree", "Strongly Agree")
  add_variant("master_micro_survey.csv", "likert3",
              ms %>% mutate(parent_trust = sample(likert3, n(), replace = TRUE)),
              expect = "eyeball", targets = "3.07",
              note = "3-point scale; Neutral is dropped — confirm that is acceptable")
  add_variant("master_micro_survey.csv", "likert5",
              ms %>% mutate(parent_trust = sample(likert5, n(), replace = TRUE)),
              expect = "eyeball", targets = "3.07",
              note = "5-point scale; Neutral dropped by the 4-point mapping")
  add_variant("master_micro_survey.csv", "likert7",
              ms %>% mutate(parent_trust = sample(likert7, n(), replace = TRUE)),
              expect = "eyeball", targets = "3.07",
              note = "7-point scale; Somewhat/Neutral handling under scrutiny")
}

# --- master_macro_panel.csv ---------------------------------------------------
if (has("master_macro_panel.csv")) {
  mp <- loaded[["master_macro_panel.csv"]]

  # 3.06 slopegraph / 3.10 bump — missing years for some entities.
  mp_missing <- mp %>%
    filter(!(district == "Lahore" & year == 2025),
           !(district == "Karachi" & year == 2021))
  add_variant("master_macro_panel.csv", "missing_years", mp_missing,
              expect = "eyeball", targets = "3.06;3.10",
              note = "ragged panel: some entities lack an anchor year")

  # 3.11 bullet / 3.12 deviation — multiple rows per (entity, filter year).
  mp_dupe2025 <- bind_rows(
    mp,
    mp %>% filter(year == 2025) %>% mutate(literacy_rate = literacy_rate + 5)
  )
  add_variant("master_macro_panel.csv", "multi_per_entity_time", mp_dupe2025,
              expect = "eyeball", targets = "3.11;3.12",
              note = "two 2025 rows per district; bullet expects one, deviation means them")

  # 3.17 small multiples — a single facet.
  add_variant("master_macro_panel.csv", "single_facet",
              mp %>% filter(district == "Lahore"),
              expect = "eyeball", targets = "3.17",
              note = "one district; facet grid collapses to a single panel")

  # 3.18 heatmap matrix — all cell values NA.
  add_variant("master_macro_panel.csv", "all_na_cells",
              mp %>% mutate(literacy_rate = NA_real_),
              expect = "error", targets = "3.18",
              note = "every cell empty; must reject, not draw a blank grid")
}

# --- master_budget_pipeline.csv ----------------------------------------------
if (has("master_budget_pipeline.csv")) {
  bp <- loaded[["master_budget_pipeline.csv"]]

  # 3.09 waterfall — a supplied total that does not equal the running sum.
  bp_nosum <- bind_rows(bp, tibble(budget_stage = "Total Deployed",
                                   budget_amount = 999))
  add_variant("master_budget_pipeline.csv", "stages_nosum", bp_nosum,
              expect = "eyeball", targets = "3.09",
              note = "explicit total row that doesn't reconcile with the stages")

  # 3.09 waterfall — mixed-sign stages (large gains and losses interleaved).
  bp_mixed <- bp %>%
    mutate(budget_amount = budget_amount *
             rep_len(c(1, -1, 1, -1), length.out = n()) * 2)
  add_variant("master_budget_pipeline.csv", "mixed_signs", bp_mixed,
              expect = "eyeball", targets = "3.09",
              note = "alternating +/- magnitudes; connectors/labels under stress")
}

# --- master_daily_interaction.csv --------------------------------------------
if (has("master_daily_interaction.csv")) {
  di <- loaded[["master_daily_interaction.csv"]]

  # 3.14 calendar — ISO timestamps instead of plain dates.
  add_variant("master_daily_interaction.csv", "dates_timestamps",
              di %>% mutate(date = paste0(date, "T04:36:41Z")),
              expect = "ok", targets = "3.14",
              note = "timestamped dates; cerp_parse_date should still land one day each")

  # 3.14 calendar — a multi-week gap in the series.
  add_variant("master_daily_interaction.csv", "dates_gaps",
              di %>% filter(!(row_number() %in% 30:60)),
              expect = "ok", targets = "3.14",
              note = "missing stretch; complete() should backfill as pale days")

  # 3.14 calendar — data spanning two calendar years.
  di_multi <- di %>%
    mutate(date = as.Date(date) + if_else(row_number() > n() / 2, 365, 0))
  add_variant("master_daily_interaction.csv", "dates_multiyear", di_multi,
              expect = "ok", targets = "3.14",
              note = "two years; layout should switch to one month-row per year")

  # 3.14 calendar — the value column is absent entirely.
  add_variant("master_daily_interaction.csv", "no_value_col",
              di %>% select(-adoption_rate),
              expect = "error", targets = "3.14",
              note = "value_var column dropped; cerp_validate must name it")
}

# --- master_district_indicators.csv ------------------------------------------
if (has("master_district_indicators.csv")) {
  ind <- loaded[["master_district_indicators.csv"]]

  # 3.15 choropleth — names that cannot be matched to the geometry.
  add_variant("master_district_indicators.csv", "unmatchable_districts",
              ind %>% mutate(district = paste0("Nowhere_", row_number())),
              expect = "error", targets = "3.15",
              note = "unresolvable names; cerp_harmonize must fail loud with suggestions")

  # 3.15 choropleth — a duplicated district with a different value.
  add_variant("master_district_indicators.csv", "duplicate_districts",
              bind_rows(ind, ind[1, ] %>% mutate(literacy_rate = literacy_rate + 20)),
              expect = "eyeball", targets = "3.15",
              note = "same district twice; values are silently averaged — confirm intended")
}

# --- master_event_panel.csv ---------------------------------------------------
if (has("master_event_panel.csv")) {
  ep <- loaded[["master_event_panel.csv"]]

  # 3.16 event study — an unbalanced panel (drop scattered unit-periods).
  add_variant("master_event_panel.csv", "unbalanced",
              ep %>% filter(!(school_id %% 5 == 0 & event_time %in% c(1, 2))),
              expect = "ok", targets = "3.16",
              note = "unbalanced panel; feols should still estimate")

  # 3.16 event study — the reference event time is missing from the data.
  add_variant("master_event_panel.csv", "missing_event_times",
              ep %>% filter(event_time != -1),
              expect = "error", targets = "3.16",
              note = "ref_period (-1) absent; must stop, listing available event times")

  # 3.16 event study — nobody is treated.
  add_variant("master_event_panel.csv", "never_treated",
              ep %>% mutate(treated = 0),
              expect = "error", targets = "3.16",
              note = "no treated units; DiD is unidentified — must not draw a flat line")
}

# ------------------------------------------------------------------------------
# 3. Write the manifest --------------------------------------------------------
# ------------------------------------------------------------------------------
manifest_df <- do.call(rbind, manifest)
readr::write_csv(manifest_df, file.path(out_dir, "_manifest.csv"))

cli::cli_rule()
cli::cli_alert_success(
  "Generated {nrow(manifest_df)} stress variant{?s} across {length(loaded)} master file{?s} -> 5_tests/stress_data/")
cli::cli_alert_info("Manifest: 5_tests/stress_data/_manifest.csv. Next: source 5_tests/stress_harness.R")
