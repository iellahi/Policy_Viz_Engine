library(tidyverse)
library(lubridate)
library(here)
set.seed(2026)

# ==============================================================================
# DATASET 1: Macro Panel Data (School/District Level)
# Use for: Slopegraph, Bump Chart, Bullet Chart, Deviation Bar
# ==============================================================================
districts <- c("Lahore", "Karachi", "Islamabad", "Faisalabad", "Multan")
years <- 2021:2025

macro_panel <- expand_grid(district = districts, year = years) %>%
  mutate(
    literacy_rate = round(runif(n(), 50, 85) + (year - 2021) * 2),
    infrastructure_score = round(rnorm(n(), mean = 65, sd = 10)),
    target_score = 80
  )

write_csv(macro_panel, here::here("1_data", "master_macro_panel.csv"))

# ==============================================================================
# DATASET 2: Budget Pipeline Data 
# Use for: Waterfall Chart 
# ==============================================================================
budget_pipeline <- tibble(
  budget_stage = c("Allocated", "Admin Loss", "Transport", "Deployed"),
  budget_amount = c(5000000, -450000, -200000, 4350000)
)

write_csv(budget_pipeline, here::here("1_data", "master_budget_pipeline.csv"))

# ==============================================================================
# DATASET 3: Micro Survey Data (Student/Household Level)
# Use for: Dumbbell, Distribution Shift, Forest Plot, Diverging Bar, 
#          Waffle Chart, Icon Array, Ridgeline, AND Coefficient Plots (HTE)
# ==============================================================================
n_students <- 5000

micro_survey <- tibble(
  student_id = 1:n_students,
  district = sample(districts, n_students, replace = TRUE),
  
  # New Demographic Subgroups for HTE (Heterogeneous Treatment Effects)
  gender = sample(c("Female", "Male"), n_students, replace = TRUE),
  income_tier = sample(c("Low", "Middle", "High"), n_students, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  
  treatment_group = sample(c("Treatment", "Control"), n_students, replace = TRUE, prob = c(0.5, 0.5)),
  baseline_score = round(rnorm(n_students, mean = 45, sd = 12)),
  
  parent_trust = sample(
    c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
    n_students, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)
  ),
  
  tech_adopted = sample(c("Yes", "No"), n_students, replace = TRUE, prob = c(0.65, 0.35)),
  attrition_status = sample(
    c("Surveyed", "Refused", "Not Reached"), 
    n_students, replace = TRUE, prob = c(0.75, 0.10, 0.15)
  )
) %>%
  mutate(
    # Simulate the treatment effect: Higher impact for low-income tier to make the Coefficient plot interesting
    impact_multiplier = if_else(income_tier == "Low", 1.5, 1.0),
    endline_score = baseline_score + if_else(
      treatment_group == "Treatment", 
      rnorm(n(), 12 * impact_multiplier, 5), 
      rnorm(n(), 3, 5)
    ),
    baseline_score = pmax(0, pmin(100, baseline_score)),
    endline_score = pmax(0, pmin(100, endline_score))
  ) %>%
  select(-impact_multiplier) # Hide the multiplier so it just looks like natural data

write_csv(micro_survey, here::here("1_data", "master_micro_survey.csv"))

# ==============================================================================
# DATASET 4: Daily Interaction Data (High-Frequency Adoption)
# Use for: Calendar Heatmaps, Adoption Curves
# ==============================================================================
# Simulating 6 months of a daily tablet rollout
start_date <- ymd("2025-01-01")
end_date <- ymd("2025-06-30")
date_sequence <- seq(start_date, end_date, by = "day")

daily_interaction <- tibble(
  date = date_sequence,
  day_of_week = wday(date, label = TRUE)
) %>%
  mutate(
    # Simulate gradual adoption growing over time
    base_users = seq(500, 4500, length.out = n()),
    # Add weekend dips
    weekend_penalty = if_else(day_of_week %in% c("Sat", "Sun"), 0.3, 1.0),
    # Add some random daily noise
    active_users = round(base_users * weekend_penalty * runif(n(), 0.9, 1.1)),
    total_enrolled = 5000,
    adoption_rate = round((active_users / total_enrolled) * 100, 1)
  ) %>%
  select(date, active_users, total_enrolled, adoption_rate)

write_csv(daily_interaction, here::here("1_data", "master_daily_interaction.csv"))

# ==============================================================================
# DATASET 5: District Indicators — DEMO (Choropleth 3.15)
# Names are deliberately messy to exercise district-name harmonization against
# 1_data/geo/pk_districts_demo.geojson via the versioned crosswalk:
#   "Lahore District"  -> suffix stripped by cerp_norm()
#   "Karachi "         -> trailing space stripped by cerp_norm()
#   "Islamabad Capital Territory" -> suffix stripped by cerp_norm()
#   "Lyallpur"         -> historic alias, resolved via district_lookup.csv
# ==============================================================================
district_indicators <- tibble(
  district = c("Lahore District", "Karachi ", "Islamabad Capital Territory",
               "Lyallpur", "Multan"),
  literacy_rate = c(74, 66, 81, 69, 63)
)
write_csv(district_indicators, here::here("1_data", "master_district_indicators.csv"))

# ==============================================================================
# DATASET 6: District Indicators — PAKISTAN ADM2 (Choropleth 3.15, full map)
# If a REAL ADM2 boundary file is present (>40 districts), derive the demo
# indicators straight from its district names so the choropleth join is
# guaranteed clean (subset shaded, the rest render grey). Otherwise fall back to
# the stylized-demo names that ship with the repo. This means re-running the
# generator won't clobber a real-boundary swap with mismatched names.
# NOTE: the shipped pk_districts_adm2.geojson is stylized demo geometry (real
# district names at approximate centroids). To use real boundaries, drop a
# geoBoundaries/GADM ADM2 file (name field -> `district`) at the same path.
# ==============================================================================
adm2_path <- here::here("1_data", "geo", "pk_districts_adm2.geojson")
real_names <- NULL
if (requireNamespace("sf", quietly = TRUE) && file.exists(adm2_path)) {
  real_names <- sort(unique(as.character(sf::st_read(adm2_path, quiet = TRUE)$district)))
}
# --- RNG isolation (5B) -------------------------------------------------------
# This dataset's draw count depends on which branch runs (real file: sample +
# runif(40); fallback: runif(29)), and every dataset BELOW reads the same
# global RNG stream seeded once at the top. Without isolation, swapping the
# boundary file would silently regenerate every downstream master_*.csv and
# break their golden snapshots. So: snapshot the stream, use a dataset-local
# seed, then restore the stream and replay the exact draws the pre-5B code
# consumed here (runif(29), the fallback branch that shipped) — datasets 7+
# stay byte-identical whichever branch runs.
.ds6_seed_state <- .Random.seed
set.seed(516)
if (!is.null(real_names) && length(real_names) > 40) {
  picked <- sort(sample(real_names, 40))
  adm2_indicators <- tibble(district = picked,
                            literacy_rate = round(runif(length(picked), 45, 85)))
} else {
  adm2_indicators <- tibble(
    district = c("Lahore", "Kasur", "Sheikhupura", "Faisalabad", "Jhang",
                 "Toba Tek Singh", "Multan", "Khanewal", "Vehari", "RWP",
                 "Islamabad", "Attock", "Gujranwala", "Sialkot ", "Gujrat",
                 "Sargodha", "Bahawalpur", "Rahimyar Khan", "D.G. Khan", "Karachi",
                 "Hyderabad ", "Sukkur", "Larkana", "Mirpur Khas", "Peshawar",
                 "Mardan", "Abbottabad", "D.I. Khan", "Quetta"),
    literacy_rate = round(runif(29, 45, 85))
  )
}
write_csv(adm2_indicators, here::here("1_data", "master_district_indicators_pk.csv"))
.Random.seed <- .ds6_seed_state   # restore the pre-dataset-6 stream…
invisible(runif(29))              # …and replay the historical fallback draws so
rm(.ds6_seed_state)               # datasets 7+ see exactly the stream they always did.
# --- end RNG isolation (5B) ---------------------------------------------------

# ==============================================================================
# DATASET 7: Event-Study Panel (3.16)
# 40 units over 2018–2025; units 1–20 treated at 2022. Flat (~0) pre-trend,
# effect ramps after onset, so the event-study plot tells a clean story.
# ==============================================================================
unit_fe <- rnorm(40, mean = 60, sd = 6)
event_panel <- expand_grid(school_id = 1:40, period = 2018:2025) %>%
  mutate(
    treated    = if_else(school_id <= 20, 1L, 0L),
    event_time = period - 2022,
    period_fe  = (period - 2018) * 0.8,
    eff = case_when(
      treated == 1 & event_time <  0 ~ rnorm(n(), 0, 0.4),   # flat pre-trend
      treated == 1 & event_time >= 0 ~ 2.5 * (event_time + 1) + rnorm(n(), 0, 0.5),
      TRUE                           ~ rnorm(n(), 0, 0.4)
    ),
    test_score = round(unit_fe[school_id] + period_fe + eff + rnorm(n(), 0, 1.2), 2)
  ) %>%
  select(school_id, period, event_time, treated, test_score)
write_csv(event_panel, here::here("1_data", "master_event_panel.csv"))

# ==============================================================================
# DATASET 8: Sample Flow (CONSORT diagram, 3.19)
# One row per trial stage. Pre-randomization stages have a blank `arm` (they sit
# on the central spine); post-randomization stages carry an arm so the flow
# splits into Treatment/Control columns. `note` holds exclusion/attrition text.
# ==============================================================================
sample_flow <- tibble(
  stage = c("Assessed for eligibility",
            "Randomized",
            "Allocated", "Allocated",
            "Completed follow-up", "Completed follow-up",
            "Analyzed", "Analyzed"),
  n     = c(1200, 900, 450, 450, 410, 405, 402, 398),
  arm   = c("", "", "Treatment", "Control", "Treatment", "Control",
            "Treatment", "Control"),
  note  = c("Excluded (n = 300): did not meet inclusion criteria (210); declined to participate (90)",
            "", "", "",
            "Lost to follow-up (n = 40)", "Lost to follow-up (n = 45)",
            "Excluded from analysis (n = 8)", "Excluded from analysis (n = 7)")
)
write_csv(sample_flow, here::here("1_data", "master_sample_flow.csv"))

# ==============================================================================
# DATASET 9: District Scatter (Quadrant Scatter, 3.22)
# District-level spending index vs outcome score. Two numeric axes + a label, laid
# out so all four median-split quadrants are populated (efficient / underperforming
# / high-high / low-low). Deterministic (no RNG) so the tracked CSV is stable.
# ==============================================================================
district_scatter <- tibble::tribble(
  ~district,          ~spending_index, ~outcome_score,
  "Abbottabad",        48, 84,  "Sialkot",           52, 80,
  "Sargodha",          58, 74,  "Mardan",            55, 72,
  "Gujranwala",        62, 78,  "Rawalpindi",        64, 70,
  "Islamabad",         88, 90,  "Lahore",            82, 85,
  "Faisalabad",        75, 76,  "Karachi",           90, 72,
  "Multan",            72, 69,  "Peshawar",          78, 68,
  "Vehari",            44, 52,  "Khanewal",          50, 58,
  "Jhang",             56, 48,  "Kasur",             60, 55,
  "Larkana",           46, 45,  "Mirpur Khas",       42, 50,
  "Quetta",            85, 47,  "Hyderabad",         74, 60,
  "Sukkur",            70, 54,  "Bahawalpur",        80, 58,
  "Sheikhupura",       68, 62,  "Dera Ghazi Khan",   76, 49
)
write_csv(district_scatter, here::here("1_data", "master_district_scatter.csv"))

# ==============================================================================
# DATASET 10: Time-to-Event (Kaplan-Meier survival curve, 3.23)
# 240 enrolled participants tracked over a 24-week program. `weeks` is time to
# dropout (event) or end of follow-up (censored); `dropped` is 1 = dropped out,
# 0 = still enrolled at last contact. Treatment has a lower dropout hazard than
# Control, so its retention curve stays higher.
# ==============================================================================
max_weeks <- 24
make_arm <- function(arm, n, hazard) {
  t_event <- rexp(n, rate = hazard)                 # continuous time to dropout
  weeks   <- ceiling(pmin(t_event, max_weeks))      # discretize to program weeks
  dropped <- as.integer(t_event <= max_weeks)       # censored if it exceeds follow-up
  tibble(group = arm, weeks = pmax(1L, as.integer(weeks)), dropped = dropped)
}
time_to_event <- bind_rows(
  make_arm("Treatment", 120, hazard = 1 / 45),      # lower hazard -> better retention
  make_arm("Control",   120, hazard = 1 / 22)
) %>%
  mutate(participant_id = row_number()) %>%
  select(participant_id, group, weeks, dropped)
write_csv(time_to_event, here::here("1_data", "master_time_to_event.csv"))

message("Success! Ten master datasets generated in /1_data. ",
        "(Spatial assets in 1_data/geo/ are versioned/sourced, not generated here.)")