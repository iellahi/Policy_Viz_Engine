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
if (!is.null(real_names) && length(real_names) > 40) {
  picked <- sample(real_names, 40)
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

message("Success! Seven master datasets generated in /1_data. ",
        "(Spatial assets in 1_data/geo/ are versioned/sourced, not generated here.)")