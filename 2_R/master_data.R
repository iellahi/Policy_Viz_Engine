library(tidyverse)
library(lubridate)
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

write_csv(macro_panel, "../1_data/master_macro_panel.csv")

# ==============================================================================
# DATASET 2: Budget Pipeline Data 
# Use for: Waterfall Chart 
# ==============================================================================
budget_pipeline <- tibble(
  budget_stage = c("Allocated", "Admin Loss", "Transport", "Deployed"),
  budget_amount = c(5000000, -450000, -200000, 4350000)
)

write_csv(budget_pipeline, "../1_data/master_budget_pipeline.csv")

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

write_csv(micro_survey, "../1_data/master_micro_survey.csv")

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

write_csv(daily_interaction, "../1_data/master_daily_interaction.csv")

message("Success! Four optimized master datasets generated in /1_data.")