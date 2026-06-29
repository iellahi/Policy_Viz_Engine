# ==============================================================================
# Script Name: 01_prep_leaps_data.R
# Purpose:     Convert Stata (.dta) files to CSV for repository standardization
# ==============================================================================

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, readr, dplyr, here)

# 1. Load Data
# Assuming your working directory is the root of your cerp_viz_repo
child_data       <- read_dta(here("data", "aer_data_child.dta"))
perceptions_data <- read_dta(here("data", "aer_data_perceptions.dta"))
school_data      <- read_dta(here("data", "aer_data_school.dta"))

# 2. Write Data to CSV
write_csv(child_data,       here("data", "aer_data_child.csv"))
write_csv(perceptions_data, here("data", "aer_data_perceptions.csv"))
write_csv(school_data,      here("data", "aer_data_school.csv"))

# 3. Glimpse Data for Architecture Planning
# Run these lines and copy/paste the console output back here
cat("\n--- CHILD DATA STRUCT ---\n")
glimpse(child_data)

cat("\n--- PERCEPTIONS DATA STRUCT ---\n")
glimpse(perceptions_data)

cat("\n--- SCHOOL DATA STRUCT ---\n")
glimpse(school_data)


# ==============================================================================
# Part 2: Generate "Lite" Datasets for Easy Viewing
# ==============================================================================
library(tidyverse)
library(here)

# 1. Load the full CSVs you generated earlier
school_full <- read_csv(here("data", "aer_data_school.csv"))
child_full  <- read_csv(here("data", "aer_data_child.csv"))

# 2. Extract Core School Variables
school_lite <- school_full %>%
  select(
    # Identifiers & Geography
    schoolid, district, mauzaid, 
    # Treatment Arm
    reportcard, 
    # School Characteristics
    gov,                  # 1 = Public/Gov, 0 = Private
    school_type1,         # Further breakdown of school type
    S_enroll1,            # Baseline Enrollment
    # Test Scores (Standardized Theta)
    S_avg_theta1,         # Baseline Average Score
    S_avg_theta2,         # Endline Average Score
    # Infrastructure (Good for subsetting later)
    has_electricity1, has_wall1, has_toilets1 = school_toilets_perstudent1
  )

# 3. Extract Core Child Variables
child_lite <- child_full %>%
  select(
    # Identifiers
    childcode, district, mauzaid, schoolid = child_schoolid1,
    # Treatment Arm
    reportcard,
    # Demographics
    child_female, child_age1,
    # School Status
    gov,                  # Public vs Private
    child_enrolled1,      # Baseline enrollment status
    child_dropout2,       # Endline dropout status
    # Test Scores
    child_avg_theta1,     # Baseline overall score
    child_avg_theta2,     # Endline overall score
    child_english_theta1, # Baseline English
    child_math_theta1     # Baseline Math
  )

# 4. Save the Lite versions to the data folder
write_csv(school_lite, here("data", "aer_data_school_lite.csv"))
write_csv(child_lite,  here("data", "aer_data_child_lite.csv"))

cat("Lite datasets successfully created in the /data folder!\n")
