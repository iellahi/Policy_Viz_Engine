# CERP Analytics: Automated Visualization Suite

This repository is a modular, automated reporting engine. It turns raw data into policy-ready PDFs and HTML reports without requiring the user to write any R code.

## Repository Architecture

* **/1_data** | **The Drop Zone:** Place raw `.csv` files here.
* **/2_R** | **The Engine Room:** Houses setup themes and the master render script.
* **/3_templates** | **The Factory:** Contains parameterized `.Rmd` templates. Users only edit the YAML headers here.
* **/4_output** | **The Deliverables:** Final generated HTML and compiled PDF reports.

## Prerequisites & Setup

1. Install **R** and **RStudio**.
2. Initialize the reproducible environment by opening the R console and running:
   ```R
   install.packages("renv")
   renv::restore()
   tinytex::install_tinytex()
   ```

## Quick Start Guide

1. **Drop Data:** Place your new `.csv` into the `/1_data` folder.
2. **Update Parameters:** Open the desired template in `/3_templates` and update the top 5 lines of text (the YAML header) to match your dataset variables and labels.
3. **Generate:** Open `2_R/2.2_master_knit.R` and click "Run". Retrieve your final report from `/4_output`.

## Template Dictionary

Choose the right template for your policy narrative:

* **3.01 Dumbbell Plot:** Show absolute magnitude of change over time (Before vs. After).
* **3.02 Distribution Shift:** Compare the spread of continuous outcomes between two groups.
* **3.03 Forest Plot:** Prove statistical significance of a program's impact (Treatment vs. Control CIs).
* **3.05 Waffle Chart:** Visualize proportional adoption rates (Out of 100).
* **3.06 Slopegraph:** Track clean longitudinal trajectory and ranking changes.
* **3.07 Diverging Stacked Bar:** Display household survey/Likert sentiment cleanly.
* **3.08 Icon Array:** Humanize massive sample attrition counts and population sizes.
* **3.09 Waterfall Chart:** Track budget pipelines, additions, and systemic losses.
* **3.10 Bump Chart:** Highlight specific regional rank changes over time (League Tables).
* **3.11 Bullet Chart:** Track entity performance against specific KPIs and target zones.
* **3.12 Deviation Bar Chart:** Instantly identify outlier performance relative to a baseline average.
* **3.13 Ridgeline Plot:** Show macro-level distribution shifts across an entire population over time.
* **3.14 Calendar Heatmap:** Visualize high-frequency daily interaction and operational consistency over time.

## Troubleshooting & Roadmap

* **Script Failures:** If a template fails during the master render, verify that your CSV contains no `NA` values in critical columns, and ensure your YAML variable names exactly match your CSV headers.
* **Roadmap:**
    * **Git Initialization:** Set up a `.gitignore` file (specifically excluding `/4_output/*` and `/1_data/*` to keep the repo lightweight), initialize the repository, commit the current state, and push the suite to GitHub.
    * **Messy Data Stress Test:** Pull a raw, uncleaned dataset from a previous field project, drop it into the data folder, and run the templates to identify and patch any remaining edge cases.