# ==============================================================================
# Script Name: 5_tests/stress_spec.R
# Purpose:     Single source of truth for the stress suite (phase 4). Mirrors the
#              YAML headers of the 18 production templates so the generator and
#              the harness agree, in ONE place, on:
#                * which master CSV each template reads,
#                * which viz_*() function it drives, and
#                * the exact non-text argument values that template passes.
#
#              The stress generator (5_tests/stress_data_generator.R) reads the
#              per-file column roles to build corrupted variants; the stress
#              harness (5_tests/stress_harness.R) reads the per-template call
#              spec to invoke each viz_*() the same way its Rmd wrapper does.
#
# Why a registry (not the Rmds): the harness must call viz_*() thousands of times
# (template x variant) in seconds. Round-tripping through rmarkdown::render for
# every case would be far too slow and would drag in pandoc/YAML noise unrelated
# to the chart logic. The render-level pass (5_tests/stress_render.R) is the thin,
# slow layer that DOES knit the real Rmds, once each, to catch Rmd-layer failures.
#
# KEEP IN SYNC: if a template's YAML *_var / option params change, update its
# entry here. Text params (chart_title/subtitle/x_label/y_label/source_note) are
# deliberately omitted — the harness always leaves them "" (auto defaults).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Master files + their column roles ----------------------------------------
# For each master CSV the templates consume, tag every column by the role the
# engine treats it as. The generator uses these roles to aim each corruption at
# columns where it actually bites (e.g. numbers-as-text only hits numeric roles,
# unicode only hits text roles). `key` = the analytic columns a chart depends on
# (NA injection targets these); `id` columns are left intact so rows stay
# addressable.
# ------------------------------------------------------------------------------
stress_files <- list(
  master_micro_survey.csv = list(
    numeric = c("baseline_score", "endline_score"),
    text    = c("district", "gender", "income_tier", "treatment_group",
                "parent_trust", "tech_adopted", "attrition_status"),
    id      = "student_id",
    key     = c("baseline_score", "endline_score", "treatment_group",
                "income_tier", "parent_trust", "tech_adopted", "attrition_status")
  ),
  master_macro_panel.csv = list(
    numeric = c("literacy_rate", "infrastructure_score", "target_score"),
    text    = "district",
    time    = "year",
    key     = c("district", "year", "literacy_rate", "target_score")
  ),
  master_budget_pipeline.csv = list(
    numeric = "budget_amount",
    text    = "budget_stage",
    key     = c("budget_stage", "budget_amount")
  ),
  master_daily_interaction.csv = list(
    numeric = c("active_users", "total_enrolled", "adoption_rate"),
    date    = "date",
    key     = c("date", "adoption_rate")
  ),
  master_district_indicators.csv = list(
    numeric = "literacy_rate",
    text    = "district",
    key     = c("district", "literacy_rate")
  ),
  master_event_panel.csv = list(
    numeric = c("test_score", "event_time", "period"),
    text    = "treated",
    id      = "school_id",
    key     = c("school_id", "period", "event_time", "treated", "test_score")
  ),
  # --- Phase 10 datasets ------------------------------------------------------
  master_sample_flow.csv = list(          # 3.19 CONSORT
    numeric = "n",
    text    = c("stage", "arm", "note"),
    key     = c("stage", "n")
  ),
  master_district_scatter.csv = list(     # 3.22 quadrant scatter
    numeric = c("spending_index", "outcome_score"),
    text    = "district",
    key     = c("district", "spending_index", "outcome_score")
  ),
  master_time_to_event.csv = list(        # 3.23 Kaplan-Meier
    numeric = c("weeks", "dropped"),
    text    = "group",
    id      = "participant_id",
    key     = c("group", "weeks", "dropped")
  )
)

# ------------------------------------------------------------------------------
# 2. Per-template call spec ----------------------------------------------------
# One entry per production template, keyed by its numeric id. Fields:
#   template  file name in 3_templates/ (used by the render-level pass).
#   fn        the viz_*() function name the wrapper calls.
#   data      the master CSV basename (must be a key of stress_files above).
#   args      the exact non-text arguments the wrapper passes, name -> value.
#             Column-referencing args end in "_var" (the harness validates these
#             with cerp_validate, exactly as the Rmd does).
#   spatial   TRUE only for the choropleth, which additionally needs geometry +
#             the crosswalk loaded and passed in (handled specially in the harness).
#   nastiest  the variant name the render-level pass knits this template against
#             (its single worst-case input; see the generator for the names).
# ------------------------------------------------------------------------------
stress_templates <- list(
  "3.01" = list(
    template = "3.01_baseline_endline.Rmd", fn = "viz_dumbbell",
    data = "master_micro_survey.csv",
    args = list(group_var = "treatment_group", before_var = "baseline_score",
                after_var = "endline_score", treatment_label = "Treatment",
                control_label = "Control", metric_name = "Standardized Test Score"),
    nastiest = "nums_as_text"
  ),
  "3.02" = list(
    template = "3.02_distribution_shifts.Rmd", fn = "viz_distribution",
    data = "master_micro_survey.csv",
    args = list(outcome_var = "baseline_score", group_var = "treatment_group",
                group1_label = "Treatment Group", group2_label = "Control Group",
                metric_name = "Baseline Test Score"),
    nastiest = "groups_tiny"
  ),
  "3.03" = list(
    template = "3.03_treatment_effects.Rmd", fn = "viz_forest",
    data = "master_micro_survey.csv",
    args = list(outcome_var = "endline_score", group_var = "treatment_group",
                treatment_label = "Received Program", control_label = "Comparison Group",
                metric_name = "Endline Test Score"),
    nastiest = "zero_variance"
  ),
  "3.04" = list(
    template = "3.04_subgroup_impacts.Rmd", fn = "viz_coefficient",
    data = "master_micro_survey.csv",
    args = list(outcome_var = "endline_score", group_var = "treatment_group",
                subgroup_var = "income_tier",
                metric_name = "Impact on Endline Score (Treatment vs. Control)"),
    nastiest = "zero_variance"
  ),
  "3.05" = list(
    template = "3.05_waffle_chart.Rmd", fn = "viz_waffle",
    data = "master_micro_survey.csv",
    args = list(status_var = "tech_adopted", category_order = "",
                metric_name = "Hardware Rollout Adoption"),
    nastiest = "manycat_tech_adopted"
  ),
  "3.06" = list(
    template = "3.06_slopegraph.Rmd", fn = "viz_slopegraph",
    data = "master_macro_panel.csv",
    args = list(entity_var = "district", time_var = "year", value_var = "literacy_rate",
                start_time = 2021, end_time = 2025, metric_name = "Literacy Rate (%)"),
    nastiest = "missing_years"
  ),
  "3.07" = list(
    template = "3.07_diverging_bar.Rmd", fn = "viz_diverging",
    data = "master_micro_survey.csv",
    args = list(entity_var = "district", response_var = "parent_trust",
                metric_name = "Trust in Local School Data"),
    nastiest = "likert7"
  ),
  "3.08" = list(
    template = "3.08_icon_array.Rmd", fn = "viz_icon_array",
    data = "master_micro_survey.csv",
    args = list(category_var = "attrition_status", scale_factor = 50,
                unit_name = "Households", metric_name = "Survey Completion & Attrition"),
    nastiest = "manycat_attrition_status"
  ),
  "3.09" = list(
    template = "3.09_waterfall_chart.Rmd", fn = "viz_waterfall",
    data = "master_budget_pipeline.csv",
    args = list(category_var = "budget_stage", value_var = "budget_amount",
                final_column_name = "Total Deployed",
                metric_name = "Education Funds Pipeline (PKR)"),
    nastiest = "mixed_signs"
  ),
  "3.10" = list(
    template = "3.10_bump_chart.Rmd", fn = "viz_bump",
    data = "master_macro_panel.csv",
    args = list(entity_var = "district", time_var = "year", value_var = "literacy_rate",
                highlight_entity = "Lahore", metric_name = "Literacy Rank"),
    nastiest = "missing_years"
  ),
  "3.11" = list(
    template = "3.11_bullet_chart.Rmd", fn = "viz_bullet",
    data = "master_macro_panel.csv",
    args = list(entity_var = "district", time_var = "year", filter_time = 2025,
                value_var = "literacy_rate", target_var = "target_score",
                zone_poor_max = 50, zone_fair_max = 75, zone_good_max = 100,
                metric_name = "Literacy Rate vs. Target (%)"),
    nastiest = "multi_per_entity_time"
  ),
  "3.12" = list(
    template = "3.12_deviation_bar.Rmd", fn = "viz_deviation",
    data = "master_macro_panel.csv",
    args = list(entity_var = "district", time_var = "year", filter_time = 2025,
                value_var = "literacy_rate", baseline_target = 65,
                metric_name = "Standardized Literacy Rate"),
    nastiest = "multi_per_entity_time"
  ),
  "3.13" = list(
    template = "3.13_ridgeline_plot.Rmd", fn = "viz_ridgeline",
    data = "master_micro_survey.csv",
    args = list(group_var = "income_tier", value_var = "endline_score",
                max_groups = 8, metric_name = "Endline Test Score"),
    nastiest = "groups_many"
  ),
  "3.14" = list(
    template = "3.14_calendar_heatmap.Rmd", fn = "viz_calendar_heatmap",
    data = "master_daily_interaction.csv",
    args = list(date_var = "date", value_var = "adoption_rate", aggregate = "none",
                metric_name = "Daily Adoption Rate (%)"),
    nastiest = "dates_timestamps"
  ),
  "3.15" = list(
    template = "3.15_choropleth_map.Rmd", fn = "viz_choropleth",
    data = "master_district_indicators.csv", spatial = TRUE,
    geo_file = "pk_districts_demo.geojson", lookup_file = "district_lookup.csv",
    args = list(region_var = "district", value_var = "literacy_rate",
                geo_key = "district", n_bins = 5,
                metric_name = "Adult Literacy Rate (%)"),
    nastiest = "unmatchable_districts"
  ),
  "3.16" = list(
    template = "3.16_event_study.Rmd", fn = "viz_event_study",
    data = "master_event_panel.csv",
    args = list(unit_var = "school_id", time_var = "period",
                event_time_var = "event_time", treat_var = "treated",
                outcome_var = "test_score", ref_period = -1, metric_name = "Test Score"),
    nastiest = "never_treated"
  ),
  "3.17" = list(
    template = "3.17_small_multiples.Rmd", fn = "viz_small_multiples",
    data = "master_macro_panel.csv",
    args = list(facet_var = "district", x_var = "year", y_var = "literacy_rate",
                scale_mode = "fixed", metric_name = "Literacy Rate (%)"),
    nastiest = "single_facet"
  ),
  "3.18" = list(
    template = "3.18_heatmap_matrix.Rmd", fn = "viz_heatmap_matrix",
    data = "master_macro_panel.csv",
    args = list(row_var = "district", col_var = "year", value_var = "literacy_rate",
                aggregate = "none", show_values = TRUE, metric_name = "Literacy Rate (%)"),
    nastiest = "all_na_cells"
  ),
  # --- Phase 10 templates -----------------------------------------------------
  "3.19" = list(
    template = "3.19_consort_flow.Rmd", fn = "viz_consort_flow",
    data = "master_sample_flow.csv",
    args = list(stage_var = "stage", n_var = "n", arm_var = "arm",
                note_var = "note", metric_name = "Participant Flow"),
    nastiest = "stages_out_of_order"
  ),
  "3.20" = list(
    template = "3.20_balance_plot.Rmd", fn = "viz_balance_plot",
    data = "master_micro_survey.csv",
    args = list(treat_var = "treatment_group",
                balance_vars = c("baseline_score", "gender", "income_tier"),
                threshold = 0.1, metric_name = "Covariate Balance"),
    nastiest = "constant_covariate"
  ),
  "3.21" = list(
    template = "3.21_summary_table.Rmd", fn = "viz_summary_table",
    data = "master_micro_survey.csv", table = TRUE,
    args = list(group_var = "treatment_group",
                summary_vars = c("baseline_score", "endline_score", "gender", "income_tier"),
                digits = 1, metric_name = "Sample Characteristics"),
    nastiest = "group_n1"
  ),
  "3.22" = list(
    template = "3.22_scatter_quadrant.Rmd", fn = "viz_scatter_quadrant",
    data = "master_district_scatter.csv",
    args = list(x_var = "spending_index", y_var = "outcome_score",
                label_var = "district", x_split = "median", y_split = "median",
                metric_name = "Spending vs Outcomes"),
    nastiest = "zero_variance_axis"
  ),
  "3.23" = list(
    template = "3.23_survival_curve.Rmd", fn = "viz_survival_curve",
    data = "master_time_to_event.csv",
    args = list(time_var = "weeks", event_var = "dropped", group_var = "group",
                show_ci = TRUE, metric_name = "Program Retention"),
    nastiest = "all_censored"
  )
)

# ------------------------------------------------------------------------------
# 3. Helper: the *_var arguments for a template entry --------------------------
# Mirrors the Rmd wrapper's `params[grepl("_var$", names(params))]` selection, so
# the harness validates exactly the column-referencing params cerp_validate()
# would see on a real render. geo_key is a *_var-shaped arg that names a field in
# the GEOMETRY, not the data, so it is excluded (the wrapper validates it
# separately against the geojson).
# ------------------------------------------------------------------------------
stress_var_args <- function(entry) {
  a <- entry$args
  vars <- a[grepl("_var$", names(a))]
  vars[names(vars) != "geo_key"]
}
