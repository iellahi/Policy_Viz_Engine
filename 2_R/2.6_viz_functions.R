# ==============================================================================
# Script Name: 2.6_viz_functions.R
# Purpose:     ALL chart logic for the templates, one viz_*() function each. Each
#              function takes the loaded data plus the template's *_var / option /
#              text params, does its own data prep, and RETURNS a ggplot object.
#              The Rmd templates are thin wrappers: params -> cerp_load ->
#              cerp_validate -> viz_*() -> print. Chart code is never duplicated
#              between an Rmd and a function.
#
# Sourcing:    source this AFTER 2_R/2.0_setup_theme.R (the functions reference
#              theme objects — cerp_cols, theme_cerp(), cerp_font_caption — and
#              assume the tidyverse is attached). The house helpers in
#              2_R/2.5_helpers.R (cerp_load/cerp_validate/etc.) are used by the
#              wrappers, not here.
#
# Text params: every function ends with the standard report-text block
#              (chart_title, chart_subtitle, x_label, [y_label], source_note).
#              An empty string "" keeps the auto-generated, data-driven default —
#              exactly as the templates behaved before extraction.
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_dumbbell() — 3.01 baseline vs endline dumbbell. Group means Before/After,
# connected per group, with direct "Before"/"After" labels on the top line.
# ------------------------------------------------------------------------------
viz_dumbbell <- function(data,
                         group_var, before_var, after_var,
                         treatment_label, control_label, metric_name,
                         chart_title = "", chart_subtitle = "",
                         x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs only in the columns essential for this visualization
    drop_na(!!sym(group_var), !!sym(before_var), !!sym(after_var)) %>%

    # 2. Scrub and standardize text (fixes trailing spaces, case inconsistencies)
    mutate(
      Clean_Group = str_to_title(str_trim(as.character(!!sym(group_var)))),

      # Intelligently map messy data to the clean Policy Labels defined in YAML
      # and wrap text to 15 characters so long labels don't ruin the Y-axis margin
      Group = case_when(
        str_detect(Clean_Group, "(?i)Treat|Intervention|1|Yes") ~ str_wrap(treatment_label, width = 15),
        TRUE ~ str_wrap(control_label, width = 15)
      )
    ) %>%

    # 3. Efficiently calculate means grouped by our clean labels
    group_by(Group) %>%
    summarize(
      Before = mean(!!sym(before_var), na.rm = TRUE),
      After  = mean(!!sym(after_var), na.rm = TRUE),
      .groups  = "drop"
    ) %>%

    # 4. Reshape data structurally for ggplot
    pivot_longer(
      cols = c(Before, After),
      names_to = "Time",
      values_to = "Score"
    ) %>%
    mutate(
      Time = factor(Time, levels = c("Before", "After"))
    )

  # Isolate data to attach labels strictly to the top line (acting as a clean legend)
  label_data <- plot_data %>%
    filter(Group == str_wrap(treatment_label, width = 15))

  ggplot(plot_data, aes(x = Score, y = Group)) +

    # Draw the connecting line between Before and After
    geom_line(aes(group = Group), color = cerp_cols["neutral"], linewidth = 2) +

    # Draw the Before and After dots
    geom_point(aes(color = Time), size = 5) +

    # Add "Before" and "After" text directly above the top dots
    geom_text(data = label_data, aes(label = Time, color = Time),
              vjust = -2, fontface = "bold", size = 4.5) +

    # Map CERP colors
    scale_color_manual(values = c(
      "Before" = unname(cerp_cols["neutral"]),
      "After"  = unname(cerp_cols["primary"])
    )) +

    # Expand Y limits slightly so the text labels aren't cut off at the top edge
    scale_y_discrete(expand = expansion(mult = c(0.2, 0.4))) +

    theme_cerp() +

    # Strip unnecessary legends
    theme(legend.position = "none") +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Impact Shift:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("Comparing overall averages before and after the program."),
      x = if (nzchar(x_label)) x_label else (metric_name),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_distribution() — 3.02 density plot. One density curve per group, stacked
# vertically, with a dashed median reference line per facet.
# ------------------------------------------------------------------------------
viz_distribution <- function(data,
                             outcome_var, group_var,
                             group1_label, group2_label, metric_name,
                             chart_title = "", chart_subtitle = "",
                             x_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs only for the required variables
    drop_na(!!sym(outcome_var), !!sym(group_var)) %>%

    # 2. Scrub and standardize text inputs
    mutate(
      Clean_Group = str_to_title(str_trim(as.character(!!sym(group_var)))),

      # Intelligently map messy data to the clean Policy Labels defined in YAML
      Group = case_when(
        str_detect(Clean_Group, "(?i)Treat|Intervention|1|Yes|Public") ~ str_wrap(group1_label, width = 25),
        TRUE ~ str_wrap(group2_label, width = 25)
      )
    ) %>%

    # 3. Lock factor levels so Group 1 always plots vertically on top of Group 2
    mutate(
      Group = factor(Group, levels = c(
        str_wrap(group1_label, width = 25),
        str_wrap(group2_label, width = 25)
      ))
    )

  # 4. Calculate medians dynamically to draw the dashed reference lines
  median_data <- plot_data %>%
    group_by(Group) %>%
    summarize(
      median_val = median(!!sym(outcome_var), na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(plot_data, aes(x = !!sym(outcome_var))) +

    # Draw the density curves, filled with the CERP primary color
    geom_density(fill = cerp_cols["primary"], color = NA, alpha = 0.8) +

    # Add the heavy dashed line for the median
    geom_vline(data = median_data, aes(xintercept = median_val),
               color = cerp_cols["accent"], linewidth = 1.2, linetype = "dashed") +

    # Stack the plots vertically based on our scrubbed grouping variable
    facet_wrap(~Group, ncol = 1) +

    theme_cerp() +

    # Strip the Y-axis text (non-technical readers do not need to read raw density decimals)
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(size = 12, face = "bold") # Format the facet labels cleanly
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Distribution Spread:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("Comparing outcomes across groups. The dashed yellow line represents the median."),
      x = if (nzchar(x_label)) x_label else (metric_name),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_forest() — 3.03 forest / point-and-CI plot. Group mean with a 95% CI bar
# (mean ± 1.96·SE), treatment on top, colored primary vs neutral.
# ------------------------------------------------------------------------------
viz_forest <- function(data,
                       outcome_var, group_var,
                       treatment_label, control_label, metric_name,
                       chart_title = "", chart_subtitle = "",
                       x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs only for the required variables
    drop_na(!!sym(outcome_var), !!sym(group_var)) %>%

    # 2. Scrub and standardize text inputs
    mutate(
      Clean_Group = str_to_title(str_trim(as.character(!!sym(group_var)))),

      # Intelligently map messy data to the clean Policy Labels defined in YAML
      Group = case_when(
        str_detect(Clean_Group, "(?i)Treat|Intervention|1|Yes|Received") ~ str_wrap(treatment_label, width = 20),
        TRUE ~ str_wrap(control_label, width = 20)
      )
    ) %>%

    # 3. Group data to calculate statistics efficiently
    group_by(Group) %>%
    summarize(
      Estimate = mean(!!sym(outcome_var), na.rm = TRUE),
      SD = sd(!!sym(outcome_var), na.rm = TRUE),
      N = n(),
      .groups = "drop"
    ) %>%

    # 4. Calculate Standard Error and 95% Confidence Intervals
    mutate(
      SE = SD / sqrt(N),
      Lower_CI = Estimate - (1.96 * SE),
      Upper_CI = Estimate + (1.96 * SE)
    ) %>%

    # 5. Lock factor levels so the Treatment group always plots on top
    mutate(
      Group = factor(Group, levels = c(
        str_wrap(treatment_label, width = 20),
        str_wrap(control_label, width = 20)
      ))
    )

  # Create a named color vector dynamically based on the wrapped YAML labels
  group_colors <- setNames(
    c(unname(cerp_cols["primary"]), unname(cerp_cols["neutral"])),
    c(str_wrap(treatment_label, width = 20), str_wrap(control_label, width = 20))
  )

  ggplot(plot_data, aes(x = Estimate, y = Group, color = Group)) +

    # Draw the 95% Confidence Interval error bars
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0, linewidth = 2) +

    # Draw the point estimate (the average)
    geom_point(size = 7) +

    # Map the colors explicitly so treatment is primary and control is neutral
    scale_color_manual(values = group_colors) +

    theme_cerp() +

    # Remove the legend entirely and clean axes
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 12, face = "bold")
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Estimated Impact on", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("Points represent the average outcome. Lines represent the 95% confidence interval.\nIf the horizontal lines do not overlap, the difference is statistically significant."),
      x = if (nzchar(x_label)) x_label else (metric_name),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_coefficient() — 3.04 subgroup coefficient plot. Average treatment effect
# (Treatment - Control) per subgroup with 95% CIs; colored by significance and
# sorted by effect size, anchored on a zero reference line.
# ------------------------------------------------------------------------------
viz_coefficient <- function(data,
                            outcome_var, group_var, subgroup_var, metric_name,
                            chart_title = "", chart_subtitle = "",
                            x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs only for the specific variables used in this plot
    drop_na(!!sym(outcome_var), !!sym(group_var), !!sym(subgroup_var)) %>%

    # 2. Scrub and map messy text to strict internal computational labels
    mutate(
      Clean_Group = str_to_title(str_trim(as.character(!!sym(group_var)))),
      Internal_Group = case_when(
        str_detect(Clean_Group, "(?i)Treat|Intervention|1|Yes|Received") ~ "TREAT",
        TRUE ~ "CTRL"
      ),
      # Clean up the subgroup categories (e.g., standardizing "low", "Low ", "LOW")
      Subgroup = str_to_title(str_trim(as.character(!!sym(subgroup_var))))
    ) %>%

    # 3. Calculate Mean, Variance, and Count per Subgroup & Treatment Arm
    group_by(Subgroup, Internal_Group) %>%
    summarize(
      Mean = mean(!!sym(outcome_var), na.rm = TRUE),
      Var = var(!!sym(outcome_var), na.rm = TRUE),
      N = n(),
      .groups = "drop"
    ) %>%

    # 4. Reshape data so Treatment and Control are side-by-side for each Subgroup
    pivot_wider(
      names_from = Internal_Group,
      values_from = c(Mean, Var, N)
    ) %>%

    # 5. Calculate the Average Treatment Effect (ATE) and 95% Confidence Intervals
    mutate(
      ATE = Mean_TREAT - Mean_CTRL,
      # Standard Error of the difference between two means
      SE = sqrt((Var_TREAT / N_TREAT) + (Var_CTRL / N_CTRL)),
      Lower_CI = ATE - (1.96 * SE),
      Upper_CI = ATE + (1.96 * SE),

      # Check if the effect is statistically significant (CI doesn't cross zero)
      Is_Sig = ifelse(Lower_CI > 0 | Upper_CI < 0, "Significant", "Not Significant"),

      # Sort the subgroups from highest impact to lowest for a clean visual hierarchy
      Subgroup = fct_reorder(Subgroup, ATE)
    )

  ggplot(plot_data, aes(y = Subgroup, x = ATE)) +

    # The Null Effect Line: Anchors the entire chart at zero difference
    geom_vline(xintercept = 0, color = cerp_cols[["text"]], linetype = "dashed", linewidth = 1) +

    # Draw the 95% Confidence Intervals
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI, color = Is_Sig),
                   height = 0, linewidth = 2) +

    # Draw the Point Estimate (Average Treatment Effect)
    geom_point(aes(color = Is_Sig), size = 6) +

    # Direct labels showing the exact numerical impact
    geom_text(aes(label = sprintf("%+0.1f", ATE)),
              vjust = -1.5, fontface = "bold", size = 4) +

    # Color-code based on significance (Grey if it touches zero, Primary color if significant)
    scale_color_manual(values = c(
      "Significant" = unname(cerp_cols["primary"]),
      "Not Significant" = unname(cerp_cols["neutral"])
    )) +

    theme_cerp() +

    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 12, face = "bold"),
      panel.grid.minor.x = element_blank()
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Subgroup Impact:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("Comparing average treatment effects across demographics.\nGrey lines cross zero, indicating no statistically significant impact for that group."),
      x = if (nzchar(x_label)) x_label else ("Difference (Treatment - Control)"),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_waffle() — 3.05 waffle chart. Any number of categories (<= 8) rendered as a
# 10x10 grid; largest-remainder rounding makes the squares sum to exactly 100.
# Uses semantic colors when the categories carry universal meaning.
# ------------------------------------------------------------------------------
viz_waffle <- function(data,
                       status_var, category_order, metric_name,
                       chart_title = "", chart_subtitle = "", source_note = "") {

  # 1. Count each category of the status variable
  cat_counts <- data %>%
    filter(!is.na(.data[[status_var]]),
           nzchar(as.character(.data[[status_var]]))) %>%
    count(Category = as.character(.data[[status_var]]), name = "n")

  if (nrow(cat_counts) == 0) stop("No non-missing values found in '", status_var, "'.")
  if (nrow(cat_counts) > 8) stop(
    "'", status_var, "' has ", nrow(cat_counts),
    " distinct categories — too many for a readable waffle chart. ",
    "Choose a variable with 8 or fewer categories."
  )

  # 2. Resolve category order: YAML-specified, else by frequency
  if (nzchar(category_order)) {
    wanted <- str_squish(str_split(category_order, ",")[[1]])
    lvls <- c(intersect(wanted, cat_counts$Category),
              setdiff(cat_counts$Category, wanted))
  } else {
    lvls <- cat_counts %>% arrange(desc(n)) %>% pull(Category)
  }

  # 3. Exact percentages + largest-remainder rounding so squares sum to exactly 100
  cat_counts <- cat_counts %>%
    mutate(Category = factor(Category, levels = lvls)) %>%
    arrange(Category) %>%
    mutate(pct_exact = 100 * n / sum(n))

  squares <- floor(cat_counts$pct_exact)
  leftover <- 100 - sum(squares)
  if (leftover > 0) {
    top_up <- order(cat_counts$pct_exact - squares, decreasing = TRUE)[seq_len(leftover)]
    squares[top_up] <- squares[top_up] + 1
  }
  cat_counts$squares <- squares

  # 4. Legend labels carry the exact share, so the chart needs no axis at all
  cat_counts <- cat_counts %>%
    mutate(Label = paste0(Category, " — ", round(pct_exact), "%"))

  # 5. Build the 10x10 grid, filled bottom-to-top, left-to-right
  waffle_data <- expand.grid(x = 1:10, y = 1:10) %>%
    arrange(y, x) %>%
    mutate(Label = factor(
      rep(cat_counts$Label, times = cat_counts$squares),
      levels = cat_counts$Label
    ))

  # 6. Colors: semantic if the categories carry universal meaning
  #    (Green/Yellow/Red triage, Yes/No, risk levels), otherwise house palette
  semantic <- cerp_match_semantic(as.character(cat_counts$Category))
  waffle_colors <- setNames(
    if (!is.null(semantic)) unname(semantic) else cerp_fill_n(nrow(cat_counts)),
    cat_counts$Label
  )

  # 7. Headline for the subtitle: lead with the largest category
  top_cat <- cat_counts %>% slice_max(pct_exact, n = 1, with_ties = FALSE)

  ggplot(waffle_data, aes(x = x, y = y, fill = Label)) +

    # Squares with a background-colored border for the classic waffle separation
    geom_tile(color = cerp_cols[["bg"]], linewidth = 2) +

    # Lock the aspect ratio so squares stay square
    coord_equal() +

    scale_fill_manual(values = waffle_colors) +

    theme_cerp() +

    # Strip all axes and grids — the legend percentages tell the whole story
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.justification = "left"
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste0( "Each square is 1% of the sample (N = ", scales::comma(sum(cat_counts$n)), "). ", top_cat$Category, " accounts for ", round(top_cat$pct_exact), "%." )),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_slopegraph() — 3.06 slopegraph. Two anchor times per entity, connected,
# with directly-labeled endpoints (ggrepel) and no y-axis. Requires ggrepel
# (attached by the wrapper); referenced here namespaced as ggrepel::.
# ------------------------------------------------------------------------------
viz_slopegraph <- function(data,
                           entity_var, time_var, value_var,
                           start_time, end_time, metric_name,
                           chart_title = "", chart_subtitle = "",
                           x_label = "", y_label = "", source_note = "") {

  # Define string parameters OUTSIDE the mutate chain to prevent vector duplication
  start_str <- as.character(start_time)
  end_str <- as.character(end_time)

  plot_data <- data %>%
    # 1. Safely drop NAs only for the specific variables required
    drop_na(!!sym(time_var), !!sym(entity_var), !!sym(value_var)) %>%

    # 2. Type-Safe Filtering: Convert data to character to match our safe parameters
    mutate(Time_Raw = as.character(!!sym(time_var))) %>%
    filter(Time_Raw %in% c(start_str, end_str)) %>%

    # 3. Scrub and wrap Entity names
    mutate(
      Entity = str_wrap(str_to_title(str_trim(as.character(!!sym(entity_var)))), width = 15),
      Value = as.numeric(!!sym(value_var)),
      # Lock factor levels so start time is ALWAYS on the left
      Time = factor(Time_Raw, levels = c(start_str, end_str))
    )

  # 4. Isolate data for direct left and right text labeling to avoid using a messy Y-axis
  left_labels <- plot_data %>% filter(Time == start_str)
  right_labels <- plot_data %>% filter(Time == end_str)

  ggplot(plot_data, aes(x = Time, y = Value, group = Entity)) +

    # Draw the connecting lines
    geom_line(color = cerp_cols["neutral"], linewidth = 1.2, alpha = 0.8) +

    # Draw the anchor points at start and end
    geom_point(color = cerp_cols["primary"], size = 3.5) +

    # Direct Left Labels: Name + Value, repelled vertically so entities that
    # start at the same value fan out instead of overprinting
    ggrepel::geom_text_repel(
      data = left_labels,
      aes(label = paste0(Entity, "  ", Value)),
      direction = "y", hjust = 1, nudge_x = -0.15,
      fontface = "bold", size = 4, family = cerp_font_caption,
      color = cerp_cols[["text"]],
      box.padding = 0.15, point.padding = 0.3,
      segment.color = "#b7c9d3", segment.size = 0.3, seed = 26
    ) +

    # Direct Right Labels: Value + Name, so every line is identifiable at BOTH
    # ends without tracing it back across the chart
    ggrepel::geom_text_repel(
      data = right_labels,
      aes(label = paste0(Value, "  ", Entity)),
      direction = "y", hjust = 0, nudge_x = 0.15,
      fontface = "bold", size = 4, family = cerp_font_caption,
      color = cerp_cols[["text"]],
      box.padding = 0.15, point.padding = 0.3,
      segment.color = "#b7c9d3", segment.size = 0.3, seed = 26
    ) +

    # Expand limits horizontally so the text does not get cut off by the edge of the image
    scale_x_discrete(expand = expansion(mult = c(0.35, 0.35))) +

    theme_cerp() +

    # Strip the Y-axis entirely for maximum data-ink ratio
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 14, face = "bold")
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Shift in", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste0("Tracking regional trajectories from ", start_time, " to ", end_time, ".")),
      x = if (nzchar(x_label)) x_label else (NULL),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_diverging() — 3.07 diverging stacked bar (Likert). Maps messy responses to
# a fixed 4-point scale, plots negatives left / positives right of a zero line,
# sorted by net-positive share. Palette is intentionally the fixed Likert ramp.
# ------------------------------------------------------------------------------
viz_diverging <- function(data,
                          entity_var, response_var, metric_name,
                          chart_title = "", chart_subtitle = "",
                          x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs only for required variables
    drop_na(!!sym(entity_var), !!sym(response_var)) %>%

    # 2. Scrub strings and wrap long Entity labels (like long District names)
    mutate(
      Entity = str_wrap(str_to_title(str_trim(as.character(!!sym(entity_var)))), width = 15),
      Clean_Resp = str_trim(as.character(!!sym(response_var))),

      # 3. Intelligently map messy Likert scale data into strict categories
      Response = case_when(
        str_detect(Clean_Resp, "(?i)Strongly Dis") ~ "Strongly Disagree",
        str_detect(Clean_Resp, "(?i)Dis") & !str_detect(Clean_Resp, "(?i)Strongly") ~ "Disagree",
        str_detect(Clean_Resp, "(?i)Strongly Ag") ~ "Strongly Agree",
        str_detect(Clean_Resp, "(?i)Ag") & !str_detect(Clean_Resp, "(?i)Strongly") ~ "Agree",
        TRUE ~ NA_character_ # Flag unrecognizable garbage data as NA
      )
    ) %>%

    # Drop any garbage data that couldn't be mapped to our 4 categories
    drop_na(Response) %>%

    # 4. Calculate counts and percentages per Entity
    count(Entity, Response) %>%
    group_by(Entity) %>%
    mutate(
      Pct = n / sum(n),
      # Flip the sign for negative sentiments so they plot to the left of zero
      Plot_Pct = ifelse(str_detect(Response, "Disagree"), -Pct, Pct),

      # Lock factor levels so they stack outward from the center zero-line correctly
      Response = factor(Response, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
    ) %>%
    ungroup()

  # 5. Calculate the net positive score to sort the Y-axis dynamically
  sort_order <- plot_data %>%
    filter(str_detect(Response, "Agree")) %>%
    group_by(Entity) %>%
    summarize(Net_Positive = sum(Pct, na.rm = TRUE), .groups = "drop") %>%
    arrange(Net_Positive) %>%
    pull(Entity)

  plot_data <- plot_data %>%
    mutate(Entity = factor(Entity, levels = sort_order))

  ggplot(plot_data, aes(x = Plot_Pct, y = Entity, fill = Response)) +

    # Draw the stacked bars
    geom_col(width = 0.6) +

    # Drop a heavy vertical line exactly at zero to anchor the eye
    geom_vline(xintercept = 0, color = cerp_cols[["text"]], linewidth = 1) +

    # Use a high-contrast diverging color palette.
    scale_fill_manual(values = c(
      "Strongly Disagree" = "#e07a5f", # Terracotta (strong negative)
      "Disagree"          = "#f2cc8f", # Sand (mild negative)
      "Agree"             = "#a8dadc", # Pale teal (mild positive)
      "Strongly Agree"    = "#457b9d"  # Steel blue (strong positive)
    )) +

    # Format the X-axis as percentages and strip away the negative signs
    scale_x_continuous(
      labels = function(x) scales::percent(abs(x), accuracy = 1),
      limits = c(-1, 1) # Lock the scale from -100% to +100%
    ) +

    theme_cerp() +

    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank(), # Remove horizontal lines to keep bars clean
      axis.text.y = element_text(size = 12, face = "bold")
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Household Sentiment:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("Percentage of respondents leaning negative (left) vs. positive (right)."),
      x = if (nzchar(x_label)) x_label else ("Percentage of Respondents"),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_icon_array() — 3.08 icon array. One dot per `scale_factor` units, grouped
# into a rough square, colored by attrition category.
# ------------------------------------------------------------------------------
viz_icon_array <- function(data,
                           category_var, scale_factor, unit_name, metric_name,
                           chart_title = "", chart_subtitle = "", source_note = "") {

  # 1. Calculate the raw counts directly from the microdata
  array_data <- data %>%
    # Drop missing statuses
    drop_na(!!sym(category_var)) %>%

    # Scrub text and group messy data into strict attrition categories
    mutate(
      Clean_Status = str_trim(as.character(!!sym(category_var))),
      Category = case_when(
        str_detect(Clean_Status, "(?i)Survey|Complete|Success") ~ "Surveyed",
        str_detect(Clean_Status, "(?i)Refuse|Deny") ~ "Refused",
        str_detect(Clean_Status, "(?i)Not Reach|Unreach|Miss|Absent") ~ "Not Reached",
        TRUE ~ "Other"
      )
    ) %>%

    # Aggregate the counts
    count(Category, name = "count") %>%

    # 2. Calculate the number of dots needed based on the YAML scale factor
    mutate(dots_needed = round(count / scale_factor)) %>%
    # Drop categories that round down to 0 dots
    filter(dots_needed > 0)

  # 3. "Uncount" the data: Copies each row N times so we have exactly one row per dot
  dot_df <- array_data %>%
    uncount(dots_needed) %>%
    mutate(id = row_number())

  # 4. Calculate grid dimensions dynamically to make it a rough square
  total_dots <- nrow(dot_df)
  cols <- ceiling(sqrt(total_dots))

  # 5. Assign X and Y coordinates to construct the neat grid
  dot_df <- dot_df %>%
    mutate(
      x = (id - 1) %% cols + 1,
      y = ceiling(id / cols),
      # Lock the factor levels so the most important categories plot and color correctly
      Category = factor(Category, levels = c("Surveyed", "Refused", "Not Reached", "Other"))
    )

  ggplot(dot_df, aes(x = x, y = y, color = Category)) +

    # Draw the "Icons" (thick circles).
    # Using shape 16 (solid dot) with size 5 creates a clean, readable dot matrix.
    geom_point(size = 6, shape = 16, alpha = 0.9) +

    # coord_equal prevents the dots from stretching into ovals
    coord_equal() +

    # Use high-contrast colors to differentiate success from attrition
    scale_color_manual(values = c(
      "Surveyed"    = unname(cerp_cols["primary"]),  # Dark Blue/CERP Primary
      "Refused"     = "#E69F00",                     # Orange/Warning
      "Not Reached" = unname(cerp_cols["neutral"]),  # Grey
      "Other"       = "#B0B0B0"                      # Light Grey
    )) +

    theme_cerp() +

    # Strip all axes, gridlines, and backgrounds to leave only the dots floating in space
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 11, face = "bold")
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste0("Each dot represents ", scale_factor, " ", tolower(unit_name), ".")),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_waterfall() — 3.09 waterfall. Floating bars from a start total through
# additions/deductions to a computed end total, with dashed connectors.
# ------------------------------------------------------------------------------
viz_waterfall <- function(data,
                          category_var, value_var, final_column_name, metric_name,
                          chart_title = "", chart_subtitle = "",
                          x_label = "", y_label = "", source_note = "") {

  # 1. Standardize the data and drop NAs
  clean_data <- data %>%
    drop_na(!!sym(category_var), !!sym(value_var)) %>%
    mutate(
      Category = str_wrap(str_to_title(str_trim(as.character(!!sym(category_var)))), width = 15),
      Value = as.numeric(!!sym(value_var))
    )

  # 2. Intelligent Summary Detection
  # Checks if the user already included the final total in the CSV.
  # If yes, we strip it out so we can build the mathematical anchors perfectly.
  n_rows <- nrow(clean_data)
  if (n_rows > 1) {
    sum_previous <- sum(clean_data$Value[1:(n_rows-1)], na.rm = TRUE)
    last_val <- clean_data$Value[n_rows]

    if (round(abs(sum_previous)) == round(abs(last_val))) {
      clean_data <- clean_data[1:(n_rows-1), ]
    }
  }

  # 3. Calculate the true final landing total
  final_total <- sum(clean_data$Value, na.rm = TRUE)

  # 4. Build the Waterfall coordinates
  plot_data <- clean_data %>%
    # Append our calculated final row
    bind_rows(tibble(Category = final_column_name, Value = final_total)) %>%
    mutate(
      id = row_number(),
      # Categorize blocks to assign specific brand colors later
      Type = case_when(
        id == 1 ~ "Start",
        id == n() ~ "End",
        Value > 0 ~ "Addition",
        TRUE ~ "Deduction"
      ),
      # Lock the factor levels so the pipeline flows left-to-right correctly
      Category = factor(Category, levels = Category),

      # Calculate the top and bottom coordinates for the floating bars
      ymax = cumsum(Value),
      ymin = lag(ymax, default = 0)
    ) %>%
    # Correct the coordinates for the Start and End blocks so they are anchored to zero
    mutate(
      ymin = if_else(Type %in% c("Start", "End"), 0, ymin),
      ymax = if_else(Type == "End", final_total, ymax),

      # Coordinates for drawing the dashed connecting lines between bars
      next_id = id + 1,
      next_ymax = ymax
    )

  ggplot(plot_data, aes(fill = Type)) +

    # Draw the floating rectangles
    geom_rect(aes(xmin = id - 0.4, xmax = id + 0.4, ymin = ymin, ymax = ymax),
              color = "white", linewidth = 0.5) +

    # Draw the classic dashed waterfall connecting lines
    geom_segment(data = plot_data %>% filter(Type != "End"),
                 aes(x = id + 0.4, xend = next_id - 0.4, y = ymax, yend = ymax),
                 linetype = "dashed", color = cerp_cols[["text"]]) +

    # Add direct text labels to the bars automatically formatting massive numbers
    geom_text(aes(x = id,
                  y = if_else(Type == "Deduction", ymin - (ymin - ymax)/2, ymax + (ymax - ymin)/2),
                  label = scales::comma(abs(Value))),
              vjust = 0.5, fontface = "bold", size = 4, color = "black") +

    # Map policy-friendly colors: CERP Primary for anchors, Red for loss, Blue for gains
    scale_fill_manual(values = c(
      "Start"     = unname(cerp_cols["primary"]),
      "End"       = unname(cerp_cols["primary"]),
      "Deduction" = "#e07a5f",  # Terracotta (loss)
      "Addition"  = "#a8dadc"   # Pale teal (gain)
    )) +

    # Format the Y-axis to handle large financial numbers cleanly
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +

    # Replace the numeric X-axis with our actual category names
    scale_x_continuous(breaks = plot_data$id, labels = plot_data$Category) +

    theme_cerp() +

    # Strip unnecessary background grid elements and the legend
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 0, size = 11, face = "bold")
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Pipeline Breakdown:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("Tracking total allocations against systemic reductions to determine final delivery."),
      x = if (nzchar(x_label)) x_label else (NULL),
      y = if (nzchar(y_label)) y_label else (metric_name),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_bump() — 3.10 bump chart. Yearly ranks per entity, one highlighted; tied
# scores flagged with an asterisk. Requires ggrepel (attached by the wrapper).
# ------------------------------------------------------------------------------
viz_bump <- function(data,
                     entity_var, time_var, value_var, highlight_entity, metric_name,
                     chart_title = "", chart_subtitle = "",
                     x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs only for the specific variables required
    drop_na(!!sym(time_var), !!sym(entity_var), !!sym(value_var)) %>%

    # 2. Scrub and wrap Entity names, and force Time/Value to numeric types
    mutate(
      Entity_Clean = str_to_title(str_trim(as.character(!!sym(entity_var)))),
      Entity = str_wrap(Entity_Clean, width = 15),
      Time = as.numeric(!!sym(time_var)),
      Value = as.numeric(!!sym(value_var))
    ) %>%

    # 3. Group by year to establish who is in 1st, 2nd, 3rd place, etc.
    group_by(Time) %>%
    mutate(
      # Strict tie-breaker for vertical spacing on the chart
      Rank = row_number(desc(Value)),
      # Mathematical detection: Is this exact score duplicated in this year?
      is_tied = duplicated(Value) | duplicated(Value, fromLast = TRUE)
    ) %>%
    ungroup() %>%

    # 4. Create safe styling rules and dynamic text labels
    mutate(
      target_clean = str_to_title(str_trim(highlight_entity)),
      is_highlight = ifelse(Entity_Clean == target_clean, TRUE, FALSE),
      # If tied, append an asterisk to the name
      Display_Name = ifelse(is_tied, paste0(Entity, "*"), Entity)
    )

  # 5. Isolate the starting and ending points to draw direct text labels
  start_labels <- plot_data %>% filter(Time == min(Time))
  end_labels <- plot_data %>% filter(Time == max(Time))

  ggplot(plot_data, aes(x = Time, y = Rank, group = Entity)) +

    # Draw the trajectory lines
    geom_line(aes(
      color = is_highlight,
      linewidth = is_highlight,
      alpha = is_highlight
    )) +

    # Draw the specific rank points for each year
    geom_point(aes(
      color = is_highlight,
      size = is_highlight
    )) +

    # Left-side labels (Start) — repelled vertically so tied ranks never overprint
    ggrepel::geom_text_repel(
      data = start_labels, aes(label = Display_Name, color = is_highlight),
      direction = "y", hjust = 1, nudge_x = -0.15,
      fontface = "bold", size = 4, family = cerp_font_caption,
      box.padding = 0.12, segment.color = "#b7c9d3", segment.size = 0.3, seed = 26
    ) +

    # Right-side labels (End) — same protection
    ggrepel::geom_text_repel(
      data = end_labels, aes(label = Display_Name, color = is_highlight),
      direction = "y", hjust = 0, nudge_x = 0.15,
      fontface = "bold", size = 4, family = cerp_font_caption,
      box.padding = 0.12, segment.color = "#b7c9d3", segment.size = 0.3, seed = 26
    ) +

    # Apply conditional styling
    scale_color_manual(values = c("TRUE" = unname(cerp_cols["primary"]), "FALSE" = "#B0B0B0")) +
    scale_linewidth_manual(values = c("TRUE" = 2.5, "FALSE" = 1)) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
    scale_size_manual(values = c("TRUE" = 5, "FALSE" = 3)) +

    # Reverse the Y-axis so Rank #1 is at the top of the chart
    scale_y_reverse(breaks = 1:max(plot_data$Rank)) +

    # FIX: Expanded the X-axis margins (40% left, 20% right) to prevent long names from clipping
    scale_x_continuous(breaks = unique(plot_data$Time),
                       expand = expansion(mult = c(0.4, 0.2))) +

    theme_cerp() +

    # Strip everything unnecessary from the plot
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_text(size = 14, face = "bold", color = cerp_cols[["text"]]),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste(metric_name, "Trajectory")),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste("Tracking regional shifts over time. Highlighting the performance of", highlight_entity, ".")),
      x = if (nzchar(x_label)) x_label else (NULL),
      y = if (nzchar(y_label)) y_label else ("Rank"),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_bullet() — 3.11 bullet chart. Actual value bar over graded qualitative
# zones with a target marker, one row per entity, filtered to one time period.
# ------------------------------------------------------------------------------
viz_bullet <- function(data,
                       entity_var, time_var, filter_time, value_var, target_var,
                       zone_poor_max, zone_fair_max, zone_good_max, metric_name,
                       chart_title = "", chart_subtitle = "",
                       x_label = "", y_label = "", source_note = "") {

  # Define string parameters OUTSIDE the mutate chain to prevent vector duplication
  filter_str <- as.character(filter_time)

  plot_data <- data %>%
    # 1. Safely drop NAs for required columns
    drop_na(!!sym(time_var), !!sym(entity_var), !!sym(value_var), !!sym(target_var)) %>%

    # 2. Type-Safe Filtering for the specific time period
    mutate(Time_Raw = as.character(!!sym(time_var))) %>%
    filter(Time_Raw == filter_str) %>%

    # 3. Scrub labels and force strict numeric types for chart math
    mutate(
      Entity = str_wrap(str_to_title(str_trim(as.character(!!sym(entity_var)))), width = 15),
      Value = as.numeric(!!sym(value_var)),
      Target = as.numeric(!!sym(target_var)),
      Poor = as.numeric(zone_poor_max),
      Fair = as.numeric(zone_fair_max),
      Good = as.numeric(zone_good_max)
    ) %>%

    # 4. Sort entities by performance so the chart flows logically
    arrange(Value) %>%
    mutate(Entity = factor(Entity, levels = unique(Entity)))

  ggplot(plot_data, aes(y = Entity)) +

    # 1. Background Zones (Drawn widest to narrowest so they stack correctly)
    # Good Zone (Lightest Grey)
    geom_col(aes(x = Good), fill = "#EBEBEB", width = 0.6) +
    # Fair Zone (Medium Grey)
    geom_col(aes(x = Fair), fill = "#D6D6D6", width = 0.6) +
    # Poor Zone (Dark Grey)
    geom_col(aes(x = Poor), fill = "#BFBFBF", width = 0.6) +

    # 2. Actual Performance Bar (Thinner, filled with primary brand color)
    geom_col(aes(x = Value), fill = cerp_cols["primary"], width = 0.25) +

    # 3. Target Marker (Using geom_segment forces rendering without range-guessing)
    geom_segment(aes(
        x = Target, xend = Target,
        y = as.numeric(Entity) - 0.3, yend = as.numeric(Entity) + 0.3
      ),
      color = cerp_cols[["text"]], linewidth = 1.2
    ) +

    theme_cerp() +

    # Set the X-axis limits based on the good zone, adding a tiny right margin to avoid clipping targets
    scale_x_continuous(limits = c(0, zone_good_max), expand = expansion(mult = c(0, 0.05))) +

    # Clean up the theme for a minimal dashboard look
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold", color = cerp_cols[["text"]]),
      axis.ticks.y = element_blank()
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("KPI Tracking:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste0("Status as of ", filter_time, ". Dark grey: <", zone_poor_max, "%, Medium: <", zone_fair_max, "%, Light: <", zone_good_max, "%. Black line indicates target.")),
      x = if (nzchar(x_label)) x_label else (metric_name),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_deviation() — 3.12 deviation bar. Pre-aggregates to one value per entity,
# benchmarks against a fixed target or the grand mean, and plots the signed gap.
# The wrapper sizes fig.height from nrow(p$data).
# ------------------------------------------------------------------------------
viz_deviation <- function(data,
                          entity_var, time_var, filter_time, value_var,
                          baseline_target, metric_name,
                          chart_title = "", chart_subtitle = "",
                          x_label = "", y_label = "", source_note = "") {

  d <- data

  # 1. Time filter (optional): build a comparable key from whatever the time
  #    column contains — "2025", "2025-11-01", or "2025-11-01T04:36:41Z"
  if (nzchar(time_var) && nzchar(as.character(filter_time))) {
    time_raw <- d[[time_var]]
    as_dates <- cerp_parse_date(time_raw, quiet = TRUE)
    time_key <- if (mean(!is.na(as_dates)) > 0.8) {
      as.character(year(as_dates))            # date-like column -> compare by year
    } else {
      str_squish(as.character(time_raw))      # plain years/labels -> compare as text
    }
    d <- d %>% filter(time_key == str_squish(as.character(filter_time)))

    if (nrow(d) == 0) stop(
      "No rows remain after filtering '", time_var, "' to '",
      filter_time, "'. Values present in the data: ",
      paste(sort(unique(time_key)), collapse = ", ")
    )
  }

  # 2. Pre-aggregate: one summary value per entity (mean of all its rows).
  #    Harmless when data is already one-row-per-entity.
  agg <- d %>%
    mutate(
      Entity = str_to_title(str_squish(as.character(.data[[entity_var]]))),
      Value  = cerp_numeric(.data[[value_var]], value_var)
    ) %>%
    drop_na(Entity, Value) %>%
    group_by(Entity) %>%
    summarize(Value = mean(Value), n_obs = n(), .groups = "drop")

  # 3. Resolve the baseline: fixed policy target, or the grand mean
  use_mean_baseline <- identical(tolower(as.character(baseline_target)), "mean")
  target <- if (use_mean_baseline) mean(agg$Value) else as.numeric(baseline_target)

  baseline_text <- if (use_mean_baseline) {
    paste0("the average across all ", nrow(agg), " entities (",
           round(target, 1), ")")
  } else {
    paste0("the target of ", round(target, 1))
  }

  # 4. Deviations, sorted so the biggest laggard sits at the bottom
  plot_data <- agg %>%
    mutate(
      Entity_Label = str_wrap(Entity, width = 18),
      Deviation = Value - target,
      Performance = ifelse(Deviation >= 0, "Above baseline", "Below baseline")
    ) %>%
    arrange(Deviation) %>%
    mutate(Entity_Label = factor(Entity_Label, levels = unique(Entity_Label)))

  perf_colors <- c(
    "Above baseline" = unname(cerp_cols["primary"]),
    "Below baseline" = unname(cerp_cols["accent"])
  )

  ggplot(plot_data, aes(x = Deviation, y = Entity_Label, fill = Performance)) +

    geom_col(width = 0.7) +

    # Direct labels: the exact +/- gap at the end of each bar
    geom_text(
      aes(
        label = ifelse(Deviation > 0,
                       paste0("+", round(Deviation, 1)),
                       round(Deviation, 1)),
        hjust = ifelse(Deviation > 0, -0.25, 1.25)
      ),
      fontface = "bold", size = 3.4, family = cerp_font_caption,
      color = cerp_cols[["text"]]
    ) +

    # The baseline anchor
    geom_vline(xintercept = 0, color = cerp_cols[["text"]], linewidth = 0.9) +

    scale_fill_manual(values = perf_colors) +
    scale_x_continuous(expand = expansion(mult = c(0.18, 0.18))) +

    theme_cerp() +

    # Direct labels replace the x-axis entirely
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold",
                                 color = cerp_cols[["text"]])
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste0(metric_name, ": Who Is Ahead, Who Is Behind")),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste0( "Gap relative to ", baseline_text, if (nzchar(time_var) && nzchar(as.character(filter_time))) paste0(", ", filter_time) else "", "." )),
      x = if (nzchar(x_label)) x_label else ("Deviation from baseline"),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_ridgeline() — 3.13 ridgeline. Overlapping per-group densities ordered by
# median, high-cardinality groups lumped into "Other". Requires ggridges
# (attached by the wrapper). The wrapper sizes fig.height from nlevels(p$data$Group).
# ------------------------------------------------------------------------------
viz_ridgeline <- function(data,
                          group_var, value_var, max_groups, metric_name,
                          chart_title = "", chart_subtitle = "",
                          x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    # 1. Safely drop NAs
    drop_na(all_of(c(group_var, value_var))) %>%

    # 2. Scrub strings, enforce numeric values
    mutate(
      Group_Raw = str_to_title(str_squish(as.character(.data[[group_var]]))),
      Value = cerp_numeric(.data[[value_var]], value_var)
    ) %>%
    drop_na(Value) %>%

    # 3. Cardinality guard: keep the max_groups most frequent, lump the rest
    mutate(Group_Lumped = cerp_lump(Group_Raw, n = max_groups)) %>%

    # 4. Density protection: a curve needs enough observations
    group_by(Group_Lumped) %>%
    filter(n() >= 3) %>%
    ungroup() %>%

    # 5. Wrap long names, order ridges by median so the eye reads a ranking
    mutate(
      Group = str_wrap(as.character(Group_Lumped), width = 18),
      Group = fct_reorder(Group, Value, .fun = median, na.rm = TRUE)
    )

  n_lumped <- sum(str_detect(as.character(plot_data$Group), "^Other"))

  # De-emphasize the lumped "Other" ridge; brand color for real groups
  ridge_fills <- setNames(
    ifelse(str_detect(levels(plot_data$Group), "^Other"),
           unname(cerp_cols["neutral"]),
           unname(cerp_cols["primary"])),
    levels(plot_data$Group)
  )

  ggplot(plot_data, aes(x = Value, y = Group, fill = Group)) +

    # Overlapping ridges with an internal median line for instant comparison
    geom_density_ridges(
      scale = 1.7,
      color = cerp_cols[["bg"]],
      linewidth = 0.8,
      alpha = 0.9,
      rel_min_height = 0.01,
      quantile_lines = TRUE,
      quantiles = 2,
      vline_color = "#1b3a4b55"
    ) +

    scale_x_continuous(labels = scales::comma, expand = c(0.01, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.2, 1.4))) +
    scale_fill_manual(values = ridge_fills) +

    theme_cerp() +

    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 12, face = "bold",
                                 color = cerp_cols[["text"]]),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#e5e1d8", linewidth = 0.45)
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("Population Distribution:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else (paste0( "Each ridge is a group's full distribution; the vertical line marks its median. ", "Ridges are ordered by median, highest at the top", if (n_lumped > 0) paste0(". Smaller groups are combined into ‘Other’") else "", "." )),
      x = if (nzchar(x_label)) x_label else (metric_name),
      y = if (nzchar(y_label)) y_label else (NULL),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_calendar_heatmap() — 3.14 calendar heatmap. Collapses to one value per day,
# fills missing days, lays them on a month grid. Returns the FULLY-faceted plot
# (single year = one row of months; multi-year = a row per year). The wrapper
# sizes fig.height from n_distinct(p$data$Year).
# ------------------------------------------------------------------------------
viz_calendar_heatmap <- function(data,
                                 date_var, value_var, aggregate, metric_name,
                                 chart_title = "", chart_subtitle = "", source_note = "") {

  # 1. Parse dates robustly: "2025-11-01 04:36:41", ISO timestamps, plain dates
  dated <- data %>%
    drop_na(all_of(date_var)) %>%
    mutate(Date = cerp_parse_date(.data[[date_var]])) %>%
    drop_na(Date)

  # 2. Collapse to exactly one value per day, per the aggregate parameter
  daily <- switch(aggregate,
    "count" = dated %>% count(Date, name = "Value"),
    "mean"  = dated %>%
      mutate(V = cerp_numeric(.data[[value_var]], value_var)) %>%
      group_by(Date) %>% summarize(Value = mean(V, na.rm = TRUE), .groups = "drop"),
    "sum"   = dated %>%
      mutate(V = cerp_numeric(.data[[value_var]], value_var)) %>%
      group_by(Date) %>% summarize(Value = sum(V, na.rm = TRUE), .groups = "drop"),
    "none"  = {
      if (anyDuplicated(dated$Date) > 0) stop(
        "Multiple rows share the same date, but aggregate is 'none'. ",
        "Set the 'aggregate' parameter to 'count' (rows per day), ",
        "'mean', or 'sum' so the template knows how to combine them."
      )
      dated %>% transmute(Date, Value = cerp_numeric(.data[[value_var]],
                                                     value_var))
    },
    stop("Unknown aggregate option: '", aggregate,
         "'. Use 'none', 'count', 'mean', or 'sum'.")
  )

  # 3. Missing Day Protection + calendar grid math
  plot_data <- daily %>%
    complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
    mutate(
      Year = year(Date),
      Month = month(Date, label = TRUE, abbr = FALSE),
      DayOfWeek = wday(Date, label = TRUE, abbr = TRUE, week_start = 1),
      first_day_of_month = floor_date(Date, "month"),
      first_day_wday = wday(first_day_of_month, week_start = 1),
      WeekOfMonth = ceiling((day(Date) + first_day_wday - 1) / 7),
      DayOfWeek = fct_rev(DayOfWeek)
    )

  n_years <- n_distinct(plot_data$Year)

  p <- ggplot(plot_data, aes(x = WeekOfMonth, y = DayOfWeek, fill = Value)) +

    geom_tile(color = cerp_cols[["bg"]], linewidth = 0.6) +

    coord_equal() +

    # Sequential ramp: box tint (low) -> primary (high); missing days stay faint
    scale_fill_gradient(
      low = unname(cerp_cols["box"]),
      high = unname(cerp_cols["primary"]),
      na.value = "#f1ede4",
      name = metric_name
    ) +

    theme_cerp() +

    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.key.width = unit(1.8, "cm"),
      legend.key.height = unit(0.35, "cm"),
      legend.title = element_text(family = cerp_font_caption, size = 10,
                                  color = cerp_cols[["subtle"]]),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0.25, "lines"),
      strip.text = element_text(size = 11)
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else (paste("High-Frequency Engagement:", metric_name)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else ("One square per day. Darker means higher; pale squares are days with no data."),
      caption = source_note
    )

  # Single year: months in one row. Multiple years: one row of months per year.
  if (n_years > 1) {
    p + facet_grid(Year ~ Month)
  } else {
    p + facet_wrap(~ Month, nrow = 1)
  }
}

# ------------------------------------------------------------------------------
# viz_choropleth() — 3.15 district choropleth. Harmonizes messy district names to
# the geometry (fail-loud), attaches the metric BY MATCH (never left_join, which
# can silently strip the sf class), quantile-bins, and draws with geom_sf. `geo`
# (sf) and `lookup` (crosswalk) are loaded/validated by the wrapper and passed
# in; requires sf attached by the wrapper.
# ------------------------------------------------------------------------------
viz_choropleth <- function(data, geo, lookup,
                           region_var, value_var, geo_key, n_bins, metric_name,
                           chart_title = "", chart_subtitle = "", source_note = "") {

  # Resolve messy data names -> canonical geometry names. cerp_harmonize() fails
  # LOUDLY (with closest-match suggestions) on any district it cannot place, so a
  # map is never silently drawn with a district missing.
  canonical <- as.character(geo[[geo_key]])
  prepped <- data %>%
    drop_na(all_of(c(region_var, value_var))) %>%
    mutate(
      Canon = cerp_harmonize(.data[[region_var]], canonical, lookup),
      Value = cerp_numeric(.data[[value_var]], value_var)
    ) %>%
    # One value per district (mean guards against accidental duplicate rows).
    group_by(Canon) %>%
    summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop")

  # Attach the metric onto the geometry BY MATCH, not left_join(): a dplyr join
  # can silently strip the sf class (dropping the drawable geometry) so the
  # legend renders but the polygons come out blank. match() writes a plain
  # column and never demotes the sf object. Unmatched districts stay NA -> grey.
  map_data <- geo
  map_data$Value <- prepped$Value[
    match(as.character(map_data[[geo_key]]), prepped$Canon)]
  map_data <- sf::st_as_sf(map_data)   # belt-and-suspenders: guarantee sf class

  # Quantile classing into readable bins (design ramp applied in the plot).
  vals <- map_data$Value[!is.na(map_data$Value)]
  n_bins <- max(2, min(n_bins, length(unique(vals))))
  breaks <- unique(quantile(vals, probs = seq(0, 1, length.out = n_bins + 1),
                            na.rm = TRUE, type = 7))
  if (length(breaks) < 3) {                     # near-constant data: single class
    map_data$Bin <- factor(ifelse(is.na(map_data$Value), NA,
                                  sprintf("%.0f", map_data$Value)))
  } else {
    labs_bins <- paste0(round(head(breaks, -1)), "–", round(breaks[-1]))
    map_data$Bin <- cut(map_data$Value, breaks = breaks, labels = labs_bins,
                        include.lowest = TRUE)
  }
  n_missing <- sum(is.na(map_data$Value))

  # Sequential ramp: pale box tint (low) -> steel blue (high), Design.md palette.
  n_lev <- nlevels(map_data$Bin)
  ramp <- colorRampPalette(c(cerp_cols[["box"]], cerp_cols[["primary"]]))(max(1, n_lev))

  sub_default <- paste0(
    "Darker shading means higher ", tolower(metric_name), ".",
    if (n_missing > 0) paste0(" Grey districts have no data (", n_missing, ").") else ""
  )

  ggplot(map_data) +

    geom_sf(aes(fill = Bin), color = cerp_cols[["bg"]], linewidth = 0.25) +

    scale_fill_manual(
      values = ramp,
      na.value = "#e6e2d9",         # warm grey = "no data"
      name = metric_name,
      drop = FALSE,
      guide = guide_legend(direction = "horizontal", nrow = 1,
                           label.position = "bottom", keywidth = unit(1.4, "cm"))
    ) +

    coord_sf(expand = FALSE) +

    theme_cerp() +

    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(family = cerp_font_caption, size = 10,
                                  color = cerp_cols[["subtle"]]),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else paste("District Map:", metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else sub_default,
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_event_study() — 3.16 event-study DiD. Relative-time dummies × treatment,
# unit + period FE, SEs clustered by unit (fixest); the reference period is pinned
# to 0. Requires fixest attached by the wrapper. `ref_val` is pulled into a plain
# scalar because fixest's formula parser mis-reads an inline params$ref_period.
# ------------------------------------------------------------------------------
viz_event_study <- function(data,
                            unit_var, time_var, event_time_var, treat_var, outcome_var,
                            ref_period, metric_name,
                            chart_title = "", chart_subtitle = "",
                            x_label = "", y_label = "", source_note = "") {

  model_df <- data %>%
    drop_na(all_of(c(unit_var, time_var, event_time_var,
                     treat_var, outcome_var))) %>%
    mutate(
      ev_unit  = as.factor(.data[[unit_var]]),
      ev_time  = as.factor(.data[[time_var]]),
      ev_et    = as.integer(round(cerp_numeric(.data[[event_time_var]],
                                               event_time_var))),
      ev_y     = cerp_numeric(.data[[outcome_var]], outcome_var),
      # Map a messy treatment flag to a strict 1/0 indicator
      ev_treat = if_else(
        str_detect(str_to_lower(str_trim(as.character(.data[[treat_var]]))),
                   "^(1|treat|interven|yes|received|true|t)$|treat|interven"),
        1L, 0L)
    ) %>%
    drop_na(ev_et, ev_y)

  if (!(ref_period %in% unique(model_df$ev_et))) {
    stop("ref_period = ", ref_period,
         " is not present in '", event_time_var, "'. Available event times: ",
         paste(sort(unique(model_df$ev_et)), collapse = ", "), call. = FALSE)
  }

  # Event-study DiD: relative-time dummies interacted with treatment, unit &
  # period fixed effects, SEs clustered by unit. ref_period is omitted (= 0).
  # NOTE: i()'s ref must be a plain scalar in scope — fixest's formula parser
  # mis-reads an inline `params$ref_period` as two data variables.
  ref_val <- as.integer(ref_period)
  model <- fixest::feols(
    ev_y ~ i(ev_et, ev_treat, ref = ref_val) | ev_unit + ev_time,
    data = model_df, cluster = ~ev_unit
  )

  # Tidy the interaction coefficients into an event-time table. coeftable columns
  # are positional (Estimate, Std. Error, ...) — referenced by index so a renamed
  # header can't silently break the extraction.
  ct <- as.data.frame(fixest::coeftable(model), check.names = FALSE)
  plot_data <- tibble(
      term     = rownames(ct),
      Estimate = ct[[1]],
      SE       = ct[[2]]
    ) %>%
    mutate(EventTime = as.integer(str_extract(term, "(?<=::)-?\\d+"))) %>%
    drop_na(EventTime) %>%
    select(EventTime, Estimate, SE) %>%
    # Pin the reference period at exactly 0 (its coefficient is normalized out)
    bind_rows(tibble(EventTime = ref_period, Estimate = 0, SE = 0)) %>%
    arrange(EventTime) %>%
    mutate(
      Lower  = Estimate - 1.96 * SE,
      Upper  = Estimate + 1.96 * SE,
      IsRef  = EventTime == ref_period
    )

  ggplot(plot_data, aes(x = EventTime, y = Estimate)) +

    # Zero-effect anchor and the program-onset boundary (between -1 and 0)
    geom_hline(yintercept = 0, color = cerp_cols[["text"]],
               linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = -0.5, color = cerp_cols[["accent"]],
               linetype = "dotted", linewidth = 0.7) +

    # Connect the point estimates, then draw CIs and points on top
    geom_line(color = cerp_cols[["neutral"]], linewidth = 0.6) +
    geom_pointrange(aes(ymin = Lower, ymax = Upper, color = IsRef, shape = IsRef),
                    linewidth = 0.9, size = 0.7, fatten = 3.5) +

    scale_color_manual(values = c("FALSE" = unname(cerp_cols["primary"]),
                                  "TRUE"  = unname(cerp_cols["subtle"]))) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +
    scale_x_continuous(breaks = plot_data$EventTime) +

    annotate("text", x = -0.5, y = Inf, label = "  Program start",
             hjust = 0, vjust = 1.4, family = cerp_font_caption, size = 3.2,
             color = cerp_cols[["accent"]]) +

    theme_cerp() +

    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank()
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else paste("Dynamic Impact on", metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else paste0(
        "Estimated effect at each period relative to program start.\n",
        "Flat, near-zero points before the line support a credible comparison; ",
        "the period before start is fixed at 0."),
      x = if (nzchar(x_label)) x_label else "Periods relative to program start (0 = first treated period)",
      y = if (nzchar(y_label)) y_label else paste("Effect on", metric_name),
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_small_multiples() — 3.17 small multiples. One panel per facet level with a
# faint "all groups" ghost layer behind each for context. The wrapper sizes
# fig.height from n_distinct(p$data$Facet) / ceiling(sqrt(.)).
# ------------------------------------------------------------------------------
viz_small_multiples <- function(data,
                                facet_var, x_var, y_var, scale_mode, metric_name,
                                chart_title = "", chart_subtitle = "",
                                x_label = "", y_label = "", source_note = "") {

  plot_data <- data %>%
    drop_na(all_of(c(facet_var, x_var, y_var))) %>%
    mutate(
      Facet = as.character(.data[[facet_var]]),
      X = cerp_numeric(.data[[x_var]], x_var),
      Y = cerp_numeric(.data[[y_var]], y_var)
    ) %>%
    drop_na(X, Y) %>%
    arrange(Facet, X)

  # Faint "all groups" context layer drawn behind every panel: same data with the
  # facet column dropped so it is replicated across all facets.
  ghost <- plot_data %>% transmute(X, Y, gid = Facet)

  n_facets <- n_distinct(plot_data$Facet)
  n_col <- ceiling(sqrt(n_facets))
  facet_scales <- if (identical(scale_mode, "free")) "free_y" else "fixed"

  ggplot(plot_data, aes(x = X, y = Y)) +

    # Context: every group's trend, faint, in each panel
    geom_line(data = ghost, aes(group = gid), color = cerp_cols[["neutral"]],
              alpha = 0.35, linewidth = 0.5) +

    # Foreground: this panel's own series, highlighted
    geom_line(color = cerp_cols[["primary"]], linewidth = 1) +
    geom_point(color = cerp_cols[["primary"]], size = 1.6) +

    facet_wrap(~ Facet, scales = facet_scales, ncol = n_col) +

    scale_x_continuous(breaks = scales::breaks_pretty(n = 4)) +

    theme_cerp() +

    theme(
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(0.9, "lines")
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else paste("Group Comparison:", metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else paste0(
        "One panel per ", facet_var,
        "; the highlighted line is that group, faint lines are all groups for context."),
      x = if (nzchar(x_label)) x_label else str_to_title(x_var),
      y = if (nzchar(y_label)) y_label else metric_name,
      caption = source_note
    )
}

# ------------------------------------------------------------------------------
# viz_heatmap_matrix() — 3.18 row × column heatmap. Collapses to one value per
# cell, fills the full grid (missing = grey), orders columns numerically and rows
# by mean, with optional contrast-aware in-cell labels. The wrapper sizes
# fig.height from nlevels(p$data$RowCat).
# ------------------------------------------------------------------------------
viz_heatmap_matrix <- function(data,
                               row_var, col_var, value_var, aggregate, show_values,
                               metric_name,
                               chart_title = "", chart_subtitle = "",
                               x_label = "", y_label = "", source_note = "") {

  base <- data %>%
    drop_na(all_of(c(row_var, col_var))) %>%
    mutate(RowCat = as.character(.data[[row_var]]),
           ColCat = as.character(.data[[col_var]]))

  # Collapse to exactly one value per (row, col) cell
  cell <- switch(aggregate,
    "count" = base %>% count(RowCat, ColCat, name = "Value"),
    "mean"  = base %>%
      mutate(V = cerp_numeric(.data[[value_var]], value_var)) %>%
      group_by(RowCat, ColCat) %>% summarize(Value = mean(V, na.rm = TRUE), .groups = "drop"),
    "sum"   = base %>%
      mutate(V = cerp_numeric(.data[[value_var]], value_var)) %>%
      group_by(RowCat, ColCat) %>% summarize(Value = sum(V, na.rm = TRUE), .groups = "drop"),
    "none"  = {
      if (anyDuplicated(base[c("RowCat", "ColCat")]) > 0) stop(
        "Multiple rows share the same (", row_var, ", ", col_var,
        ") cell, but aggregate is 'none'. Set 'aggregate' to 'count', 'mean', ",
        "or 'sum' so the template knows how to combine them.", call. = FALSE)
      base %>% transmute(RowCat, ColCat,
                         Value = cerp_numeric(.data[[value_var]], value_var))
    },
    stop("Unknown aggregate option: '", aggregate,
         "'. Use 'none', 'count', 'mean', or 'sum'.", call. = FALSE)
  )

  # Fill the full grid so empty cells appear (grey) rather than vanish
  plot_data <- cell %>% complete(RowCat, ColCat)

  # Order columns numerically when they are numbers (e.g. years), else keep as-is;
  # order rows by their mean value so the strongest categories sit together.
  col_levels <- unique(plot_data$ColCat)
  if (!any(is.na(suppressWarnings(as.numeric(col_levels))))) {
    col_levels <- col_levels[order(as.numeric(col_levels))]
  }
  row_order <- plot_data %>% group_by(RowCat) %>%
    summarize(m = mean(Value, na.rm = TRUE), .groups = "drop") %>% arrange(m) %>% pull(RowCat)

  plot_data <- plot_data %>%
    mutate(ColCat = factor(ColCat, levels = col_levels),
           RowCat = factor(RowCat, levels = row_order))

  val_digits <- if (aggregate == "mean") 1 else 0
  mid_val <- mean(range(plot_data$Value, na.rm = TRUE))

  p <- ggplot(plot_data, aes(x = ColCat, y = RowCat, fill = Value)) +

    geom_tile(color = cerp_cols[["bg"]], linewidth = 0.8) +

    # Sequential ramp: pale box tint (low) -> steel blue (high); empty cells grey
    scale_fill_gradient(
      low = unname(cerp_cols["box"]),
      high = unname(cerp_cols["primary"]),
      na.value = "#e6e2d9",
      name = metric_name
    ) +

    theme_cerp() +

    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.key.width = unit(1.8, "cm"),
      legend.key.height = unit(0.35, "cm"),
      legend.title = element_text(family = cerp_font_caption, size = 10,
                                  color = cerp_cols[["subtle"]]),
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold", color = cerp_cols[["text"]])
    ) +

    labs(
      title = if (nzchar(chart_title)) chart_title else paste("Category Matrix:", metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else "Each cell is shaded by value; darker means higher. Grey cells have no data.",
      x = if (nzchar(x_label)) x_label else str_to_title(col_var),
      y = if (nzchar(y_label)) y_label else str_to_title(row_var),
      caption = source_note
    )

  # Optional in-cell value labels, with contrast-aware text color
  if (isTRUE(show_values)) {
    p <- p + geom_text(
      aes(label = ifelse(is.na(Value), "",
                         formatC(Value, format = "f", digits = val_digits)),
          color = Value > mid_val),
      family = cerp_font_caption, size = 3.4, show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = cerp_cols[["text"]], "TRUE" = "#FFFFFF"),
                         na.value = cerp_cols[["subtle"]])
  }

  p
}

# ==============================================================================
# PHASE 10 — RCT reporting-narrative templates (3.19–3.23). Born as functions,
# adopting the Phase-4B carry-forward conventions from day one: every numeric
# input goes through cerp_numeric(), and each function opens with a
# cerp_require_rows() usable-row guard so empty / all-NA input fails loudly
# instead of drawing a silently-wrong chart.
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_consort_flow() — 3.19 CONSORT participant-flow diagram. Boxes + connecting
# arrows drawn purely with ggplot geom_rect/segment/text (NO DiagrammeR/grViz).
# One row per stage; an optional arm_var splits the flow into parallel columns
# after randomization, and an optional note_var prints exclusion/attrition notes
# to the right of the stage they belong to.
# ------------------------------------------------------------------------------
viz_consort_flow <- function(data,
                             stage_var, n_var, arm_var = "", note_var = "",
                             metric_name = "Participant Flow",
                             chart_title = "", chart_subtitle = "",
                             source_note = "") {

  has_arm  <- nzchar(arm_var)  && arm_var  %in% names(data)
  has_note <- nzchar(note_var) && note_var %in% names(data)

  df <- data %>%
    mutate(
      Stage = str_squish(as.character(.data[[stage_var]])),
      N     = cerp_numeric(.data[[n_var]], n_var),
      Arm   = if (has_arm)  str_squish(as.character(.data[[arm_var]]))  else "",
      Note  = if (has_note) str_squish(as.character(.data[[note_var]])) else ""
    ) %>%
    mutate(Arm  = ifelse(is.na(Arm),  "", Arm),
           Note = ifelse(is.na(Note), "", Note)) %>%
    filter(!is.na(Stage), nzchar(Stage))

  # Usable-row guard (4B convention): need at least one stage with a count.
  cerp_require_rows(df, c(stage_var, n_var), min_rows = 1, what = metric_name)
  df <- df %>% drop_na(N)

  # Stage order = first appearance in the file (top -> bottom of the flow).
  stage_levels <- unique(df$Stage)
  df <- df %>% mutate(y = -match(Stage, stage_levels))

  # Arm columns: a blank arm sits on the central spine (x = 0); named arms are
  # spread symmetrically left-to-right (a single named arm stays centered).
  arms   <- sort(unique(df$Arm[nzchar(df$Arm)]))
  n_arms <- length(arms)
  arm_x  <- if (n_arms > 1) setNames(seq(-1.7, 1.7, length.out = n_arms), arms)
            else if (n_arms == 1) setNames(0, arms) else numeric(0)
  x_of   <- function(a) if (nzchar(a) && a %in% names(arm_x)) unname(arm_x[[a]]) else 0
  df <- df %>% mutate(x = vapply(Arm, x_of, numeric(1)), .row = row_number())

  # Colour cue by arm: treatment-like arms take the primary hue, everything else
  # the muted neutral; the spine is the pale box tint. Boxes stay uniform fill;
  # the border + the column header carry the colour so the page stays quiet.
  arm_hue <- function(a) if (str_detect(str_to_lower(a), "treat|interven|program|program"))
    unname(cerp_cols["primary"]) else unname(cerp_cols["neutral"])
  border_of <- function(a) if (nzchar(a)) arm_hue(a) else unname(cerp_cols["primary"])

  bw <- 1.55; bh <- 0.42
  boxes <- df %>%
    mutate(
      xmin = x - bw/2, xmax = x + bw/2,
      ymin = y - bh/2, ymax = y + bh/2,
      Border = vapply(Arm, border_of, character(1)),
      Label  = paste0(Stage, "\n(n = ", scales::comma(round(N)), ")")
    )

  # Arrows: for each consecutive stage pair, connect every child to its parent —
  # a same-arm parent if one exists at the level above, else the spine box (this
  # is what makes randomization branch into the two arm columns).
  seg_list <- list()
  for (i in seq_len(length(stage_levels) - 1)) {
    parents  <- boxes %>% filter(Stage == stage_levels[i])
    children <- boxes %>% filter(Stage == stage_levels[i + 1])
    for (k in seq_len(nrow(children))) {
      ch <- children[k, ]
      par <- parents %>% filter(Arm == ch$Arm)
      if (nrow(par) == 0) par <- parents %>% filter(!nzchar(Arm))
      if (nrow(par) == 0) par <- parents[1, ]
      if (nrow(par) == 0) next
      par <- par[1, ]
      seg_list[[length(seg_list) + 1]] <- data.frame(
        x = par$x, xend = ch$x, y = par$ymin, yend = ch$ymax)
    }
  }
  segs <- if (length(seg_list)) do.call(rbind, seg_list) else
    data.frame(x = numeric(0), xend = numeric(0), y = numeric(0), yend = numeric(0))

  # Notes: a boxed callout on the far right, wired to its stage with an elbow.
  x_note <- max(boxes$xmax) + 0.55
  notes <- boxes %>% filter(nzchar(Note)) %>%
    mutate(nx = x_note, Note_wrapped = str_wrap(Note, width = 34))

  # Arm column headers, above the first split stage.
  headers <- if (n_arms > 0) {
    tibble(Arm = arms, x = unname(arm_x[arms]), y = max(boxes$y) + 0.75,
           hue = vapply(arms, arm_hue, character(1)))
  } else NULL

  x_min_lim <- min(boxes$xmin) - 0.3
  x_max_lim <- if (nrow(notes) > 0) x_note + 2.4 else max(boxes$xmax) + 0.3

  p <- ggplot() +
    # connectors first, arrowheads pointing into each child box
    geom_segment(data = segs, aes(x = x, xend = xend, y = y, yend = yend),
                 color = cerp_cols[["neutral"]], linewidth = 0.6,
                 arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
    # note connectors (horizontal elbow to the right rail)
    { if (nrow(notes) > 0) geom_segment(
        data = notes, aes(x = xmax, xend = nx - 0.02, y = y, yend = y),
        color = cerp_cols[["accent"]], linewidth = 0.4, linetype = "dotted") } +
    # stage boxes
    geom_rect(data = boxes,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = Border),
              fill = cerp_cols[["box"]], linewidth = 0.7) +
    geom_text(data = boxes, aes(x = x, y = y, label = Label),
              family = cerp_font_caption, size = 3.3, lineheight = 0.95,
              color = cerp_cols[["text"]], fontface = "bold") +
    # note callouts
    { if (nrow(notes) > 0) geom_label(
        data = notes, aes(x = nx, y = y, label = Note_wrapped),
        hjust = 0, family = cerp_font_caption, size = 2.8, lineheight = 0.95,
        color = cerp_cols[["text"]], fill = "#fbeee9",
        label.size = 0, label.padding = unit(0.28, "lines")) } +
    scale_color_identity() +
    coord_cartesian(xlim = c(x_min_lim, x_max_lim), clip = "off") +
    theme_cerp() +
    theme(
      axis.text = element_blank(), axis.title = element_blank(),
      panel.grid = element_blank(), axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    labs(
      title    = if (nzchar(chart_title)) chart_title else metric_name,
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else
        "Flow of participants through each stage of the trial. Counts (n) shown per box; notes list exclusions and losses.",
      caption  = source_note
    )

  # Column headers drawn on top (after theme so colour is explicit).
  if (!is.null(headers)) {
    p <- p + geom_text(data = headers, aes(x = x, y = y, label = Arm, color = hue),
                       family = cerp_font_caption, fontface = "bold", size = 3.7) +
      scale_color_identity()
  }
  p
}

# ------------------------------------------------------------------------------
# viz_balance_plot() — 3.20 covariate-balance "love plot". Standardized mean
# difference (treatment − control) per covariate, one point each, sorted by
# magnitude, with |SMD| = threshold reference lines. SMD is computed in-code (no
# cobalt): numeric covariates use (mean_T − mean_C) / pooled SD; categorical
# covariates are expanded to per-level indicators using the raw difference in
# proportions (the first level is the dropped reference).
# ------------------------------------------------------------------------------
viz_balance_plot <- function(data,
                             treat_var, balance_vars, threshold = 0.1,
                             metric_name = "Covariate Balance",
                             chart_title = "", chart_subtitle = "",
                             x_label = "", y_label = "", source_note = "") {

  balance_vars <- unlist(balance_vars, use.names = FALSE)
  balance_vars <- balance_vars[!is.na(balance_vars) & nzchar(balance_vars)]

  d <- data %>%
    mutate(.trt = if_else(
      str_detect(str_to_lower(str_trim(as.character(.data[[treat_var]]))),
                 "^(1|treat|interven|yes|received|true|t)$|treat|interven"),
      1L, 0L)) %>%
    filter(!is.na(.trt))

  # Guard: need usable rows and BOTH arms (else an SMD is undefined).
  cerp_require_rows(d, treat_var, min_rows = 2, what = metric_name)
  if (length(unique(d$.trt)) < 2) {
    stop("Balance plot needs BOTH a treatment and a control group in '",
         treat_var, "', but only one was found after coercion.", call. = FALSE)
  }

  # Per-covariate SMD (numeric) or per-level proportion difference (categorical).
  smd_rows <- function(var) {
    x  <- d[[var]]
    tv <- d$.trt
    keep <- !is.na(x)
    x <- x[keep]; tv <- tv[keep]
    if (length(x) == 0) return(NULL)

    # Probe the type SILENTLY (a categorical covariate would otherwise trigger a
    # spurious all-NA coercion warning); once we commit to numeric, re-coerce
    # VISIBLY so genuine numbers-as-text loss warns (4B warn-only convention).
    parsed  <- suppressWarnings(cerp_numeric(x, var))
    is_num  <- is.numeric(x) || mean(!is.na(parsed)) >= 0.8
    n_dist  <- length(unique(x))

    if (is_num && n_dist > 2) {
      parsed <- cerp_numeric(x, var)
      xt <- parsed[tv == 1]; xc <- parsed[tv == 0]
      pooled <- sqrt((stats::sd(xt, na.rm = TRUE)^2 + stats::sd(xc, na.rm = TRUE)^2) / 2)
      smd <- if (is.na(pooled) || pooled == 0) 0 else
        (mean(xt, na.rm = TRUE) - mean(xc, na.rm = TRUE)) / pooled
      tibble(term = var, smd = smd)
    } else {
      lv <- levels(factor(x))
      lv <- lv[-1]                                   # drop first level = reference
      if (length(lv) == 0) return(NULL)
      map_dfr(lv, function(l) {
        pt <- mean(x[tv == 1] == l); pc <- mean(x[tv == 0] == l)
        tibble(term = paste0(var, ": ", l), smd = pt - pc)
      })
    }
  }

  plot_data <- map_dfr(balance_vars, smd_rows) %>%
    drop_na(smd) %>%
    mutate(
      Imbalanced = ifelse(abs(smd) > threshold, "Above threshold", "Balanced"),
      term = fct_reorder(term, abs(smd))
    )

  if (nrow(plot_data) == 0) {
    stop("No covariates in balance_vars produced a usable SMD — check the ",
         "column names and that they contain non-missing values.", call. = FALSE)
  }

  n_flag <- sum(plot_data$Imbalanced == "Above threshold")

  ggplot(plot_data, aes(x = smd, y = term)) +
    geom_vline(xintercept = 0, color = cerp_cols[["text"]], linewidth = 0.8) +
    geom_vline(xintercept = c(-threshold, threshold), color = cerp_cols[["accent"]],
               linetype = "dashed", linewidth = 0.6) +
    geom_segment(aes(x = 0, xend = smd, yend = term),
                 color = cerp_cols[["grid"]], linewidth = 0.5) +
    geom_point(aes(color = Imbalanced), size = 4) +
    scale_color_manual(values = c(
      "Above threshold" = unname(cerp_cols["accent"]),
      "Balanced"        = unname(cerp_cols["primary"]))) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    theme_cerp() +
    theme(
      legend.position = "top",
      axis.text.y = element_text(size = 11, face = "bold", color = cerp_cols[["text"]]),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      title    = if (nzchar(chart_title)) chart_title else paste0(metric_name, ": Treatment vs Control"),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else paste0(
        "Standardized mean difference per covariate. Dashed lines mark |SMD| = ",
        threshold, "; ", n_flag, " covariate(s) exceed it."),
      x = if (nzchar(x_label)) x_label else "Standardized mean difference (Treatment − Control)",
      y = if (nzchar(y_label)) y_label else NULL,
      caption  = source_note
    )
}

# ------------------------------------------------------------------------------
# cerp_gt_theme() — house style for gt tables (Design.md palette + fonts), kept
# next to the viz functions so any future gt table reuses one styling surface.
# ------------------------------------------------------------------------------
cerp_gt_theme <- function(gt_tbl) {
  gt_tbl %>%
    gt::opt_table_font(font = c(cerp_font_body, "Georgia", "serif")) %>%
    gt::tab_options(
      table.background.color        = cerp_cols[["bg"]],
      table.font.color              = cerp_cols[["text"]],
      table.border.top.style        = "none",
      table.border.bottom.color     = cerp_cols[["text"]],
      heading.title.font.size       = gt::px(20),
      heading.title.font.weight     = "bold",
      heading.subtitle.font.size    = gt::px(13),
      heading.align                 = "left",
      column_labels.font.weight     = "bold",
      column_labels.background.color = cerp_cols[["box"]],
      column_labels.border.top.style = "none",
      column_labels.border.bottom.color = cerp_cols[["text"]],
      row_group.font.weight         = "bold",
      row_group.background.color    = cerp_cols[["bg"]],
      table_body.hlines.color       = cerp_cols[["grid"]],
      table.font.size               = gt::px(13),
      source_notes.font.size        = gt::px(10),
      data_row.padding              = gt::px(5)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = cerp_cols[["subtle"]]),
      locations = gt::cells_row_groups()
    )
}

# ------------------------------------------------------------------------------
# viz_summary_table() — 3.21 Table-1-style grouped summary via gt (RETURNS a gt
# object, not a ggplot). Numeric summary vars -> mean (sd); categorical -> n (%)
# per level. Columns are the groups plus an Overall column, each headed with its
# N. Requires gt (attached/guarded by the wrapper); referenced namespaced here.
# ------------------------------------------------------------------------------
viz_summary_table <- function(data,
                              group_var, summary_vars, digits = 1,
                              metric_name = "Sample Summary",
                              chart_title = "", chart_subtitle = "",
                              source_note = "") {

  summary_vars <- unlist(summary_vars, use.names = FALSE)
  summary_vars <- summary_vars[!is.na(summary_vars) & nzchar(summary_vars)]

  d <- data %>%
    mutate(.grp = str_squish(as.character(.data[[group_var]]))) %>%
    filter(!is.na(.grp), nzchar(.grp))

  cerp_require_rows(d, group_var, min_rows = 1, what = metric_name)

  groups   <- sort(unique(d$.grp))
  grp_cols <- c(groups, "Overall")
  n_by_grp <- c(vapply(groups, function(g) sum(d$.grp == g), integer(1)), nrow(d))
  names(n_by_grp) <- grp_cols

  num_fmt <- function(v) formatC(v, format = "f", digits = digits)
  # SD is undefined for a group of n = 1 (sd() -> NA); render it as an em dash
  # rather than a bare "NA" in the table cell.
  fmt_sd  <- function(v) if (is.na(v)) "—" else num_fmt(v)

  # One tidy long block per summary variable: numeric = single "mean (sd)" row;
  # categorical = one "n (%)" row per level, under a variable row group.
  block_for <- function(var) {
    x <- d[[var]]
    # Probe type SILENTLY (a categorical column would otherwise warn on all-NA
    # coercion); re-coerce VISIBLY once numeric so real numbers-as-text loss
    # warns (4B warn-only convention).
    parsed <- suppressWarnings(cerp_numeric(x, var))
    is_num <- is.numeric(x) || mean(!is.na(parsed)) >= 0.8
    n_dist <- length(unique(x[!is.na(x)]))

    if (is_num && n_dist > 2) {
      parsed <- cerp_numeric(x, var)
      vals <- vapply(grp_cols, function(g) {
        idx <- if (g == "Overall") rep(TRUE, nrow(d)) else d$.grp == g
        xv <- parsed[idx]
        paste0(num_fmt(mean(xv, na.rm = TRUE)), " (", fmt_sd(stats::sd(xv, na.rm = TRUE)), ")")
      }, character(1))
      out <- tibble(group_label = paste0(var, ", mean (SD)"), label = "")
      for (g in grp_cols) out[[g]] <- vals[[g]]
      out
    } else {
      lv <- levels(factor(x))
      map_dfr(lv, function(l) {
        vals <- vapply(grp_cols, function(g) {
          idx <- if (g == "Overall") rep(TRUE, nrow(d)) else d$.grp == g
          xv <- x[idx]; denom <- sum(!is.na(xv))
          cnt <- sum(xv == l, na.rm = TRUE)
          pct <- if (denom > 0) 100 * cnt / denom else 0
          paste0(scales::comma(cnt), " (", num_fmt(pct), "%)")
        }, character(1))
        row <- tibble(group_label = paste0(var, ", n (%)"), label = l)
        for (g in grp_cols) row[[g]] <- vals[[g]]
        row
      })
    }
  }

  tbl <- map_dfr(summary_vars, block_for)

  col_labels <- setNames(
    lapply(grp_cols, function(g) gt::md(paste0("**", g, "**<br>N = ", scales::comma(n_by_grp[[g]])))),
    grp_cols)

  gt_out <- tbl %>%
    gt::gt(groupname_col = "group_label", rowname_col = "label") %>%
    gt::tab_header(
      title    = if (nzchar(chart_title)) chart_title else metric_name,
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else
        paste0("Grouped by ", group_var, ". Numeric variables show mean (SD); categorical variables show n (%).")
    ) %>%
    gt::cols_label(.list = col_labels) %>%
    gt::cols_align(align = "right", columns = tidyselect::all_of(grp_cols)) %>%
    gt::cols_align(align = "left", columns = "label")

  if (nzchar(source_note)) gt_out <- gt_out %>% gt::tab_source_note(source_note)

  cerp_gt_theme(gt_out)
}

# ------------------------------------------------------------------------------
# viz_scatter_quadrant() — 3.22 labeled scatter split into four quadrants by a
# vertical x-split and a horizontal y-split (default: the median of each axis).
# Points are coloured by quadrant (muted house palette) and labeled with ggrepel;
# optional corner captions name each quadrant. Requires ggrepel (attached by the
# wrapper); referenced namespaced here.
# ------------------------------------------------------------------------------
viz_scatter_quadrant <- function(data,
                                 x_var, y_var, label_var,
                                 x_split = "median", y_split = "median",
                                 quadrant_label_tr = "", quadrant_label_tl = "",
                                 quadrant_label_br = "", quadrant_label_bl = "",
                                 metric_name = "",
                                 chart_title = "", chart_subtitle = "",
                                 x_label = "", y_label = "", source_note = "") {

  what <- if (nzchar(metric_name)) metric_name else "this scatter"

  # Usable-row guard (4B convention): guard on the ORIGINAL params (not the
  # internal X/Y) so the error names real columns and reads clearly.
  cerp_require_rows(data, c(x_var, y_var), min_rows = 1, what = what)

  d <- data %>%
    mutate(
      X     = cerp_numeric(.data[[x_var]], x_var),
      Y     = cerp_numeric(.data[[y_var]], y_var),
      Label = str_squish(as.character(.data[[label_var]]))
    ) %>%
    drop_na(X, Y)

  # Second guard for the all-unparseable case (numbers-as-text that all coerce to
  # NA): fail loudly naming the axis columns rather than drawing an empty panel.
  if (nrow(d) == 0) {
    stop(what, " has no rows with usable numeric '", x_var, "' and '", y_var,
         "' after coercion. Check those columns in the data file.", call. = FALSE)
  }

  # Resolve each split: "median" (default), "mean", or a fixed numeric value.
  resolve_split <- function(s, v) {
    key <- tolower(str_trim(as.character(s)))
    if (length(key) == 0 || key %in% c("", "median")) stats::median(v, na.rm = TRUE)
    else if (key == "mean") mean(v, na.rm = TRUE)
    else {
      num <- suppressWarnings(as.numeric(s))
      if (is.na(num)) stats::median(v, na.rm = TRUE) else num
    }
  }
  xs <- resolve_split(x_split, d$X)
  ys <- resolve_split(y_split, d$Y)

  d <- d %>% mutate(
    Quadrant = case_when(
      X >= xs & Y >= ys ~ "tr",
      X <  xs & Y >= ys ~ "tl",
      X >= xs & Y <  ys ~ "br",
      TRUE              ~ "bl"
    )
  )

  # Muted house palette, one per quadrant (Design.md).
  quad_cols <- c(tr = unname(cerp_cols["primary"]),
                 tl = unname(cerp_cols["secondary"]),
                 br = unname(cerp_cols["accent"]),
                 bl = unname(cerp_cols["neutral"]))

  # Corner captions, drawn only where a label was supplied.
  x_lo <- min(d$X); x_hi <- max(d$X); y_lo <- min(d$Y); y_hi <- max(d$Y)
  corners <- tibble::tribble(
    ~key,  ~label,             ~x,   ~y,   ~hjust, ~vjust,
    "tr",  quadrant_label_tr,  x_hi, y_hi,  1,      1,
    "tl",  quadrant_label_tl,  x_lo, y_hi,  0,      1,
    "br",  quadrant_label_br,  x_hi, y_lo,  1,      0,
    "bl",  quadrant_label_bl,  x_lo, y_lo,  0,      0
  ) %>% filter(nzchar(label))

  p <- ggplot(d, aes(x = X, y = Y)) +
    geom_vline(xintercept = xs, color = cerp_cols[["text"]],
               linetype = "dashed", linewidth = 0.6) +
    geom_hline(yintercept = ys, color = cerp_cols[["text"]],
               linetype = "dashed", linewidth = 0.6) +
    geom_point(aes(color = Quadrant), size = 3.4, alpha = 0.9) +
    ggrepel::geom_text_repel(
      aes(label = Label), size = 3.2, family = cerp_font_caption,
      color = cerp_cols[["text"]], max.overlaps = 20,
      box.padding = 0.3, point.padding = 0.2,
      segment.color = "#b7c9d3", segment.size = 0.3, seed = 26) +
    scale_color_manual(values = quad_cols, guide = "none") +
    theme_cerp() +
    labs(
      title    = if (nzchar(chart_title)) chart_title else
        paste0(str_to_title(y_var), " vs ", str_to_title(x_var)),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else
        "Dashed lines split the field at each axis; points are colored by quadrant.",
      x = if (nzchar(x_label)) x_label else str_to_title(x_var),
      y = if (nzchar(y_label)) y_label else str_to_title(y_var),
      caption  = source_note
    )

  if (nrow(corners) > 0) {
    p <- p + geom_label(
      data = corners,
      aes(x = x, y = y, label = str_wrap(label, 22), hjust = hjust, vjust = vjust),
      inherit.aes = FALSE, family = cerp_font_caption, fontface = "bold",
      size = 3.1, color = cerp_cols[["subtle"]], fill = cerp_cols[["box"]],
      label.size = 0, alpha = 0.85)
  }
  p
}

# ------------------------------------------------------------------------------
# viz_survival_curve() — 3.23 Kaplan-Meier time-to-event by group. Fits with
# survival::survfit, tidies the fit manually into a step table (with a t0 = 1.0
# anchor per group), and draws step curves with optional stepped CI ribbons.
# `event_var` is coerced to 1 (event) / 0 (censored) via cerp_numeric with a
# text fallback. Requires survival (attached/guarded by the wrapper).
# ------------------------------------------------------------------------------
viz_survival_curve <- function(data,
                               time_var, event_var, group_var, show_ci = TRUE,
                               metric_name = "Retention",
                               chart_title = "", chart_subtitle = "",
                               x_label = "", y_label = "", source_note = "") {

  ev_num <- suppressWarnings(cerp_numeric(data[[event_var]], event_var))
  d <- data %>%
    mutate(
      Time  = cerp_numeric(.data[[time_var]], time_var),
      Event = case_when(
        !is.na(ev_num) ~ as.integer(ev_num >= 1),
        str_detect(str_to_lower(as.character(.data[[event_var]])),
                   "yes|event|true|drop|died|left|1") ~ 1L,
        TRUE ~ 0L),
      Group = str_squish(as.character(.data[[group_var]]))
    ) %>%
    filter(!is.na(Time), !is.na(Group), nzchar(Group))

  # Usable-row guard (4B convention).
  cerp_require_rows(d, c(time_var, group_var), min_rows = 1, what = metric_name)
  d <- d %>% drop_na(Time, Event)

  # Kaplan-Meier by group; tidy the fit manually (survfit strata are positional).
  fit <- survival::survfit(survival::Surv(Time, Event) ~ Group, data = d)
  strata_lab <- sub("^Group=", "", rep(names(fit$strata), fit$strata))
  km <- tibble(
    time  = fit$time,
    surv  = fit$surv,
    lower = if (!is.null(fit$lower)) fit$lower else NA_real_,
    upper = if (!is.null(fit$upper)) fit$upper else NA_real_,
    Group = strata_lab
  )
  # Anchor every curve at (t = 0, S = 1) so the step starts at 100%.
  starts <- km %>% distinct(Group) %>%
    mutate(time = 0, surv = 1, lower = 1, upper = 1)
  km <- bind_rows(starts, km) %>% arrange(Group, time)

  n_groups   <- dplyr::n_distinct(km$Group)
  grp_colors <- setNames(cerp_fill_n(n_groups), sort(unique(km$Group)))

  p <- ggplot(km, aes(x = time, y = surv, color = Group))

  # Optional stepped CI ribbon: expand each interval to a flat segment so the
  # ribbon steps with the curve instead of interpolating diagonally.
  if (isTRUE(show_ci) && !all(is.na(km$lower))) {
    ribbon <- km %>% group_by(Group) %>% arrange(time) %>%
      mutate(t2 = lead(time)) %>% ungroup() %>% filter(!is.na(t2)) %>%
      { bind_rows(transmute(., Group, t = time,  lower, upper),
                  transmute(., Group, t = t2,    lower, upper)) } %>%
      arrange(Group, t)
    # Only add the ribbon layer + its fill scale when it actually has rows;
    # otherwise ggplot warns about a manual scale with no matching data
    # (degenerate single-observation input).
    if (nrow(ribbon) > 0) {
      p <- p + geom_ribbon(
        data = ribbon, inherit.aes = FALSE,
        aes(x = t, ymin = lower, ymax = upper, fill = Group),
        alpha = 0.15, color = NA) +
        scale_fill_manual(values = grp_colors, guide = "none")
    }
  }

  p +
    geom_step(linewidth = 1.1) +
    scale_color_manual(values = grp_colors) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
    theme_cerp() +
    labs(
      title    = if (nzchar(chart_title)) chart_title else paste("Time to Event:", metric_name),
      subtitle = if (nzchar(chart_subtitle)) chart_subtitle else
        "Share still event-free over time (Kaplan-Meier). Shaded bands are 95% confidence intervals.",
      x = if (nzchar(x_label)) x_label else str_to_title(time_var),
      y = if (nzchar(y_label)) y_label else paste0("Share event-free"),
      caption  = source_note
    )
}
