# ==============================================================================
# Script Name: 2.5_helpers.R
# Author:      Ibraheem Saqib Ellahi <ibraheemsaqib90@gmail.com>
# Purpose:     Shared data-loading, validation, and parsing utilities used by
#              every template. Design principle: fail LOUDLY and clearly —
#              a template must never render a silently-wrong chart.
# ==============================================================================

# ------------------------------------------------------------------------------
# cerp_require(): attach packages, NEVER installing at render time. renv owns
# the environment (hard rule 5) — a missing package is a loud, fixable error,
# not an excuse to mutate the package library mid-render.
# ------------------------------------------------------------------------------
cerp_require <- function(pkgs) {
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(!ok)) {
    stop(
      "Missing package(s): ", paste(pkgs[!ok], collapse = ", "),
      "\nThis project uses renv. Run renv::restore() from the project root, ",
      "then re-render.",
      call. = FALSE
    )
  }
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}

cerp_require(c("readr", "dplyr", "stringr", "forcats", "lubridate"))

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------------------------
# cerp_load(): read a CSV and scrub the two most common field-data landmines —
# stray whitespace in column headers and in character values.
# ------------------------------------------------------------------------------
cerp_load <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Data file not found: ", path,
      "\nCheck the 'data_path' parameter in the template YAML header.",
      call. = FALSE
    )
  }
  d <- readr::read_csv(path, show_col_types = FALSE, trim_ws = TRUE)
  names(d) <- stringr::str_squish(names(d))
  d %>% mutate(across(where(is.character), stringr::str_squish))
}

# ------------------------------------------------------------------------------
# cerp_validate(): confirm every *_var parameter names a real column.
# On failure: name the missing column, suggest the closest match, and list
# what IS available — so a non-technical user can fix the YAML themselves.
# ------------------------------------------------------------------------------
cerp_validate <- function(data, cols) {
  cols <- unlist(cols, use.names = FALSE)
  cols <- cols[!is.na(cols) & nzchar(cols)]
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    lines <- vapply(missing, function(m) {
      suggestion <- agrep(m, names(data), max.distance = 0.35,
                          value = TRUE, ignore.case = TRUE)
      paste0(
        "  - '", m, "'",
        if (length(suggestion) > 0) {
          paste0("  (did you mean '", suggestion[1], "'?)")
        } else ""
      )
    }, character(1))
    stop(
      "Column(s) named in the YAML header were not found in the data:\n",
      paste(lines, collapse = "\n"),
      "\n\nColumns available in this dataset:\n  ",
      paste(names(data), collapse = " | "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# cerp_parse_date(): robust date parsing. Handles Date/POSIX columns, ISO
# timestamps ("2025-11-01T04:36:41Z"), and common day-first / month-first
# text formats. Returns Date (time-of-day stripped).
# ------------------------------------------------------------------------------
cerp_parse_date <- function(x, quiet = FALSE) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  parsed <- suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "Ymd HMS", "Ymd HM", "Ymd",
      "dmY HMS", "dmY", "mdY HMS", "mdY",
      "dbY", "bdY", "Y"
    )
  ))
  out <- as.Date(parsed)
  n_bad <- sum(is.na(out) & !is.na(x) & nzchar(as.character(x)))
  if (n_bad > 0 && !quiet) {
    warning(n_bad, " value(s) could not be parsed as dates and became NA.",
            call. = FALSE)
  }
  out
}

# ------------------------------------------------------------------------------
# cerp_numeric(): coerce to numeric, surviving "45%", "1,200", "N/A".
# Warns with a count when values are lost so the user knows to inspect.
# ------------------------------------------------------------------------------
cerp_numeric <- function(x, name = "value") {
  if (is.numeric(x)) return(x)
  out <- suppressWarnings(readr::parse_number(as.character(x)))
  n_bad <- sum(is.na(out) & !is.na(x) & nzchar(as.character(x)))
  if (n_bad > 0) {
    warning(n_bad, " '", name,
            "' value(s) could not be converted to numbers and became NA.",
            call. = FALSE)
  }
  out
}

# ------------------------------------------------------------------------------
# cerp_lump(): consolidate a high-cardinality categorical into its n most
# frequent levels + "Other", so axes stay readable (e.g. 20+ facilities).
# ------------------------------------------------------------------------------
cerp_lump <- function(x, n = 8, other = "Other (smaller groups)") {
  x <- forcats::fct_infreq(as.factor(x))
  if (nlevels(x) > n) {
    x <- forcats::fct_lump_n(x, n = n, other_level = other)
  }
  x
}

# ------------------------------------------------------------------------------
# cerp_require_rows(): usable-row guard. Counts rows with non-missing values
# across a template's ESSENTIAL columns and stops loudly if fewer than min_rows
# remain — so empty files and all-NA columns fail with a clear message naming the
# columns, instead of drawing a meaningless chart (silent-wrong, hard rule 4) or
# throwing an opaque downstream error. Call it at the top of every viz_*()
# function on the columns that chart cannot be built without. (Phase 4B
# carry-forward, adopted early by the Phase-10 templates so they are born robust.)
#
#   data     the loaded data.frame.
#   cols     character vector / list of the essential column names.
#   min_rows minimum usable (complete-case over cols) rows required. Default 1.
#   what     short label for the chart, used in the error message.
# ------------------------------------------------------------------------------
cerp_require_rows <- function(data, cols, min_rows = 1, what = "this visualization") {
  cols <- unlist(cols, use.names = FALSE)
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  present <- intersect(cols, names(data))
  n_ok <- if (length(present) == 0) nrow(data) else sum(stats::complete.cases(data[present]))
  if (n_ok < min_rows) {
    stop(
      str_to_sentence(what), " needs at least ", min_rows,
      " row(s) with a value in ",
      if (length(present) > 0) paste0("every one of: ", paste(present, collapse = ", "))
      else "the required columns",
      ", but only ", n_ok, " such row(s) were found.",
      "\nCheck the data file and the column parameters in the YAML header.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# cerp_norm(): canonicalize a place name for matching. Strips whitespace,
# common admin suffixes (District / Tehsil / Division / Capital Territory /
# City), and punctuation, then title-cases. Used on BOTH sides of a spatial
# join so cosmetic differences ("Karachi ", "D.G. Khan", "Lahore District")
# stop blocking matches.
# ------------------------------------------------------------------------------
cerp_norm <- function(s) {
  s <- as.character(s)
  s <- stringr::str_squish(s)
  s <- stringr::str_remove_all(
    s, stringr::regex("\\b(district|tehsil|division|capital territory|city)\\b",
                      ignore_case = TRUE))
  s <- stringr::str_replace_all(s, "[[:punct:]]", " ")
  s <- stringr::str_to_title(stringr::str_squish(s))
  s
}

# ------------------------------------------------------------------------------
# cerp_harmonize(): resolve messy district/region names in the data to the
# canonical names used by the geometry (geojson), so a choropleth can join
# cleanly. Order of resolution for each value:
#   1. normalized direct match to a geometry name;
#   2. else a row in the versioned crosswalk (lookup) whose canonical target
#      IS a geometry name;
#   3. else UNMATCHED -> fail loudly (never silently drop a district).
#
#   x         character vector from the data's region column.
#   canonical character vector of names present in the geometry.
#   lookup    optional data.frame with columns raw_name, canonical_name
#             (the versioned 1_data/geo/district_lookup.csv).
# Returns a character vector, same length/order as x, of matched canonical
# geometry names. On any unmatched value it stops with the offending names,
# the closest geometry candidates (agrep), and how to fix the crosswalk.
# ------------------------------------------------------------------------------
cerp_harmonize <- function(x, canonical, lookup = NULL) {
  canonical <- unique(canonical[!is.na(canonical) & nzchar(canonical)])
  # normalized geometry name -> original geometry name (first wins on collision)
  canon_norm <- cerp_norm(canonical)
  canon_map  <- canonical[!duplicated(canon_norm)]
  names(canon_map) <- canon_norm[!duplicated(canon_norm)]

  # normalized raw crosswalk key -> canonical target from the lookup table
  xwalk <- NULL
  if (!is.null(lookup) && nrow(lookup) > 0) {
    stopifnot(all(c("raw_name", "canonical_name") %in% names(lookup)))
    keep  <- !is.na(lookup$raw_name) & nzchar(lookup$raw_name)
    xwalk <- setNames(as.character(lookup$canonical_name[keep]),
                      cerp_norm(lookup$raw_name[keep]))
  }

  resolve_one <- function(v) {
    nv <- cerp_norm(v)
    if (nv %in% names(canon_map)) return(unname(canon_map[[nv]]))
    if (!is.null(xwalk) && nv %in% names(xwalk)) {
      ncn <- cerp_norm(xwalk[[nv]])
      if (ncn %in% names(canon_map)) return(unname(canon_map[[ncn]]))
    }
    NA_character_
  }

  uvals   <- unique(x)
  matched <- vapply(uvals, resolve_one, character(1))
  bad     <- uvals[is.na(matched)]

  if (length(bad) > 0) {
    lines <- vapply(bad, function(m) {
      sugg <- agrep(cerp_norm(m), unname(canon_map),
                    max.distance = 0.35, value = TRUE, ignore.case = TRUE)
      paste0("  - '", m, "'",
             if (length(sugg) > 0) paste0("  (closest geometry name: '",
                                          sugg[1], "'?)") else "")
    }, character(1))
    stop(
      "District name(s) in the data could not be matched to the map geometry:\n",
      paste(lines, collapse = "\n"),
      "\n\nFix: add a row to the versioned crosswalk ",
      "(1_data/geo/district_lookup.csv) mapping each raw name above to the ",
      "matching geometry name, then re-render.\n\nGeometry names available:\n  ",
      paste(sort(canonical), collapse = " | "),
      call. = FALSE
    )
  }

  # map back onto the full-length input, preserving order
  unname(setNames(matched, uvals)[x])
}

# ------------------------------------------------------------------------------
# cerp_render_one(): render a single Rmd with the house behavior shared by the
# production (2.2) and testing (2.4) knit scripts — isolated environment,
# tryCatch-continue, consistent cli messages. Returns TRUE on success, FALSE on
# failure (so callers can tally). rmarkdown/cli are only required at call time.
# ------------------------------------------------------------------------------
cerp_render_one <- function(input, output_dir, output_file = NULL,
                            params = NULL, label = basename(input)) {
  cerp_require(c("rmarkdown", "cli"))
  cli::cli_alert_info("Rendering: {label}")
  tryCatch({
    args <- list(
      input      = input,
      output_dir = output_dir,
      envir      = new.env(),   # isolate: no data bleed between templates
      quiet      = TRUE
    )
    if (!is.null(output_file)) args$output_file <- output_file
    if (length(params) > 0)    args$params      <- params
    out <- do.call(rmarkdown::render, args)
    cli::cli_alert_success("  -> {basename(output_dir)}/{basename(out)}")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("  FAILED: {label}")
    cli::cli_alert_warning("  {e$message}")
    FALSE
  })
}

# ------------------------------------------------------------------------------
# cerp_profile(): pre-flight data QA. The engine behind
# 3_templates/0.00_data_quality_report.Rmd (and the deferred Shiny app). Given a
# data.frame, return a two-part profile describing what the data looks like BEFORE
# a user picks a visual. It only DESCRIBES — it never mutates or "fixes" anything;
# deciding what to do about a flag is the report's (and the user's) job.
#
#   $columns  tibble, one tidy row per column:
#               column, type, n_missing, missing_pct, n_distinct,
#               whitespace_issues, date_parse_rate, outlier_count
#             type is one of: numeric | categorical | date | id-like | text |
#             empty. Detection is heuristic and deterministic (see detect_type).
#   $dataset  list of dataset-level flags: n_rows, n_cols, duplicate_rows,
#             empty_columns, single_row, header_whitespace.
#
# IMPORTANT: feed cerp_profile() the RAW file (readr::read_csv(path)) — NOT the
# cerp_load() output. cerp_load() scrubs header/value whitespace on purpose, which
# is exactly the class of problem this profiler exists to surface. This is the one
# deliberate exception to the "always cerp_load()" convention, and it lives here so
# the report can honor it in one clearly-labelled place.
# ------------------------------------------------------------------------------
cerp_profile <- function(data) {
  stopifnot(is.data.frame(data))
  nm <- names(data)
  nr <- nrow(data)

  # A cell is "blank" if it is NA or (for text) whitespace-only — both mean the
  # value carries no information, so both count toward missingness.
  is_blank <- function(x) {
    if (is.character(x)) is.na(x) | !nzchar(stringr::str_squish(x)) else is.na(x)
  }

  # Column names that read like identifiers rather than measures/categories.
  id_name_rx <- "(^|[ _])id([ _]|$)|_id$|^id$|uuid|serial|(^|[ _])code$"

  # detect_type(): assign one label per column. Order matters — dates are checked
  # before numeric because date strings ("2025-11-01") also satisfy parse_number()
  # (it would grab the year), and id-like is checked before free text so all-unique
  # key columns aren't mislabelled "text".
  detect_type <- function(x, name) {
    clean  <- if (is.character(x)) stringr::str_squish(x) else x
    non_na <- clean[!is_blank(x)]
    if (length(non_na) == 0) return("empty")
    nd      <- length(unique(non_na))
    id_hint <- grepl(id_name_rx, name, ignore.case = TRUE)

    if (inherits(x, "Date") || inherits(x, "POSIXt")) return("date")

    if (is.numeric(x)) {
      is_int <- all(non_na == floor(non_na))
      # id-like if an integer key named like an id (even when it repeats, e.g. a
      # panel unit), or an all-distinct integer key. A 0/1 flag stays numeric.
      if (is_int && nd > 1 && (id_hint || nd == length(non_na))) return("id-like")
      return("numeric")
    }

    if (is.logical(x)) return("categorical")

    chr <- as.character(non_na)
    # date? (before numeric — see note above)
    if (mean(!is.na(cerp_parse_date(chr, quiet = TRUE))) >= 0.8) return("date")
    # explicit key name, or every value distinct across a non-trivial column
    if (id_hint || (nd == length(non_na) && nd > 20)) return("id-like")
    # numbers stored as text ("1,200", "45%")
    if (mean(!is.na(suppressWarnings(cerp_numeric(chr, name)))) >= 0.8) return("numeric")
    # small level count -> categorical; otherwise free text
    if (nd <= 20 || nd <= 0.5 * length(non_na)) return("categorical")
    "text"
  }

  # Per-column one-row profile, bound together below.
  prof_one <- function(x, name) {
    blank     <- is_blank(x)
    n_missing <- sum(blank)
    clean     <- if (is.character(x)) stringr::str_squish(x) else x
    non_na    <- clean[!blank]
    type      <- detect_type(x, name)

    # cosmetic whitespace: raw value differs from its squished form
    ws <- if (is.character(x)) sum(!is.na(x) & x != stringr::str_squish(x)) else 0L

    # date-parse success, reported for anything date-ish or textual (NA otherwise)
    dpr <- if (is.character(x) || is.factor(x) ||
               inherits(x, "Date") || inherits(x, "POSIXt")) {
      if (length(non_na) == 0) NA_real_
      else round(mean(!is.na(cerp_parse_date(as.character(non_na), quiet = TRUE))), 3)
    } else NA_real_

    # 1.5xIQR outliers, only where a numeric reading is meaningful
    n_out <- NA_integer_
    if (type == "numeric") {
      num <- suppressWarnings(cerp_numeric(x, name))
      num <- num[!is.na(num)]
      if (length(num) >= 4 && length(unique(num)) > 1) {
        q   <- stats::quantile(num, c(0.25, 0.75), names = FALSE)
        iqr <- q[2] - q[1]
        n_out <- sum(num < q[1] - 1.5 * iqr | num > q[2] + 1.5 * iqr)
      } else {
        n_out <- 0L
      }
    }

    data.frame(
      column            = name,
      type              = type,
      n_missing         = n_missing,
      missing_pct       = if (nr > 0) round(100 * n_missing / nr, 1) else NA_real_,
      n_distinct        = length(unique(non_na)),
      whitespace_issues = as.integer(ws),
      date_parse_rate   = dpr,
      outlier_count     = n_out,
      stringsAsFactors  = FALSE
    )
  }

  columns <- if (length(nm) > 0) {
    dplyr::as_tibble(do.call(rbind, Map(prof_one, data, nm)))
  } else {
    dplyr::tibble(column = character(), type = character(),
                  n_missing = integer(), missing_pct = double(),
                  n_distinct = integer(), whitespace_issues = integer(),
                  date_parse_rate = double(), outlier_count = integer())
  }

  dataset <- list(
    n_rows            = nr,
    n_cols            = length(nm),
    duplicate_rows    = sum(duplicated(data)),
    empty_columns     = columns$column[columns$type == "empty"],
    single_row        = nr == 1,
    header_whitespace = nm[nm != stringr::str_squish(nm)]
  )

  list(columns = columns, dataset = dataset)
}

# ------------------------------------------------------------------------------
# cerp_recommend(): deterministic template recommender. The engine behind the
# `recommender` chunk of 3_templates/0.00_data_quality_report.Rmd AND the phase-12
# Shiny config-builder — one shared rule table, never duplicated. Given a profile
# (the cerp_profile() output) and the RAW data.frame, return the ranked templates
# whose column-type profile the data can feed, each with its suggested column ->
# *_var mapping. A fixed rule table + light name hints: no randomness, no model —
# same input always yields the same recommendations. Suggestions only; the user
# confirms the mapping in the template YAML (or the app's Map tab).
#
#   profile   the list returned by cerp_profile() (uses profile$columns).
#   raw_data  the RAW data.frame (readr::read_csv output) — needed for the Likert
#             level detector, which reads actual values.
#
# Returns a data.frame ranked by score (desc) then id (asc), one row per matched
# template: id, template, why, mapping (a "k = v; k = v" string), score. When no
# rule matches cleanly it returns a 0-row data.frame with those columns, so callers
# can branch on nrow() == 0.
# ------------------------------------------------------------------------------
cerp_recommend <- function(profile, raw_data) {
  cols <- profile$columns
  raw  <- raw_data

  # --- Role extraction from the profile ----------------------------------------
  nd        <- setNames(cols$n_distinct, cols$column)
  by_type   <- function(t) cols$column[cols$type == t]
  num_cols  <- by_type("numeric")
  cat_cols  <- by_type("categorical")
  date_cols <- by_type("date")
  id_cols   <- by_type("id-like")

  nm_match  <- function(pool, rx) pool[grepl(rx, pool, ignore.case = TRUE)]
  first     <- function(v) if (length(v)) v[[1]] else NA_character_

  # Likert-pattern detector: cleaned levels overlap a rating lexicon, 3–7 levels.
  likert_lex <- c("strongly disagree","disagree","somewhat disagree","neutral",
                  "neither agree nor disagree","somewhat agree","agree","strongly agree",
                  "strongly dissatisfied","dissatisfied","satisfied","very satisfied",
                  "strongly satisfied","never","rarely","sometimes","often","always",
                  "poor","fair","good","very good","excellent")
  is_likert <- function(colname) {
    v <- unique(tolower(stringr::str_squish(as.character(raw[[colname]]))))
    v <- v[nzchar(v) & !is.na(v)]
    if (length(v) < 3 || length(v) > 7) return(FALSE)
    mean(v %in% likert_lex) >= 0.6
  }

  # Named-role sub-pools (deterministic; name hints only sharpen, never fabricate).
  year_num   <- nm_match(num_cols, "year|yr|wave|round|period")
  time_cols  <- union(date_cols, year_num)
  target_num <- nm_match(num_cols, "target|goal|benchmark")
  event_num  <- nm_match(num_cols, "event.?time|rel.?time|time.?to|relative")
  stage_cats <- nm_match(cat_cols, "stage|step|phase|pipeline")
  district   <- nm_match(c(cat_cols, id_cols, by_type("text")),
                         "district|region|province|tehsil|division|zone")
  # A treatment flag is any 2-level column — categorical ("Treatment"/"Control")
  # OR numeric (0/1). Name hints (treat/arm/…) sharpen the pick when present.
  binary_cols <- cols$column[cols$n_distinct == 2 &
                             cols$type %in% c("categorical", "numeric")]
  treat_cats <- {
    hinted <- nm_match(binary_cols, "treat|arm|group|condition|intervention|treated")
    if (length(hinted)) hinted else binary_cols
  }
  likert_cats <- cat_cols[vapply(cat_cols, is_likert, logical(1))]
  entity_cats <- nm_match(c(cat_cols, id_cols),
                          "district|school|clinic|facility|entity|unit|region|name")
  # "Plain" measures: numeric that isn't a target/event-time/year role.
  measure_num <- setdiff(num_cols, c(target_num, event_num, year_num))
  outcome_pref <- function() {
    hinted <- nm_match(measure_num, "score|outcome|rate|result|amount|index|value")
    first(if (length(hinted)) hinted else measure_num)
  }

  # --- Rule accumulator ---------------------------------------------------------
  recs <- list()
  add  <- function(id, name, why, mapping, score) {
    recs[[length(recs) + 1]] <<- data.frame(
      id = id, template = name, why = why,
      mapping = paste(sprintf("%s = %s", names(mapping), unlist(mapping)),
                      collapse = "; "),
      score = score, stringsAsFactors = FALSE)
  }
  has <- function(x) length(x) > 0 && !all(is.na(x))

  # --- Rules (one per production template) --------------------------------------
  # 3.01 dumbbell — 2-level group + before/after numerics
  {
    before <- first(nm_match(measure_num, "base|pre|before|initial"))
    after  <- first(nm_match(measure_num, "end|post|after|final"))
    if (has(treat_cats) && !is.na(before) && !is.na(after) && before != after) {
      add("3.01", "Baseline vs endline (dumbbell)",
          "A treatment/control group with matched before/after measures.",
          list(group_var = first(treat_cats), before_var = before, after_var = after), 9)
    }
  }
  # 3.02 distribution shifts — group + one numeric
  if (has(cat_cols) && has(measure_num)) {
    add("3.02", "Distribution shifts (density)",
        "A grouping column and a numeric outcome to compare distributions.",
        list(group_var = first(cat_cols), outcome_var = outcome_pref()), 5)
  }
  # 3.03 treatment effects (forest) — treatment 2-level + numeric outcome
  if (has(treat_cats) && has(measure_num)) {
    add("3.03", "Treatment effects (forest plot)",
        "A treatment/control flag and a numeric outcome.",
        list(group_var = first(treat_cats), outcome_var = outcome_pref()), 7)
  }
  # 3.04 subgroup impacts — treatment + a second categorical + numeric
  {
    sub <- first(setdiff(cat_cols, first(treat_cats)))
    if (has(treat_cats) && !is.na(sub) && has(measure_num)) {
      add("3.04", "Subgroup impacts (coefficient plot)",
          "A treatment flag, a subgroup dimension, and a numeric outcome.",
          list(group_var = first(treat_cats), subgroup_var = sub,
               outcome_var = outcome_pref()), 6)
    }
  }
  # 3.05 waffle — a single categorical (parts of a whole)
  if (has(cat_cols)) {
    add("3.05", "Waffle chart (share of whole)",
        "A categorical status column shown as proportions.",
        list(status_var = first(cat_cols)), 3)
  }
  # 3.06 slopegraph — entity + time (>=2 points) + numeric
  if (has(entity_cats) && has(time_cols) && has(measure_num)) {
    add("3.06", "Slopegraph (two-point change)",
        "An entity, a time column, and a value — change between two periods.",
        list(entity_var = first(entity_cats), time_var = first(time_cols),
             value_var = outcome_pref()), 5)
  }
  # 3.07 diverging bar — entity + Likert response
  if (has(likert_cats)) {
    add("3.07", "Diverging bar (Likert sentiment)",
        "A Likert-scale response column, ideal for a diverging bar.",
        list(entity_var = if (has(entity_cats)) first(entity_cats) else first(cat_cols),
             response_var = first(likert_cats)), 8)
  }
  # 3.08 icon array — a single categorical proportion
  if (has(cat_cols)) {
    add("3.08", "Icon array (frequency)",
        "A categorical column shown as an intuitive icon frequency.",
        list(category_var = first(cat_cols)), 3)
  }
  # 3.09 waterfall — stage-like categorical + numeric
  if (has(stage_cats) && has(measure_num)) {
    add("3.09", "Waterfall (running total by stage)",
        "A stage/step column with one numeric amount per stage.",
        list(category_var = first(stage_cats), value_var = outcome_pref()), 7)
  }
  # 3.10 bump — entity + time (>=3 points) + numeric
  {
    t <- first(time_cols)
    if (has(entity_cats) && !is.na(t) && nd[t] >= 3 && has(measure_num)) {
      add("3.10", "Bump chart (ranking over time)",
          "An entity tracked across 3+ time points on a numeric metric.",
          list(entity_var = first(entity_cats), time_var = t,
               value_var = outcome_pref()), 6)
    }
  }
  # 3.11 bullet — entity + value + target numeric
  if (has(entity_cats) && has(measure_num) && has(target_num)) {
    add("3.11", "Bullet chart (value vs target)",
        "An entity with a value and an explicit target/benchmark column.",
        list(entity_var = first(entity_cats), value_var = outcome_pref(),
             target_var = first(target_num)), 7)
  }
  # 3.12 deviation bar — entity/category + numeric
  if (has(entity_cats) && has(measure_num)) {
    add("3.12", "Deviation bar (vs reference)",
        "An entity/category with a numeric value to show deviation from a reference.",
        list(entity_var = first(entity_cats), value_var = outcome_pref()), 4)
  }
  # 3.13 ridgeline — multi-level categorical + numeric distribution
  {
    grp <- first(cat_cols[nd[cat_cols] >= 3])
    if (!is.na(grp) && has(measure_num)) {
      add("3.13", "Ridgeline (distributions by group)",
          "A categorical with 3+ groups and a numeric distribution to compare.",
          list(group_var = grp, value_var = outcome_pref()), 5)
    }
  }
  # 3.14 calendar heatmap — a date column + numeric
  if (has(date_cols) && has(measure_num)) {
    add("3.14", "Calendar heatmap (daily values)",
        "A real date column with a numeric value — daily intensity over a calendar.",
        list(date_var = first(date_cols), value_var = outcome_pref()), 7)
  }
  # 3.15 choropleth — district-like names + numeric
  if (has(district) && has(measure_num)) {
    add("3.15", "Choropleth map (by district)",
        "A district/region name column and a numeric metric to shade.",
        list(region_var = first(district), value_var = outcome_pref()), 8)
  }
  # 3.16 event study — unit + time + event-time + treatment + numeric outcome (panel)
  if (has(id_cols) && has(time_cols) && has(event_num) &&
      has(treat_cats) && has(measure_num)) {
    add("3.16", "Event study (dynamic effects)",
        "A panel with unit, calendar time, event-time, a treatment flag, and an outcome.",
        list(unit_var = first(id_cols), time_var = first(time_cols),
             event_time_var = first(event_num), treat_var = first(treat_cats),
             outcome_var = outcome_pref()), 9)
  }
  # 3.17 small multiples — facet categorical + x + y
  {
    facet <- first(cat_cols[nd[cat_cols] >= 3])
    xv    <- first(time_cols)
    if (!is.na(facet) && !is.na(xv) && has(measure_num)) {
      add("3.17", "Small multiples (faceted trend)",
          "A faceting category, an x (usually time), and a numeric y.",
          list(facet_var = facet, x_var = xv, y_var = outcome_pref()), 5)
    }
  }
  # 3.18 heatmap matrix — two categorical/time axes + numeric
  {
    two_cats <- union(cat_cols, time_cols)  # rows x cols; a time column makes a good axis
    if (length(unique(two_cats)) >= 2 && has(measure_num)) {
      add("3.18", "Heatmap matrix (category × category)",
          "Two categorical/time axes and a numeric cell value.",
          list(row_var = two_cats[[1]], col_var = two_cats[[2]],
               value_var = outcome_pref()), 4)
    }
  }

  # --- Rank (score desc, id asc) and return -------------------------------------
  if (length(recs) == 0) {
    return(data.frame(id = character(), template = character(), why = character(),
                      mapping = character(), score = numeric(),
                      stringsAsFactors = FALSE))
  }
  rec_df <- do.call(rbind, recs)
  rec_df[order(-rec_df$score, rec_df$id), , drop = FALSE]
}
