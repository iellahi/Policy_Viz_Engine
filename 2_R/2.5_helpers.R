# ==============================================================================
# Script Name: 2.5_helpers.R
# Purpose:     Shared data-loading, validation, and parsing utilities used by
#              every template. Design principle: fail LOUDLY and clearly —
#              a template must never render a silently-wrong chart.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, stringr, forcats, lubridate)

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
