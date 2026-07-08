# ==============================================================================
# Script Name: 2.5_helpers.R
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
