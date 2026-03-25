suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tibble)
})

parse_observed_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))

  xc <- str_trim(as.character(x))
  d <- suppressWarnings(dmy(xc))
  if (all(is.na(d))) d <- suppressWarnings(ymd(xc))
  if (all(is.na(d))) d <- suppressWarnings(mdy(xc))
  as.Date(d)
}

parse_numeric_safe <- function(x) {
  if (is.numeric(x)) return(x)
  xc <- str_trim(as.character(x))
  xc <- str_replace_all(xc, ",", ".")
  suppressWarnings(as.numeric(xc))
}

assert_file_exists <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
}

read_observed_discharge <- function(path, date_col, discharge_col) {
  assert_file_exists(path)

  raw <- read_csv(path, show_col_types = FALSE)

  if (!(date_col %in% names(raw))) {
    stop("Missing date column: ", date_col)
  }
  if (!(discharge_col %in% names(raw))) {
    stop("Missing discharge column: ", discharge_col)
  }

  raw %>%
    transmute(
      date = parse_observed_date(.data[[date_col]]),
      Qobs = parse_numeric_safe(.data[[discharge_col]])
    ) %>%
    filter(!is.na(date), !is.na(Qobs)) %>%
    arrange(date)
}

filter_reference_period <- function(df, start_date, end_date) {
  df %>%
    filter(date >= start_date, date <= end_date)
}

compute_reference_thresholds <- function(df, station_name, start_date, end_date, source_file) {
  if (nrow(df) == 0) {
    stop("No valid observed discharge data in selected reference period.")
  }

  expected_days <- as.integer(end_date - start_date) + 1
  actual_days <- nrow(df)

  q50_obs <- median(df$Qobs, na.rm = TRUE)
  q_low_thr <- quantile(df$Qobs, 0.20, type = 7, names = FALSE, na.rm = TRUE)
  q_high_thr <- 5 * q50_obs

  tibble(
    station = station_name,
    period_start = start_date,
    period_end = end_date,
    n_days_expected = expected_days,
    n_days_used = actual_days,
    Q50_obs_median = q50_obs,
    Q_low_thr_P20_obs = q_low_thr,
    Q_high_thr_5xMedian_obs = q_high_thr,
    source_file = basename(source_file)
  )
}
