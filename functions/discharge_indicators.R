suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

read_observed_thresholds <- function(path) {
  if (!file.exists(path)) {
    stop("Threshold file not found: ", path)
  }
  
  thr <- readr::read_csv(path, show_col_types = FALSE)
  
  required_cols <- c("Q_low_thr_P20_obs", "Q_high_thr_5xMedian_obs")
  missing_cols <- setdiff(required_cols, names(thr))
  
  if (length(missing_cols) > 0) {
    stop("Missing threshold columns in file: ", paste(missing_cols, collapse = ", "))
  }
  
  tibble(
    Q_low_thr = thr$Q_low_thr_P20_obs[[1]],
    Q_high_thr = thr$Q_high_thr_5xMedian_obs[[1]]
  )
}

read_cwatm_discharge_daily <- function(path) {
  if (!file.exists(path)) {
    stop("Discharge file not found: ", path)
  }
  
  # CWatM file structure:
  # line 1: metadata
  # line 2: xloc
  # line 3: yloc
  # line 4: actual header (Date,G1)
  df <- readr::read_csv(
    file = path,
    skip = 3,
    col_select = 1:2,
    show_col_types = FALSE,
    trim_ws = TRUE
  )
  
  required_cols <- c("Date", "G1")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Expected columns not found in discharge file: ",
      paste(missing_cols, collapse = ", "),
      " | File: ", path
    )
  }
  
  out <- df %>%
    transmute(
      date = as.Date(Date, format = "%d/%m/%Y"),
      discharge = as.numeric(G1)
    ) %>%
    filter(!is.na(date), !is.na(discharge))
  
  if (nrow(out) == 0) {
    stop("No valid rows parsed from discharge file: ", path)
  }
  
  out
}

calc_frequency_percent <- function(x, threshold, condition = c("below", "above")) {
  condition <- match.arg(condition)
  
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  
  if (condition == "below") {
    100 * mean(x < threshold)
  } else {
    100 * mean(x > threshold)
  }
}

calc_discharge_indicators <- function(df,
                                      start_date,
                                      end_date,
                                      period_label,
                                      q_low_thr,
                                      q_high_thr) {
  period_df <- df %>%
    filter(date >= start_date, date <= end_date)
  
  if (nrow(period_df) == 0) {
    return(NULL)
  }
  
  tibble(
    period = period_label,
    n_days = nrow(period_df),
    date_min = min(period_df$date, na.rm = TRUE),
    date_max = max(period_df$date, na.rm = TRUE),
    Q50 = as.numeric(stats::quantile(period_df$discharge, probs = 0.50, na.rm = TRUE, names = FALSE)),
    Q5  = as.numeric(stats::quantile(period_df$discharge, probs = 0.05, na.rm = TRUE, names = FALSE)),
    Q95 = as.numeric(stats::quantile(period_df$discharge, probs = 0.95, na.rm = TRUE, names = FALSE)),
    LFF = calc_frequency_percent(period_df$discharge, q_low_thr, condition = "below"),
    HFF = calc_frequency_percent(period_df$discharge, q_high_thr, condition = "above")
  )
}