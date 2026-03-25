suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

source("functions/reference_thresholds.R")

config <- list(
  observed_path = "data/raw/observed/Radalj_observed.csv",
  output_dir = "results/reference",
  station_name = "Radalj",
  start_date = as.Date("1990-01-01"),
  end_date = as.Date("2014-12-31"),
  date_col = "Date",
  discharge_col = "Qobs(m3/s)"
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- file.path(config$output_dir, "thresholds_observed_radalj_1990-2014.csv")
log_path <- file.path(config$output_dir, "log_thresholds_observed_radalj.txt")

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(line, "\n")
  write(line, file = log_path, append = TRUE)
}

observed_df <- read_observed_discharge(
  path = config$observed_path,
  date_col = config$date_col,
  discharge_col = config$discharge_col
)

reference_df <- filter_reference_period(
  df = observed_df,
  start_date = config$start_date,
  end_date = config$end_date
)

thresholds <- compute_reference_thresholds(
  df = reference_df,
  station_name = config$station_name,
  start_date = config$start_date,
  end_date = config$end_date,
  source_file = config$observed_path
)

write_csv(thresholds, out_path)

log_msg("Observed file: ", config$observed_path)
log_msg("Rows used: ", thresholds$n_days_used)
log_msg("Q50_obs: ", signif(thresholds$Q50_obs_median, 6))
log_msg("Q_low_thr (P20): ", signif(thresholds$Q_low_thr_P20_obs, 6))
log_msg("Q_high_thr (5xQ50): ", signif(thresholds$Q_high_thr_5xMedian_obs, 6))
log_msg("Wrote thresholds: ", out_path)

print(thresholds)
