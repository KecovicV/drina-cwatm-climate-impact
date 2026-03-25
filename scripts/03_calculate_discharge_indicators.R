suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

source("functions/discharge_indicators.R")

config <- list(
  base_dir = "data/raw/restore4life",
  analysis_dir = "results/indicators",
  thresholds_path = "results/reference/thresholds_observed_radalj_1990-2014.csv",
  discharge_file = "discharge_daily.csv",
  scenarios = c("historical", "ssp126", "ssp245", "ssp585"),
  periods = tibble::tribble(
    ~period, ~start_date, ~end_date,
    "historical_1990-2014", as.Date("1990-01-01"), as.Date("2014-12-31"),
    "future_2031-2060", as.Date("2031-01-01"), as.Date("2060-12-31")
  )
)

dir.create(config$analysis_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(config$analysis_dir, "log_discharge_indicators.txt")
out_path <- file.path(config$analysis_dir, "discharge_indicators_absolute.csv")

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(line, "\n")
  write(line, file = log_path, append = TRUE)
}

discover_runs <- function(base_dir, scenarios) {
  purrr::map_dfr(scenarios, function(scenario_name) {
    scenario_path <- file.path(base_dir, scenario_name)
    if (!dir.exists(scenario_path)) return(tibble())

    gcms <- list.dirs(scenario_path, full.names = FALSE, recursive = FALSE)

    tibble(
      scenario = scenario_name,
      gcm = gcms,
      run_path = file.path(scenario_path, gcms)
    )
  })
}

thresholds <- read_observed_thresholds(config$thresholds_path)
log_msg("Using thresholds: Q_low_thr=", thresholds$Q_low_thr, ", Q_high_thr=", thresholds$Q_high_thr)

runs <- discover_runs(config$base_dir, config$scenarios)
log_msg("Runs discovered: ", nrow(runs))

results <- purrr::map_dfr(seq_len(nrow(runs)), function(i) {
  scenario_name <- runs$scenario[i]
  gcm_name <- runs$gcm[i]
  run_path <- runs$run_path[i]
  discharge_path <- file.path(run_path, config$discharge_file)

  if (!file.exists(discharge_path)) {
    log_msg("Missing file: ", discharge_path)
    return(tibble())
  }

  discharge_df <- tryCatch(
    read_cwatm_discharge_daily(discharge_path),
    error = function(e) {
      log_msg("Read failed for ", scenario_name, " / ", gcm_name, ": ", e$message)
      NULL
    }
  )

  if (is.null(discharge_df) || nrow(discharge_df) == 0) {
    log_msg("Parsed empty: ", discharge_path)
    return(tibble())
  }

  purrr::map_dfr(seq_len(nrow(config$periods)), function(j) {
    p <- config$periods[j, ]

    out <- calc_discharge_indicators(
      df = discharge_df,
      start_date = p$start_date[[1]],
      end_date = p$end_date[[1]],
      period_label = p$period[[1]],
      q_low_thr = thresholds$Q_low_thr,
      q_high_thr = thresholds$Q_high_thr
    )

    if (is.null(out)) return(tibble())

    out %>%
      mutate(
        scenario = scenario_name,
        gcm = gcm_name,
        source_file = basename(discharge_path),
        .before = 1
      )
  })
})

if (nrow(results) == 0) {
  log_msg("No runs found or no indicators calculated.")
  print(results)
} else {
  write_csv(results, out_path)
  log_msg("Wrote indicators: ", out_path)
  print(results %>% arrange(scenario, gcm, period), n = nrow(results))
}
