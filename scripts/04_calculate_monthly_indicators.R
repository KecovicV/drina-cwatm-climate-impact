suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

source("functions/monthly_indicators.R")

config <- list(
  base_dir = "data/raw/restore4life",
  analysis_dir = "results/indicators",
  scenarios = c("historical", "ssp126", "ssp245", "ssp585"),
  periods = tibble::tribble(
    ~period, ~start_date, ~end_date,
    "historical_1990-2014", as.Date("1990-01-01"), as.Date("2014-12-31"),
    "future_2031-2060", as.Date("2031-01-01"), as.Date("2060-12-31")
  )
)

dir.create(config$analysis_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(config$analysis_dir, "log_monthly_indicators.txt")
out_path <- file.path(config$analysis_dir, "iafd_12_monthly_medians.csv")

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

runs <- discover_runs(config$base_dir, config$scenarios)
log_msg("Runs discovered: ", nrow(runs))

failed_runs <- list()

results <- purrr::map_dfr(seq_len(nrow(runs)), function(i) {
  scenario_name <- runs$scenario[i]
  gcm_name <- runs$gcm[i]
  run_path <- runs$run_path[i]

  month_file <- find_monthavg_file(run_path)
  if (is.na(month_file)) {
    log_msg("Missing monthly file for ", scenario_name, " / ", gcm_name)
    failed_runs[[length(failed_runs) + 1]] <<- tibble(
      scenario = scenario_name,
      gcm = gcm_name,
      reason = "file_not_found"
    )
    return(tibble())
  }

  monthly_df <- tryCatch(
    read_cwatm_monthavg(month_file),
    error = function(e) {
      log_msg("Read failed for ", scenario_name, " / ", gcm_name, ": ", e$message)
      NULL
    }
  )

  if (is.null(monthly_df) || nrow(monthly_df) == 0) {
    log_msg("Parsed empty for ", scenario_name, " / ", gcm_name)
    failed_runs[[length(failed_runs) + 1]] <<- tibble(
      scenario = scenario_name,
      gcm = gcm_name,
      reason = "parsed_empty"
    )
    return(tibble())
  }

  purrr::map_dfr(seq_len(nrow(config$periods)), function(j) {
    p <- config$periods[j, ]

    out <- calc_iafd_wide(
      df = monthly_df,
      start_date = p$start_date[[1]],
      end_date = p$end_date[[1]]
    )

    if (is.null(out)) return(tibble())

    tibble(
      scenario = scenario_name,
      gcm = gcm_name,
      period = p$period[[1]],
      source_file = basename(month_file),
      n_months = nrow(monthly_df %>% filter(date >= p$start_date[[1]], date <= p$end_date[[1]])),
      date_min = min(monthly_df$date[monthly_df$date >= p$start_date[[1]] & monthly_df$date <= p$end_date[[1]]], na.rm = TRUE),
      date_max = max(monthly_df$date[monthly_df$date >= p$start_date[[1]] & monthly_df$date <= p$end_date[[1]]], na.rm = TRUE)
    ) %>% bind_cols(out)
  })
})

if (length(failed_runs) > 0) {
  failed_df <- dplyr::bind_rows(failed_runs)
  write_csv(failed_df, file.path(config$analysis_dir, "iafd_failed_runs.csv"))
  log_msg("Failed runs recorded: ", nrow(failed_df))
}

if (nrow(results) == 0) {
  log_msg("No monthly indicators calculated.")
  print(results)
} else {
  write_csv(results, out_path)
  log_msg("Wrote monthly indicators: ", out_path)
  print(results %>% arrange(scenario, gcm, period), n = nrow(results))
}
