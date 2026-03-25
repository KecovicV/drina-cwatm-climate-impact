suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

source("functions/fdc_functions.R")

config <- list(
  base_dir = "data/raw/restore4life",
  analysis_dir = "results/fdc",
  discharge_file = "discharge_daily.csv",
  scenarios = c("historical", "ssp126", "ssp245", "ssp585"),
  t_grid = 0:100,
  periods = tibble::tribble(
    ~period, ~start_date, ~end_date,
    "historical_1990-2014", as.Date("1990-01-01"), as.Date("2014-12-31"),
    "future_2031-2060", as.Date("2031-01-01"), as.Date("2060-12-31")
  )
)

dir.create(config$analysis_dir, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(config$analysis_dir, "log_fdc_metrics.txt")
out_5pt_path <- file.path(config$analysis_dir, "fdc_5points_by_run_period.csv")
out_gcm_path <- file.path(config$analysis_dir, "fdc_gcm_period.csv")
out_int_path <- file.path(config$analysis_dir, "fdc_ensemble_intervals.csv")
out_failed_path <- file.path(config$analysis_dir, "fdc_failed_runs.csv")

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
rows_5pt <- list()
rows_gcm <- list()

for (i in seq_len(nrow(runs))) {
  scenario_name <- runs$scenario[i]
  gcm_name <- runs$gcm[i]
  run_path <- runs$run_path[i]

  daily_path <- find_daily_file(run_path, preferred_name = config$discharge_file)
  if (is.na(daily_path)) {
    log_msg("Missing daily file for ", scenario_name, " / ", gcm_name)
    failed_runs[[length(failed_runs) + 1]] <- tibble(
      scenario = scenario_name,
      gcm = gcm_name,
      reason = "daily_file_not_found"
    )
    next
  }

  daily_df <- tryCatch(
    read_cwatm_daily_fdc(daily_path),
    error = function(e) {
      log_msg("Read failed for ", scenario_name, " / ", gcm_name, ": ", e$message)
      failed_runs[[length(failed_runs) + 1]] <<- tibble(
        scenario = scenario_name,
        gcm = gcm_name,
        reason = e$message
      )
      NULL
    }
  )

  if (is.null(daily_df) || nrow(daily_df) == 0) {
    next
  }

  for (j in seq_len(nrow(config$periods))) {
    p <- config$periods[j, ]

    out_5pt <- calc_fdc_5points(
      df = daily_df,
      start_date = p$start_date[[1]],
      end_date = p$end_date[[1]]
    )

    if (!is.null(out_5pt)) {
      rows_5pt[[length(rows_5pt) + 1]] <- out_5pt %>%
        mutate(
          scenario = scenario_name,
          gcm = gcm_name,
          period = p$period[[1]],
          source_file = basename(daily_path),
          .before = 1
        )
    }

    out_curve <- calc_fdc_gcm_curve(
      df = daily_df,
      start_date = p$start_date[[1]],
      end_date = p$end_date[[1]],
      t_grid = config$t_grid
    )

    if (!is.null(out_curve)) {
      rows_gcm[[length(rows_gcm) + 1]] <- out_curve %>%
        mutate(
          scenario = scenario_name,
          gcm = gcm_name,
          period = p$period[[1]],
          source_file = basename(daily_path),
          .before = 1
        )
    }
  }

  if (i %% 6 == 0) {
    log_msg("Processed runs: ", i)
  }
}

fdc_5pt_tbl <- bind_rows(rows_5pt)
fdc_gcm_tbl <- bind_rows(rows_gcm)
fdc_int_tbl <- calc_fdc_ensemble_intervals(fdc_gcm_tbl)

if (nrow(fdc_5pt_tbl) > 0) {
  write_csv(fdc_5pt_tbl, out_5pt_path)
  log_msg("Wrote FDC 5-point table: ", out_5pt_path)
  print(fdc_5pt_tbl %>% arrange(scenario, gcm, period), n = nrow(fdc_5pt_tbl))
} else {
  log_msg("No FDC 5-point results calculated.")
}

if (nrow(fdc_gcm_tbl) > 0) {
  write_csv(fdc_gcm_tbl, out_gcm_path)
  log_msg("Wrote GCM FDC curves: ", out_gcm_path)
} else {
  log_msg("No GCM FDC curves calculated.")
}

if (nrow(fdc_int_tbl) > 0) {
  write_csv(fdc_int_tbl, out_int_path)
  log_msg("Wrote ensemble FDC intervals: ", out_int_path)
} else {
  log_msg("No ensemble FDC intervals calculated.")
}

if (length(failed_runs) > 0) {
  failed_df <- bind_rows(failed_runs)
  write_csv(failed_df, out_failed_path)
  log_msg("Failed runs recorded: ", nrow(failed_df))
}
