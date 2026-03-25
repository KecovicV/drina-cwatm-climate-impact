suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

source("functions/climate_indicators.R")

config <- list(
  base_dir = "data/raw/restore4life",
  analysis_dir = "results/indicators",
  scenarios = c("historical", "ssp126", "ssp245", "ssp585"),
  periods = tibble::tribble(
    ~period, ~start_date, ~end_date,
    "historical_1990-2014", as.Date("1990-01-01"), as.Date("2014-12-31"),
    "future_2031-2060", as.Date("2031-01-01"), as.Date("2060-12-31")
  ),
  precip_file = "Precipitation_areaavg_annualtot.csv",
  pet_file = "ETRef_areaavg_annualtot.csv",
  runoff_file = "runoff_areaavg_annualtot.csv"
)

dir.create(config$analysis_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(config$analysis_dir, "log_climate_indicators.txt")
out_ai_path <- file.path(config$analysis_dir, "AI_aridity_index.csv")
out_eta_path <- file.path(config$analysis_dir, "runoff_coefficient_eta.csv")
out_precip_path <- file.path(config$analysis_dir, "precip_annual_mean_by_gcm_scenario.csv")
out_precip_change_path <- file.path(config$analysis_dir, "precip_change_ssp_vs_hist_by_gcm.csv")
out_failed_path <- file.path(config$analysis_dir, "climate_failed_runs.csv")

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(line, "\n")
  write(line, file = log_path, append = TRUE)
}

runs <- discover_runs(config$base_dir, config$scenarios)
log_msg("Runs discovered: ", nrow(runs))

failed_runs <- list()
ai_rows <- list()
eta_rows <- list()
precip_rows <- list()

for (i in seq_len(nrow(runs))) {
  scenario_name <- runs$scenario[i]
  gcm_name <- runs$gcm[i]
  run_path <- runs$run_path[i]

  precip_path <- find_annual_file(run_path, config$precip_file)
  pet_path <- find_annual_file(run_path, config$pet_file)
  runoff_path <- find_annual_file(run_path, config$runoff_file)

  precip_df <- tryCatch(
    if (!is.na(precip_path)) read_cwatm_annual_series(precip_path) else NULL,
    error = function(e) {
      log_msg("Precip read failed for ", scenario_name, " / ", gcm_name, ": ", e$message)
      failed_runs[[length(failed_runs) + 1]] <<- tibble(
        scenario = scenario_name,
        gcm = gcm_name,
        variable = "precipitation",
        reason = e$message
      )
      NULL
    }
  )

  pet_df <- tryCatch(
    if (!is.na(pet_path)) read_cwatm_annual_series(pet_path) else NULL,
    error = function(e) {
      log_msg("PET read failed for ", scenario_name, " / ", gcm_name, ": ", e$message)
      failed_runs[[length(failed_runs) + 1]] <<- tibble(
        scenario = scenario_name,
        gcm = gcm_name,
        variable = "pet",
        reason = e$message
      )
      NULL
    }
  )

  runoff_df <- tryCatch(
    if (!is.na(runoff_path)) read_cwatm_annual_series(runoff_path) else NULL,
    error = function(e) {
      log_msg("Runoff read failed for ", scenario_name, " / ", gcm_name, ": ", e$message)
      failed_runs[[length(failed_runs) + 1]] <<- tibble(
        scenario = scenario_name,
        gcm = gcm_name,
        variable = "runoff",
        reason = e$message
      )
      NULL
    }
  )

  for (j in seq_len(nrow(config$periods))) {
    p <- config$periods[j, ]

    if (!is.null(precip_df)) {
      precip_out <- calc_mean_annual_value(
        df = precip_df,
        start_date = p$start_date[[1]],
        end_date = p$end_date[[1]]
      )

      if (!is.null(precip_out)) {
        precip_rows[[length(precip_rows) + 1]] <- precip_out %>%
          mutate(
            scenario = scenario_name,
            gcm = gcm_name,
            period = p$period[[1]],
            source_file = basename(precip_path),
            .before = 1
          )
      }
    }

    if (!is.null(precip_df) && !is.null(pet_df)) {
      ai_out <- calc_aridity_index(
        precip_df = precip_df,
        pet_df = pet_df,
        start_date = p$start_date[[1]],
        end_date = p$end_date[[1]]
      )

      if (!is.null(ai_out)) {
        ai_rows[[length(ai_rows) + 1]] <- ai_out %>%
          mutate(
            scenario = scenario_name,
            gcm = gcm_name,
            period = p$period[[1]],
            precip_file = basename(precip_path),
            pet_file = basename(pet_path),
            .before = 1
          )
      }
    }

    if (!is.null(precip_df) && !is.null(runoff_df)) {
      eta_out <- calc_runoff_coefficient(
        runoff_df = runoff_df,
        precip_df = precip_df,
        start_date = p$start_date[[1]],
        end_date = p$end_date[[1]]
      )

      if (!is.null(eta_out)) {
        eta_rows[[length(eta_rows) + 1]] <- eta_out %>%
          mutate(
            scenario = scenario_name,
            gcm = gcm_name,
            period = p$period[[1]],
            runoff_file = basename(runoff_path),
            precip_file = basename(precip_path),
            .before = 1
          )
      }
    }
  }
}

ai_tbl <- bind_rows(ai_rows)
eta_tbl <- bind_rows(eta_rows)
precip_tbl <- bind_rows(precip_rows)

if (nrow(ai_tbl) > 0) {
  write_csv(ai_tbl, out_ai_path)
  log_msg("Wrote AI table: ", out_ai_path)
  print(ai_tbl %>% arrange(scenario, gcm, period), n = nrow(ai_tbl))
} else {
  log_msg("No AI results calculated.")
}

if (nrow(eta_tbl) > 0) {
  write_csv(eta_tbl, out_eta_path)
  log_msg("Wrote runoff coefficient table: ", out_eta_path)
  print(eta_tbl %>% arrange(scenario, gcm, period), n = nrow(eta_tbl))
} else {
  log_msg("No runoff coefficient results calculated.")
}

if (nrow(precip_tbl) > 0) {
  write_csv(precip_tbl, out_precip_path)
  log_msg("Wrote precipitation summary: ", out_precip_path)

  precip_change_tbl <- calc_change_vs_historical(precip_tbl, "mean_annual") %>%
    rename(
      hist_mean = historical_value,
      ssp_mean = future_value
    ) %>%
    select(gcm, scenario, period, hist_mean, ssp_mean, change_abs, change_pct) %>%
    arrange(scenario, gcm)

  write_csv(precip_change_tbl, out_precip_change_path)
  log_msg("Wrote precipitation changes: ", out_precip_change_path)
} else {
  log_msg("No precipitation summary calculated.")
}

if (length(failed_runs) > 0) {
  failed_df <- bind_rows(failed_runs)
  write_csv(failed_df, out_failed_path)
  log_msg("Failed runs recorded: ", nrow(failed_df))
}
