suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

# -----------------------------
# Discover scenario / GCM runs
# -----------------------------
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

# -----------------------------
# Find annual file in a run dir
# -----------------------------
find_annual_file <- function(run_path, filename) {
  p <- file.path(run_path, filename)
  if (file.exists(p)) p else NA_character_
}

# ------------------------------------------
# Read CWatM annual csv with metadata header
# ------------------------------------------
read_cwatm_annual_series <- function(path) {
  if (!file.exists(path)) {
    stop("Annual file not found: ", path)
  }

  raw_lines <- readLines(path, warn = FALSE)
  header_idx <- which(grepl("^Date\\s*,", trimws(raw_lines)))[1]

  if (is.na(header_idx)) {
    stop("Could not find 'Date,...' header in: ", path)
  }

  # skip lines before actual header
  df <- readr::read_csv(
    file = path,
    skip = header_idx - 1,
    col_select = 1:2,
    show_col_types = FALSE,
    trim_ws = TRUE
  )

  required_cols <- c("Date", "G1")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Expected columns not found in annual file: ",
      paste(missing_cols, collapse = ", "),
      " | File: ", path
    )
  }

  out <- df %>%
    transmute(
      date = as.Date(Date, format = "%d/%m/%Y"),
      year = as.integer(format(date, "%Y")),
      value = as.numeric(G1)
    ) %>%
    filter(!is.na(year), !is.na(value))

  if (nrow(out) == 0) {
    stop("No valid rows parsed from annual file: ", path)
  }

  out
}

# ----------------------------------
# Filter a yearly series to a period
# ----------------------------------
filter_period_years <- function(df, start_date, end_date) {
  start_year <- as.integer(format(start_date, "%Y"))
  end_year <- as.integer(format(end_date, "%Y"))

  df %>% filter(year >= start_year, year <= end_year)
}

# ------------------------------------
# Mean annual value in selected period
# ------------------------------------
calc_mean_annual_value <- function(df, start_date, end_date) {
  period_df <- filter_period_years(df, start_date, end_date)

  if (nrow(period_df) == 0) return(NULL)

  tibble(
    n_years = nrow(period_df),
    year_min = min(period_df$year, na.rm = TRUE),
    year_max = max(period_df$year, na.rm = TRUE),
    mean_annual = mean(period_df$value, na.rm = TRUE)
  )
}

# -----------------------------
# Aridity index = P / PET
# -----------------------------
calc_aridity_index <- function(precip_df, pet_df, start_date, end_date) {
  p_sub <- filter_period_years(precip_df, start_date, end_date)
  pet_sub <- filter_period_years(pet_df, start_date, end_date)

  joined <- inner_join(
    p_sub %>% select(year, precip = value),
    pet_sub %>% select(year, pet = value),
    by = "year"
  )

  if (nrow(joined) == 0) return(NULL)

  p_mean <- mean(joined$precip, na.rm = TRUE)
  pet_mean <- mean(joined$pet, na.rm = TRUE)

  tibble(
    n_years = nrow(joined),
    year_min = min(joined$year, na.rm = TRUE),
    year_max = max(joined$year, na.rm = TRUE),
    P_mean_annual = p_mean,
    PET_mean_annual = pet_mean,
    AI = ifelse(is.na(pet_mean) || pet_mean == 0, NA_real_, p_mean / pet_mean)
  )
}

# --------------------------------
# Runoff coefficient = R / P
# --------------------------------
calc_runoff_coefficient <- function(runoff_df, precip_df, start_date, end_date) {
  r_sub <- filter_period_years(runoff_df, start_date, end_date)
  p_sub <- filter_period_years(precip_df, start_date, end_date)

  joined <- inner_join(
    r_sub %>% select(year, runoff = value),
    p_sub %>% select(year, precip = value),
    by = "year"
  )

  if (nrow(joined) == 0) return(NULL)

  r_mean <- mean(joined$runoff, na.rm = TRUE)
  p_mean <- mean(joined$precip, na.rm = TRUE)

  tibble(
    n_years = nrow(joined),
    year_min = min(joined$year, na.rm = TRUE),
    year_max = max(joined$year, na.rm = TRUE),
    R_mean_annual = r_mean,
    P_mean_annual = p_mean,
    eta = ifelse(is.na(p_mean) || p_mean == 0, NA_real_, r_mean / p_mean)
  )
}

# ----------------------------------------
# SSP vs historical change for one metric
# ----------------------------------------
calc_change_vs_historical <- function(df, value_col) {
  value_sym <- rlang::sym(value_col)

  hist_df <- df %>%
    filter(period == "historical_1990-2014") %>%
    select(scenario, gcm, historical_value = !!value_sym)

  fut_df <- df %>%
    filter(period != "historical_1990-2014") %>%
    select(scenario, gcm, period, future_value = !!value_sym)

  fut_df %>%
    left_join(hist_df %>% select(gcm, historical_value), by = "gcm") %>%
    mutate(
      change_abs = future_value - historical_value,
      change_pct = ifelse(!is.na(historical_value) & historical_value != 0,
                          100 * change_abs / historical_value,
                          NA_real_)
    )
}
