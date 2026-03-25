suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(purrr)
})

find_daily_file <- function(run_path, preferred_name = "discharge_daily.csv") {
  f1 <- file.path(run_path, preferred_name)
  f2 <- file.path(run_path, sub("\\.csv$", "", preferred_name))
  if (file.exists(f1)) return(f1)
  if (file.exists(f2)) return(f2)

  cand <- list.files(run_path, pattern = "^discharge_daily(\\.csv)?$", full.names = TRUE)
  if (length(cand) > 0) return(cand[1])

  NA_character_
}

read_cwatm_daily_fdc <- function(path) {
  if (!file.exists(path)) {
    stop("Discharge file not found: ", path)
  }

  df <- readr::read_csv(
    file = path,
    skip = 3,
    col_select = 1:2,
    show_col_types = FALSE,
    trim_ws = TRUE,
    name_repair = "minimal"
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

calc_fdc_5points <- function(df, start_date, end_date) {
  period_df <- df %>% filter(date >= start_date, date <= end_date)
  if (nrow(period_df) == 0) return(NULL)

  qs <- as.numeric(stats::quantile(
    period_df$discharge,
    probs = c(0, 0.25, 0.50, 0.75, 1),
    na.rm = TRUE,
    names = FALSE
  ))
  names(qs) <- c("Min", "FDC_75", "FDC_50", "FDC_25", "Max")

  tibble(
    n_days = nrow(period_df),
    date_min = min(period_df$date, na.rm = TRUE),
    date_max = max(period_df$date, na.rm = TRUE),
    Min = qs[["Min"]],
    FDC_75 = qs[["FDC_75"]],
    FDC_50 = qs[["FDC_50"]],
    FDC_25 = qs[["FDC_25"]],
    Max = qs[["Max"]]
  )
}

yearly_fdc_interp <- function(df_year, t_grid = 0:100) {
  Qs <- sort(df_year$discharge, decreasing = TRUE)
  n <- length(Qs)
  if (n < 10) return(NULL)

  i <- seq_len(n)
  t_i <- (i / (n + 1)) * 100
  Qg <- approx(x = t_i, y = Qs, xout = t_grid, rule = 2)$y

  tibble(t_pct = t_grid, Q = Qg)
}

fdc_years_for_period <- function(df, start_date, end_date, t_grid = 0:100) {
  period_df <- df %>% filter(date >= start_date, date <= end_date)
  if (nrow(period_df) == 0) return(NULL)

  period_df <- period_df %>% mutate(year = as.integer(format(date, "%Y")))
  years <- sort(unique(period_df$year))

  out <- purrr::map(years, function(yy) {
    dfy <- period_df %>% filter(year == yy)
    fdc_y <- yearly_fdc_interp(dfy, t_grid = t_grid)
    if (is.null(fdc_y)) return(NULL)
    fdc_y %>% mutate(year = yy, .before = 1)
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)

  bind_rows(out)
}

calc_fdc_gcm_curve <- function(df, start_date, end_date, t_grid = 0:100) {
  ytab <- fdc_years_for_period(df, start_date, end_date, t_grid = t_grid)
  if (is.null(ytab) || nrow(ytab) == 0) return(NULL)

  ytab %>%
    group_by(t_pct) %>%
    summarise(
      Q = median(Q, na.rm = TRUE),
      n_years = dplyr::n_distinct(year),
      .groups = "drop"
    )
}

calc_fdc_ensemble_intervals <- function(fdc_gcm_tbl) {
  if (nrow(fdc_gcm_tbl) == 0) return(tibble())

  fdc_gcm_tbl %>%
    group_by(scenario, period, t_pct) %>%
    summarise(
      n_gcm = dplyr::n_distinct(gcm),
      Q_min = min(Q, na.rm = TRUE),
      Q_p25 = as.numeric(stats::quantile(Q, probs = 0.25, na.rm = TRUE, names = FALSE)),
      Q_med = median(Q, na.rm = TRUE),
      Q_p75 = as.numeric(stats::quantile(Q, probs = 0.75, na.rm = TRUE, names = FALSE)),
      Q_max = max(Q, na.rm = TRUE),
      .groups = "drop"
    )
}
