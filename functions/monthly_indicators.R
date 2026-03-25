suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

find_monthavg_file <- function(run_path) {
  candidates <- c(
    file.path(run_path, "discharge_monthavg.csv"),
    file.path(run_path, "discharge_monthavg")
  )

  for (f in candidates) {
    if (file.exists(f)) return(f)
  }

  fallback <- list.files(run_path, pattern = "^discharge_monthavg(\\..+)?$", full.names = TRUE)
  if (length(fallback) > 0) return(fallback[[1]])

  NA_character_
}

read_cwatm_monthavg <- function(path) {
  if (!file.exists(path)) {
    stop("Monthly discharge file not found: ", path)
  }

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
      "Expected columns not found in monthly discharge file: ",
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
    stop("No valid rows parsed from monthly discharge file: ", path)
  }

  out
}

calc_iafd_wide <- function(df, start_date, end_date) {
  period_df <- df %>%
    filter(date >= start_date, date <= end_date)

  if (nrow(period_df) == 0) {
    return(NULL)
  }

  monthly <- period_df %>%
    mutate(month = as.integer(format(date, "%m"))) %>%
    group_by(month) %>%
    summarise(value = median(discharge, na.rm = TRUE), .groups = "drop") %>%
    complete(month = 1:12) %>%
    arrange(month)

  vals <- monthly$value
  names(vals) <- sprintf("IAFD_%02d", 1:12)

  tibble::as_tibble(as.list(vals))
}
