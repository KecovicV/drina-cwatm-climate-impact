suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})

# -----------------------------
# Paths
# -----------------------------
discharge_path <- "results/indicators/discharge_indicators_absolute.csv"
climate_ai_path <- "results/indicators/AI_aridity_index.csv"
climate_eta_path <- "results/indicators/runoff_coefficient_eta.csv"
fdc_path <- "results/fdc/fdc_5points_by_run_period.csv"

out_dir <- "results/compare"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Helper: compare SSP vs historical baseline by GCM
# -----------------------------
compare_vs_hist <- function(df, value_col, indicator_name) {
  
  value_sym <- rlang::sym(value_col)
  
  hist_tbl <- df %>%
    filter(grepl("historical", period, ignore.case = TRUE)) %>%
    group_by(gcm) %>%
    summarise(hist = first(!!value_sym), .groups = "drop")
  
  fut_tbl <- df %>%
    filter(grepl("future", period, ignore.case = TRUE)) %>%
    transmute(
      scenario,
      gcm,
      future = !!value_sym
    )
  
  if (nrow(hist_tbl) == 0 || nrow(fut_tbl) == 0) {
    return(tibble())
  }
  
  fut_tbl %>%
    left_join(hist_tbl, by = "gcm") %>%
    filter(!is.na(hist), !is.na(future)) %>%
    mutate(
      indicator = indicator_name,
      change_abs = future - hist,
      change_pct = ifelse(hist == 0, NA_real_, 100 * (future - hist) / hist)
    ) %>%
    select(scenario, gcm, indicator, hist, future, change_abs, change_pct) %>%
    arrange(scenario, gcm)
}

# -----------------------------
# 1. DISCHARGE
# -----------------------------
discharge <- read_csv(discharge_path, show_col_types = FALSE)

discharge_tbl <- bind_rows(
  compare_vs_hist(discharge, "Q50", "Q50"),
  compare_vs_hist(discharge, "Q5", "Q5"),
  compare_vs_hist(discharge, "Q95", "Q95"),
  compare_vs_hist(discharge, "LFF", "LFF"),
  compare_vs_hist(discharge, "HFF", "HFF")
)

write_csv(discharge_tbl, file.path(out_dir, "discharge_changes.csv"))

# -----------------------------
# 2. CLIMATE
# -----------------------------
ai <- read_csv(climate_ai_path, show_col_types = FALSE)
eta <- read_csv(climate_eta_path, show_col_types = FALSE)

climate_tbl <- bind_rows(
  compare_vs_hist(ai, "AI", "AI"),
  compare_vs_hist(ai, "P_mean_annual", "P_mean_annual"),
  compare_vs_hist(eta, "eta", "eta")
)

write_csv(climate_tbl, file.path(out_dir, "climate_changes.csv"))

# -----------------------------
# 3. FDC
# -----------------------------
fdc <- read_csv(fdc_path, show_col_types = FALSE)

fdc_tbl <- bind_rows(
  compare_vs_hist(fdc, "FDC_50", "FDC_50"),
  compare_vs_hist(fdc, "FDC_25", "FDC_25"),
  compare_vs_hist(fdc, "FDC_75", "FDC_75"),
  compare_vs_hist(fdc, "Min", "Min"),
  compare_vs_hist(fdc, "Max", "Max")
)

write_csv(fdc_tbl, file.path(out_dir, "fdc_changes.csv"))

# -----------------------------
# FINAL MERGED TABLE
# -----------------------------
all_changes <- bind_rows(discharge_tbl, climate_tbl, fdc_tbl) %>%
  arrange(indicator, scenario, gcm)

write_csv(all_changes, file.path(out_dir, "all_indicators_changes.csv"))

print(all_changes, n = nrow(all_changes))