suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

message("Starting Drina climate impact workflow...")

source("scripts/02_build_reference_thresholds.R")
source("scripts/03_calculate_discharge_indicators.R")
source("scripts/04_calculate_monthly_indicators.R")
source("scripts/05_calculate_climate_indicators.R")
source("scripts/06_calculate_fdc_metrics.R")
source("scripts/07_compare_scenarios.R")
source("scripts/08_plot_hydrological_indicators.R")
source("scripts/09_plot_climate_indicators.R")
source("scripts/10_plot_fdc_iafd.R")

message("Workflow completed successfully.")
