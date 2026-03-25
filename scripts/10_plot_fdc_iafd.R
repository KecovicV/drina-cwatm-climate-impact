suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(tibble)
})

# --------------------------------------------------
# Paths
# --------------------------------------------------
monthly_path <- "results/indicators/iafd_12_monthly_medians.csv"
fdc_path <- "results/fdc/fdc_ensemble_intervals.csv"

fig_base <- "results/figures"
fig_iafd_dir <- file.path(fig_base, "iafd")
fig_fdc_dir  <- file.path(fig_base, "fdc")

dir.create(fig_iafd_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_fdc_dir, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------
# Scenario labels and titles
# --------------------------------------------------
scenario_labels <- c(
  historical = "historical",
  ssp126 = "ssp1.2.6",
  ssp245 = "ssp2.4.5",
  ssp585 = "ssp5.8.5"
)

scenario_titles_iafd <- c(
  historical = "Intra-annual flow distribution (IAFD) of monthly discharge\nhistorical",
  ssp126 = "Intra-annual flow distribution (IAFD) of monthly discharge\nssp1.2.6",
  ssp245 = "Intra-annual flow distribution (IAFD) of monthly discharge\nssp2.4.5",
  ssp585 = "Intra-annual flow distribution (IAFD) of monthly discharge\nssp5.8.5"
)

scenario_titles_fdc <- c(
  historical = "Flow Duration Curve intervals – Historical (1990–2014)",
  ssp126 = "Flow Duration Curve intervals – SSP1-2.6 (2031–2060)",
  ssp245 = "Flow Duration Curve intervals – SSP2-4.5 (2031–2060)",
  ssp585 = "Flow Duration Curve intervals – SSP5-8.5 (2031–2060)"
)

# --------------------------------------------------
# Plot colors
# --------------------------------------------------
iafd_dark       <- "#0b2e7a"
iafd_light      <- "#00a6ff"
fdc_med_col     <- "#ef6a5b"
fdc_iqr_fill    <- "#7ed3d7"
fdc_minmax_fill <- "#f1d7d3"

# --------------------------------------------------
# IAFD data preparation
# --------------------------------------------------
iafd_raw <- read_csv(monthly_path, show_col_types = FALSE)

iafd_long <- iafd_raw %>%
  pivot_longer(
    cols = starts_with("IAFD_"),
    names_to = "month_name",
    values_to = "Q"
  ) %>%
  mutate(
    month = as.integer(gsub("IAFD_", "", month_name)),
    scenario = as.character(scenario)
  )

iafd_summary <- iafd_long %>%
  group_by(scenario, month) %>%
  summarise(
    Qmedian = median(Q, na.rm = TRUE),
    Q25p = quantile(Q, 0.25, na.rm = TRUE, names = FALSE),
    Q75p = quantile(Q, 0.75, na.rm = TRUE, names = FALSE),
    Qmin = min(Q, na.rm = TRUE),
    Qmax = max(Q, na.rm = TRUE),
    .groups = "drop"
  )

global_iafd_ymax <- max(iafd_summary$Qmax, na.rm = TRUE) * 1.08

# --------------------------------------------------
# Smooth IAFD helpers
# --------------------------------------------------
smooth_months <- seq(1, 12, length.out = 200)

smooth_series <- function(df, value_col, span = 0.6) {
  model_data <- df %>%
    select(month, value = all_of(value_col)) %>%
    filter(!is.na(value))

  if (nrow(model_data) < 4) {
    return(rep(NA_real_, length(smooth_months)))
  }

  fit <- loess(
    value ~ month,
    data = model_data,
    span = span,
    control = loess.control(surface = "direct")
  )

  as.numeric(predict(fit, newdata = data.frame(month = smooth_months)))
}

smooth_iafd <- function(df, span = 0.6) {
  out <- tibble(month = smooth_months) %>%
    mutate(
      Qmedian = smooth_series(df, "Qmedian", span = span),
      Q25p    = smooth_series(df, "Q25p", span = span),
      Q75p    = smooth_series(df, "Q75p", span = span),
      Qmin    = smooth_series(df, "Qmin", span = span),
      Qmax    = smooth_series(df, "Qmax", span = span)
    ) %>%
    tidyr::drop_na()

  if (nrow(out) == 0) {
    out <- df %>%
      transmute(
        month,
        Qmedian,
        Q25p,
        Q75p,
        Qmin,
        Qmax
      )
  }

  out
}

# --------------------------------------------------
# IAFD plots
# --------------------------------------------------
for (sc in unique(iafd_summary$scenario)) {
  raw_df <- iafd_summary %>% filter(scenario == sc)
  plot_df <- smooth_iafd(raw_df, span = 0.6)

  p <- ggplot(plot_df, aes(x = month)) +
    geom_ribbon(aes(ymin = Qmin, ymax = Qmax), fill = "grey70", alpha = 0.18) +
    geom_ribbon(aes(ymin = Q25p, ymax = Q75p), fill = "grey50", alpha = 0.18) +
    geom_line(aes(y = Qmedian, color = "IAFDmedian"), linewidth = 1.6, lineend = "round") +
    geom_line(aes(y = Q25p, color = "IAFD25p"), linewidth = 0.9, linetype = "dotted", lineend = "round") +
    geom_line(aes(y = Q75p, color = "IAFD75p"), linewidth = 0.9, linetype = "dotted", lineend = "round") +
    geom_line(aes(y = Qmin, color = "IAFDmin"), linewidth = 0.9, lineend = "round") +
    geom_line(aes(y = Qmax, color = "IAFDmax"), linewidth = 0.9, lineend = "round") +
    scale_x_continuous(breaks = 0:12, limits = c(0, 12)) +
    scale_y_continuous(
      limits = c(0, global_iafd_ymax),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_color_manual(
      values = c(
        IAFDmedian = iafd_dark,
        IAFD25p = iafd_light,
        IAFD75p = iafd_light,
        IAFDmin = iafd_dark,
        IAFDmax = iafd_dark
      ),
      breaks = c("IAFDmedian", "IAFD25p", "IAFD75p", "IAFDmin", "IAFDmax"),
      labels = c(
        paste0("IAFDmedian,", scenario_labels[[sc]]),
        paste0("IAFD25p,", scenario_labels[[sc]]),
        paste0("IAFD75p,", scenario_labels[[sc]]),
        paste0("IAFDmin,", scenario_labels[[sc]]),
        paste0("IAFDmax,", scenario_labels[[sc]])
      ),
      name = NULL
    ) +
    labs(
      title = scenario_titles_iafd[[sc]],
      x = "t [month]",
      y = expression(Q ~ "[" * m^3/s * "]")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 11)
    )

  ggsave(
    filename = file.path(fig_iafd_dir, paste0("iafd_", sc, ".png")),
    plot = p,
    width = 10,
    height = 6.2,
    dpi = 300
  )
}

# --------------------------------------------------
# FDC data preparation
# --------------------------------------------------
fdc_raw <- read_csv(fdc_path, show_col_types = FALSE) %>%
  mutate(scenario = as.character(scenario))

candidate_x <- c("t", "exceedance", "exceedance_probability", "probability", "p")
x_col <- candidate_x[candidate_x %in% names(fdc_raw)][1]

if (is.na(x_col) || length(x_col) == 0) {
  non_q_cols <- setdiff(
    names(fdc_raw),
    c("scenario", "period", "Q_min", "Q_p25", "Q_med", "Q_p75", "Q_max", "source_file")
  )
  x_col <- non_q_cols[1]
}

if (is.na(x_col) || length(x_col) == 0) {
  stop("Could not determine FDC x-axis column in fdc_ensemble_intervals.csv")
}

fdc_raw <- fdc_raw %>%
  rename(exceedance = all_of(x_col))

# --------------------------------------------------
# FDC plots
# --------------------------------------------------
for (sc in unique(fdc_raw$scenario)) {
  plot_df <- fdc_raw %>% filter(scenario == sc)

  p <- ggplot(plot_df, aes(x = exceedance)) +
    geom_ribbon(aes(ymin = Q_min, ymax = Q_max, fill = "Min–Max"), alpha = 0.55) +
    geom_ribbon(aes(ymin = Q_p25, ymax = Q_p75, fill = "P25–P75 (IQR)"), alpha = 0.75) +
    geom_line(aes(y = Q_med, color = "Median"), linewidth = 1.15) +
    scale_fill_manual(
      values = c(
        "Min–Max" = fdc_minmax_fill,
        "P25–P75 (IQR)" = fdc_iqr_fill
      ),
      name = "Ensemble range"
    ) +
    scale_color_manual(
      values = c("Median" = fdc_med_col),
      name = "Statistic"
    ) +
    labs(
      title = scenario_titles_fdc[[sc]],
      x = "Exceedance probability, t [%]",
      y = expression("Discharge, Q [" * m^3/s * "]")
    ) +
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.3)),
      fill = guide_legend(order = 2)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 11)
    )

  ggsave(
    filename = file.path(fig_fdc_dir, paste0("fdc_", sc, ".png")),
    plot = p,
    width = 10,
    height = 6.2,
    dpi = 300
  )
}

message("Saved IAFD plots to: ", fig_iafd_dir)
message("Saved FDC plots to: ", fig_fdc_dir)
