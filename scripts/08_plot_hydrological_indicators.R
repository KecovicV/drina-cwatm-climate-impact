suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# -----------------------------
# Paths and style
# -----------------------------
abs_path <- "results/indicators/discharge_indicators_absolute.csv"
chg_path <- "results/compare/discharge_changes.csv"

out_abs_dir <- "results/figures/hydrology/absolute"
out_chg_dir <- "results/figures/hydrology/change"
dir.create(out_abs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_chg_dir, recursive = TRUE, showWarnings = FALSE)

scenario_colors <- c(
  historical = "#2F2F2F",
  ssp126 = "#1f78b4",
  ssp245 = "#ff8c00",
  ssp585 = "#d73027"
)

pretty_labels <- c(
  historical = "Historical",
  ssp126 = "SSP1-2.6",
  ssp245 = "SSP2-4.5",
  ssp585 = "SSP5-8.5"
)

indicator_titles <- c(
  Q50 = "Median discharge (Q50)",
  Q5 = "Low-flow percentile (Q5)",
  Q95 = "High-flow percentile (Q95)",
  LFF = "Low Flow Frequency (LFF)",
  HFF = "High Flow Frequency (HFF)"
)

y_labels <- c(
  Q50 = expression(Q[50]~"[m"^3*"/s]"),
  Q5 = expression(Q[5]~"[m"^3*"/s]"),
  Q95 = expression(Q[95]~"[m"^3*"/s]"),
  LFF = "LFF [% of days]",
  HFF = "HFF [% of days]"
)

base_theme <- function() {
  theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# -----------------------------
# Absolute-value plots
# -----------------------------
abs_df <- read_csv(abs_path, show_col_types = FALSE) %>%
  mutate(
    scenario = factor(scenario, levels = c("historical", "ssp126", "ssp245", "ssp585"), labels = pretty_labels[c("historical", "ssp126", "ssp245", "ssp585")])
  )

for (ind in c("Q50", "Q5", "Q95", "LFF", "HFF")) {
  p <- ggplot(abs_df, aes(x = scenario, y = .data[[ind]], fill = scenario, color = scenario)) +
    geom_boxplot(width = 0.65, alpha = 0.35, outlier.shape = NA) +
    geom_jitter(width = 0.08, size = 2.4, alpha = 0.95) +
    scale_fill_manual(values = unname(scenario_colors[c("historical", "ssp126", "ssp245", "ssp585")])) +
    scale_color_manual(values = unname(scenario_colors[c("historical", "ssp126", "ssp245", "ssp585")])) +
    labs(
      title = paste0(indicator_titles[[ind]], " by scenario"),
      x = NULL,
      y = y_labels[[ind]]
    ) +
    base_theme()

  ggsave(
    filename = file.path(out_abs_dir, paste0(ind, "_absolute.png")),
    plot = p, width = 8.5, height = 5.4, dpi = 300
  )
}

# -----------------------------
# Relative-change plots
# -----------------------------
chg_df <- read_csv(chg_path, show_col_types = FALSE) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"), labels = pretty_labels[c("ssp126", "ssp245", "ssp585")])
  )

for (ind in c("Q50", "Q5", "Q95", "LFF", "HFF")) {
  df_i <- chg_df %>% filter(indicator == ind)

  p <- ggplot(df_i, aes(x = scenario, y = change_pct, fill = scenario, color = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey35") +
    geom_boxplot(width = 0.65, alpha = 0.35, outlier.shape = NA) +
    geom_jitter(width = 0.08, size = 2.4, alpha = 0.95) +
    scale_fill_manual(values = unname(scenario_colors[c("ssp126", "ssp245", "ssp585")])) +
    scale_color_manual(values = unname(scenario_colors[c("ssp126", "ssp245", "ssp585")])) +
    labs(
      title = paste0(indicator_titles[[ind]], " change relative to historical"),
      x = NULL,
      y = "Change [%]"
    ) +
    base_theme()

  ggsave(
    filename = file.path(out_chg_dir, paste0(ind, "_change_pct.png")),
    plot = p, width = 8.5, height = 5.4, dpi = 300
  )
}

message("Hydrological plots written to: ", normalizePath("results/figures/hydrology", winslash = "/", mustWork = FALSE))
