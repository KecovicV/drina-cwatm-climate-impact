suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# -----------------------------
# Paths
# -----------------------------
indicators_dir <- "results/indicators"
compare_dir <- "results/compare"
fig_base_dir <- "results/figures/climate"

fig_abs_dir <- file.path(fig_base_dir, "absolute")
fig_chg_dir <- file.path(fig_base_dir, "change")
fig_rel_dir <- file.path(fig_base_dir, "relationships")

dir.create(fig_abs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_chg_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_rel_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Color palettes
# -----------------------------
scenario_colors_all <- c(
  historical = "#2F2F2F",
  ssp126 = "#1f78b4",
  ssp245 = "#ff8c00",
  ssp585 = "#d73027"
)

scenario_colors_future <- c(
  ssp126 = "#1f78b4",
  ssp245 = "#ff8c00",
  ssp585 = "#d73027"
)

# -----------------------------
# Load data
# -----------------------------
ai_tbl <- read_csv(file.path(indicators_dir, "AI_aridity_index.csv"), show_col_types = FALSE)
eta_tbl <- read_csv(file.path(indicators_dir, "runoff_coefficient_eta.csv"), show_col_types = FALSE)
changes_tbl <- read_csv(file.path(compare_dir, "climate_changes.csv"), show_col_types = FALSE)

# -----------------------------
# Helper functions
# -----------------------------
make_absolute_plot <- function(df, value_col, title_text, y_label, out_file) {
  val_sym <- rlang::sym(value_col)

  plot_df <- df %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c("historical", "ssp126", "ssp245", "ssp585")
      )
    )

  p <- ggplot(plot_df, aes(x = scenario, y = !!val_sym, fill = scenario, color = scenario)) +
    geom_boxplot(alpha = 0.25, width = 0.55, outlier.shape = NA) +
    geom_jitter(width = 0.12, size = 2.8, alpha = 0.95) +
    scale_fill_manual(values = scenario_colors_all, drop = FALSE) +
    scale_color_manual(values = scenario_colors_all, drop = FALSE) +
    labs(
      title = title_text,
      x = NULL,
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(face = "bold")
    )

  ggsave(out_file, p, width = 9, height = 6, dpi = 300)
}

make_change_plot <- function(df, indicator_name, title_text, y_label, out_file) {
  plot_df <- df %>%
    filter(indicator == indicator_name) %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c("ssp126", "ssp245", "ssp585")
      )
    )

  p <- ggplot(plot_df, aes(x = scenario, y = change_pct, fill = scenario, color = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.6) +
    geom_boxplot(alpha = 0.25, width = 0.55, outlier.shape = NA) +
    geom_jitter(width = 0.12, size = 2.8, alpha = 0.95) +
    scale_fill_manual(values = scenario_colors_future, drop = FALSE) +
    scale_color_manual(values = scenario_colors_future, drop = FALSE) +
    labs(
      title = title_text,
      x = NULL,
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(face = "bold")
    )

  ggsave(out_file, p, width = 9, height = 6, dpi = 300)
}

# -----------------------------
# Absolute plots
# -----------------------------
make_absolute_plot(
  df = ai_tbl,
  value_col = "AI",
  title_text = "Aridity Index (AI) by scenario",
  y_label = "AI [-]",
  out_file = file.path(fig_abs_dir, "AI_absolute.png")
)

make_absolute_plot(
  df = ai_tbl,
  value_col = "P_mean_annual",
  title_text = "Mean annual precipitation by scenario",
  y_label = "P [m]",
  out_file = file.path(fig_abs_dir, "P_mean_annual_absolute.png")
)

make_absolute_plot(
  df = eta_tbl,
  value_col = "eta",
  title_text = "Runoff coefficient by scenario",
  y_label = expression(eta ~ " [-]"),
  out_file = file.path(fig_abs_dir, "eta_absolute.png")
)

# -----------------------------
# Relative change plots
# -----------------------------
make_change_plot(
  df = changes_tbl,
  indicator_name = "AI",
  title_text = "Relative change of aridity index",
  y_label = expression(Delta * "AI [%]"),
  out_file = file.path(fig_chg_dir, "AI_change_pct.png")
)

make_change_plot(
  df = changes_tbl,
  indicator_name = "P_mean_annual",
  title_text = "Relative change of mean annual precipitation",
  y_label = expression(Delta * "P [%]"),
  out_file = file.path(fig_chg_dir, "P_mean_annual_change_pct.png")
)

make_change_plot(
  df = changes_tbl,
  indicator_name = "eta",
  title_text = "Relative change of runoff coefficient",
  y_label = expression(Delta * eta ~ " [%]"),
  out_file = file.path(fig_chg_dir, "eta_change_pct.png")
)

# -----------------------------
# Relationship plots
# -----------------------------
delta_p <- changes_tbl %>%
  filter(indicator == "P_mean_annual") %>%
  select(scenario, gcm, delta_p = change_pct)

delta_ai <- changes_tbl %>%
  filter(indicator == "AI") %>%
  select(scenario, gcm, delta_ai = change_pct)

delta_eta <- changes_tbl %>%
  filter(indicator == "eta") %>%
  select(scenario, gcm, delta_eta = change_pct)

plot_df_ai <- delta_p %>%
  inner_join(delta_ai, by = c("scenario", "gcm")) %>%
  filter(!is.na(scenario)) %>%
  mutate(scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585")))

plot_df_eta <- delta_p %>%
  inner_join(delta_eta, by = c("scenario", "gcm")) %>%
  filter(!is.na(scenario)) %>%
  mutate(scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585")))

p_ai <- ggplot(plot_df_ai, aes(x = delta_p, y = delta_ai, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 3.5, alpha = 0.95) +
  scale_color_manual(values = scenario_colors_future, drop = FALSE) +
  labs(
    title = "Climate response relationship: ΔP vs ΔAI",
    x = "ΔP [%]",
    y = "ΔAI [%]",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(fig_rel_dir, "deltaP_vs_deltaAI.png"),
  plot = p_ai,
  width = 10,
  height = 7,
  dpi = 300
)

p_eta <- ggplot(plot_df_eta, aes(x = delta_p, y = delta_eta, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 3.5, alpha = 0.95) +
  scale_color_manual(values = scenario_colors_future, drop = FALSE) +
  labs(
    title = "Climate response relationship: ΔP vs Δη",
    x = "ΔP [%]",
    y = "Δη [%]",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(fig_rel_dir, "deltaP_vs_deltaEta.png"),
  plot = p_eta,
  width = 10,
  height = 7,
  dpi = 300
)

message("Climate plots generated in: ", fig_base_dir)
