#!/usr/bin/env Rscript

# Generates a png of the METR.org plot in the website style. 
# An additional manual step (not shown here) is required to convert the png to a .webp.

suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "metr-ggplot.png"

# Community page palette.
bg_page <- "#151314"
text_heading <- "#fffdfa"
text_primary <- "#e8e4df"
text_muted <- "#9e9591"
text_dim <- "#6e6764"
accent <- "#ff3366"
non_frontier <- "#8b8481"

# This is a March 3, 2026 snapshot of the METR Time Horizon 1.1 linear 50%
# chart data from https://metr.org/time-horizons/ plus the display names from
# their current chart script. The fit below mirrors their live chart logic:
# frontier models only, exponential regression, x-range ending 3 months after
# the latest release.
models <- data.frame(
  sort_order = seq_len(24),
  model = c(
    "GPT-2",
    "GPT-3",
    "GPT-3.5",
    "GPT-4",
    "GPT-4 Nov '23",
    "Claude 3 Opus",
    "GPT-4 Turbo",
    "GPT-4o",
    "Claude 3.5 Sonnet (Old)",
    "o1-preview",
    "Claude 3.5 Sonnet (New)",
    "o1",
    "Claude 3.7 Sonnet",
    "o3",
    "Claude Opus 4",
    "Claude Opus 4.1",
    "GPT-5",
    "Gemini 3 Pro",
    "GPT-5.1-Codex-Max",
    "Claude Opus 4.5",
    "GPT-5.2 (high)",
    "Claude Opus 4.6",
    "GPT-5.3-Codex (high)",
    "GPT-5.4 (xhigh)"
  ),
  release_date = as.Date(c(
    "2019-02-14",
    "2020-05-28",
    "2022-03-15",
    "2023-03-14",
    "2023-11-06",
    "2024-03-04",
    "2024-04-09",
    "2024-05-13",
    "2024-06-20",
    "2024-09-12",
    "2024-10-22",
    "2024-12-05",
    "2025-02-24",
    "2025-04-16",
    "2025-05-22",
    "2025-08-05",
    "2025-08-07",
    "2025-11-18",
    "2025-11-19",
    "2025-11-24",
    "2025-12-11",
    "2026-02-05",
    "2026-02-05",
    "2026-03-05"
  )),
  estimate_minutes = c(
    0.053778,
    0.144057,
    0.599247,
    3.987428,
    4.044959,
    3.952262,
    3.732787,
    6.991195,
    11.395377,
    20.326586,
    20.522872,
    38.831588,
    60.388937,
    119.732634,
    100.366123,
    100.472004,
    203.012577,
    224.325884,
    223.714694,
    292.994594,
    352.249302,
    718.806830,
    349.530732,
    341.735276
  ),
  ci_low_minutes = c(
    0.009994,
    0.093319,
    0.255148,
    1.932920,
    1.866859,
    1.706313,
    1.980046,
    4.001482,
    5.489734,
    11.716193,
    10.144026,
    21.164512,
    33.006168,
    74.615398,
    59.994073,
    59.272391,
    112.641357,
    139.565157,
    134.310770,
    161.717714,
    198.067494,
    316.685725,
    194.913134,
    186.581591
  ),
  ci_high_minutes = c(
    0.141825,
    0.221209,
    1.115905,
    7.995283,
    8.443226,
    8.764840,
    6.736613,
    12.905741,
    22.384214,
    33.379877,
    40.820280,
    64.952490,
    104.226017,
    190.943818,
    163.454074,
    159.451918,
    405.551565,
    379.235434,
    396.206723,
    623.704698,
    815.177445,
    3633.786163,
    816.351994,
    768.779526
  ),
  stringsAsFactors = FALSE
)

models <- models[order(models$release_date, models$sort_order), ]
models$estimate_hours <- models$estimate_minutes / 60
models$ci_low_hours <- models$ci_low_minutes / 60
models$ci_high_hours <- models$ci_high_minutes / 60

previous_best <- c(-Inf, head(cummax(models$estimate_hours), -1))
models$frontier <- models$estimate_hours > previous_best
models$point_colour <- ifelse(models$frontier, accent, non_frontier)
models$error_colour <- ifelse(models$frontier, accent, non_frontier)

fit_anchor <- as.Date("2020-01-01")
frontier_models <- subset(models, frontier)
frontier_models$x_years <- as.numeric(frontier_models$release_date - fit_anchor) / 365.25
fit <- lm(log(estimate_hours) ~ x_years, data = frontier_models)

trend_start <- as.Date("2019-01-01")
trend_end <- seq(max(models$release_date), by = "3 months", length.out = 2)[2]
trend_days <- seq(
  from = as.numeric(trend_start),
  to = as.numeric(trend_end),
  length.out = 121
)
trend_df <- data.frame(
  release_date = as.Date(trend_days, origin = "1970-01-01")
)
trend_df$x_years <- as.numeric(trend_df$release_date - fit_anchor) / 365.25
trend_df$estimate_hours <- exp(predict(fit, newdata = trend_df))

first_frontier_date <- min(frontier_models$release_date)
last_frontier_date <- max(frontier_models$release_date)
task_lines <- data.frame(
  x = as.Date(rep("2019-04-01", 4)),
  y = c(8.07, 4.00, 2.30, 1.20),
  label = c(
    "Exploit a vulnerable Ethereum smart contract",
    "Train adversarially robust image model",
    "Exploit a buffer overflow in libiec61850",
    "Fix bugs in small Python libraries"
  ),
  stringsAsFactors = FALSE
)

label_specs <- data.frame(
  model = c(
    "GPT-2",
    "GPT-3",
    "GPT-3.5",
    "GPT-4",
    "o3",
    "GPT-5",
    "Claude Opus 4.5",
    "GPT-5.2 (high)",
    "Claude Opus 4.6",
    "GPT-5.4 (xhigh)"
  ),
  label = c(
    "GPT-2",
    "GPT-3",
    "GPT-3.5",
    "GPT-4",
    "o3",
    "GPT-5",
    "Claude Opus 4.5",
    "GPT-5.2 (high)",
    "Claude Opus 4.6",
    "GPT-5.4\n(xhigh)"
  ),
  nudge_days = c(35, 40, 32, 24, -16, -20, -26, -26, -30, 34),
  nudge_hours = c(0.14, 0.14, 0.14, 0.16, 0.02, 0.04, 0.00, 0.02, 0.02, -0.02),
  hjust = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0),
  stringsAsFactors = FALSE
)
labels <- merge(models, label_specs, by = "model")
labels$label_date <- labels$release_date + labels$nudge_days
labels$label_y <- labels$estimate_hours + labels$nudge_hours

max_value <- max(models$estimate_hours)
y_upper <- max_value * 1.1
axis_breaks <- c(0, 0.5, seq(1, ceiling(max_value), by = 1))
grid_breaks <- seq(0.5, ceiling(max_value), by = 0.5)

format_hour_label <- function(values) {
  vapply(values, function(value) {
    if (abs(value) < 1e-9) {
      "0"
    } else if (abs(value - 0.5) < 1e-9) {
      "30 min"
    } else if (abs(value - 1) < 1e-9) {
      "1 hour"
    } else {
      paste0(format(value, trim = TRUE, scientific = FALSE), " hours")
    }
  }, character(1))
}

year_breaks <- as.Date(sprintf("%d-01-01", 2020:format(max(models$release_date), "%Y")))

base_plot <- ggplot(models, aes(x = release_date, y = estimate_hours)) +
  geom_hline(
    yintercept = grid_breaks,
    colour = text_dim,
    linewidth = 0.28,
    alpha = 0.22
  ) +
  geom_vline(
    xintercept = year_breaks,
    colour = text_dim,
    linewidth = 0.28,
    alpha = 0.18
  ) +
  geom_text(
    data = task_lines,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    colour = text_muted,
    alpha = 0.72,
    size = 3.35,
    family = "sans"
  ) +
  geom_errorbar(
    aes(ymin = ci_low_hours, ymax = ci_high_hours, colour = I(error_colour)),
    width = 12,
    linewidth = 0.45,
    alpha = 0.22,
    show.legend = FALSE
  ) +
  geom_line(
    data = trend_df,
    aes(x = release_date, y = estimate_hours),
    inherit.aes = FALSE,
    colour = accent,
    linewidth = 0.85,
    linetype = "22",
    alpha = 0.62
  ) +
  geom_point(
    aes(fill = I(point_colour)),
    shape = 21,
    size = 3.6,
    stroke = 0.35,
    colour = bg_page,
    show.legend = FALSE
  ) +
  geom_text(
    data = labels,
    aes(
      x = label_date,
      y = label_y,
      label = label,
      hjust = hjust
    ),
    inherit.aes = FALSE,
    colour = text_heading,
    size = 3.4,
    family = "sans",
    fontface = "bold",
    lineheight = 0.92
  ) +
  scale_x_date(
    limits = c(as.Date("2019-01-01"), trend_end),
    breaks = year_breaks,
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = axis_breaks,
    labels = format_hour_label(axis_breaks),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Time horizon of software tasks\ndifferent LLMs can complete 50% of the time",
    x = "LLM release date",
    y = paste(
      "Task duration (for humans)",
      "where logistic regression of our data",
      "predicts the AI has a 50% chance of succeeding",
      sep = "\n"
    )
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    plot.title = element_text(
      colour = text_heading,
      size = 17,
      face = "bold",
      hjust = 0.5,
      lineheight = 1.05,
      margin = margin(b = 14)
    ),
    axis.line.x = element_line(colour = text_muted, linewidth = 0.45),
    axis.line.y = element_line(colour = text_muted, linewidth = 0.45),
    axis.text.x = element_text(
      colour = text_primary,
      size = 11,
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(colour = text_muted, size = 10),
    axis.title.x = element_text(
      colour = text_primary,
      size = 12,
      margin = margin(t = 14)
    ),
    axis.title.y = element_text(
      colour = text_primary,
      size = 11,
      lineheight = 1.05,
      margin = margin(r = 16)
    ),
    plot.margin = margin(12, 72, 18, 12)
  )

ggsave(
  filename = output_path,
  plot = base_plot,
  width = 12.6,
  height = 7.2,
  dpi = 220,
  bg = "transparent"
)

message("Saved chart to ", output_path)
