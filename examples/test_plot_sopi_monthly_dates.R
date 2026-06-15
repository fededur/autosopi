library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(svglite)

source(file.path("R", "02_styling.R"))
source(file.path("R", "plot_functions", "plot_sopi.R"))

set.seed(123)

seafood_palette <- palette_from_forecast_metadata(
  project_root = getwd(),
  sector = "Seafood",
  ref = "key",
  fill = "actual"
)

seafood_categories <- names(seafood_palette)
seafood_categories <- seafood_categories[seq_len(min(3, length(seafood_categories)))]

monthly_dates <- seq(as.Date("2022-01-01"), as.Date("2025-12-01"), by = "1 month")

monthly_data <- expand.grid(
  date = monthly_dates,
  product = seafood_categories,
  stringsAsFactors = FALSE
) |>
  arrange(date, product) |>
  mutate(
    month_no = as.integer(format(date, "%m")),
    year_no = as.integer(format(date, "%Y")),
    trend = (year_no - min(year_no)) * 12 + month_no,
    seasonal = sin(2 * pi * month_no / 12),
    export_value_nzd = case_when(
      product == seafood_categories[1] ~ 85 + trend * 1.2 + seasonal * 9 + rnorm(n(), 0, 4),
      product == seafood_categories[2] ~ 45 + trend * 0.8 + seasonal * 5 + rnorm(n(), 0, 3),
      product == seafood_categories[3] ~ 60 + trend * 1.0 + seasonal * 7 + rnorm(n(), 0, 3.5)
    ) * 1000000,
    export_volume_tonnes = case_when(
      product == seafood_categories[1] ~ 18000 + trend * 180 + seasonal * 1800 + rnorm(n(), 0, 700),
      product == seafood_categories[2] ~ 9500 + trend * 90 + seasonal * 900 + rnorm(n(), 0, 450),
      product == seafood_categories[3] ~ 13000 + trend * 130 + seasonal * 1300 + rnorm(n(), 0, 550)
    )
  ) |>
  select(date, product, export_value_nzd, export_volume_tonnes)

seafood_palette <- complete_palette(unique(monthly_data$product), seafood_palette)

p_monthly <- plot_sopi(
  data = monthly_data,
  x = "date",
  x_freq = "monthly",
  y_line = "export_value_nzd",
  y_col = "export_volume_tonnes",
  group = "product",
  y_line_label = "Export revenue (NZ$)",
  y_col_label = "Export volume (tonnes)",
  palette = seafood_palette,
  primary_min_breaks = 4,
  primary_max_breaks = 6,
  secondary_min_breaks = 4,
  secondary_max_breaks = 6,
  col_position = "stacked"
)

p_monthly_forecast <- plot_sopi(
  data = monthly_data,
  x = "date",
  x_freq = "monthly",
  y_line = "export_value_nzd",
  y_col = "export_volume_tonnes",
  group = "product",
  y_line_label = "Export revenue (NZ$)",
  y_col_label = "Export volume (tonnes)",
  palette_line = seafood_palette,
  palette_fill = scales::alpha(seafood_palette, 0.6),
  primary_min_breaks = 3,
  primary_max_breaks = 5,
  secondary_min_breaks = 5,
  secondary_max_breaks = 7,
  forecast = TRUE,
  forecast_start = as.Date("2025-01-01"),
  forecast_end = as.Date("2025-12-01"),
  forecast_label = "Forecast",
  col_position = "stacked"
)

print(p_monthly)
print(p_monthly_forecast)

dir.create(file.path("outputs", "Monthly Date Tests"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path("outputs", "Monthly Date Tests", "plot_sopi_monthly_dates.svg"),
  plot = p_monthly,
  width = 11,
  height = 6,
  device = svglite::svglite
)

ggsave(
  filename = file.path("outputs", "Monthly Date Tests", "plot_sopi_monthly_dates_forecast.svg"),
  plot = p_monthly_forecast,
  width = 11,
  height = 6,
  device = svglite::svglite
)
