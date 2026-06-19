library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(svglite)

source(file.path("R", "02_styling.R"))
source(file.path("R", "data_functions", "to_delete","prep_fake_meat_wool_timeseries.R"))
#source(file.path("R", "plot_functions", "generic_ts_plot.R"))

meat_wool_data <- prep_fake_meat_wool_timeseries(
  historical_start_year = 2019,
  historical_end_year = 2030,
  project_root = getwd(),
  seed = 42
)

metadata_resource <- load_metadata_resource(getwd())

meat_wool_style <- style_from_metadata(
  metadata_resource = metadata_resource,
  sector = "Meat and Wool",
  categories = unique(meat_wool_data$group),
  ref = "key",
  fill = "actual"
)

p_meat_wool <- plot_generic_ts(
  data = meat_wool_data,
  x = "year",
  x_freq = "yearly",
  y_col = "revenue",
  y_line = "volume",
  group = "group",
  y_col_label = "Export revenue (NZ$ million)",
  y_line_label = "Export volume (tonnes)",
  col_label = "Revenue",
  line_label = "Volume",
  palette = meat_wool_style$palette,
  palette_fill = scales::alpha(meat_wool_style$palette, 0.65),
  palette_line = meat_wool_style$palette,
  labels = meat_wool_style$labels,
  forecast = TRUE,
  forecast_start = 2026,
  forecast_end = 2030,
  col_position = "stacked",
  primary_min_breaks = 4,
  primary_max_breaks = 6,
  secondary_min_breaks = 4,
  secondary_max_breaks = 6
)

print(p_meat_wool)

dir.create(file.path("outputs", "Example Tests", "Meat and Wool"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path("outputs", "Example Tests", "Meat and Wool", "meat_wool_generic_ts_plot.svg"),
  plot = p_meat_wool,
  width = 11,
  height = 6,
  device = svglite::svglite
)
