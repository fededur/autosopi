ola$0114
# SharePoint SVG Chart Framework

This is a small R-based framework for generating SVG charts from Excel configuration files. It is designed for SharePoint-synced folders and does not require a server or Shiny.

## Quick Start

1. Sync or copy this `chart-framework` folder from SharePoint to your computer.
2. Open `chart-framework.Rproj` if you use RStudio, or set your working directory to this folder.
3. Edit `config/chart_config.xlsx`.
4. Run:

```r
source("run_charts.R")
```

SVG files are written to `outputs/<sector>/`.

## Main Files

```text
config/chart_config.xlsx
  Main non-R control workbook.

data/raw/example_excel_data.xlsx
  Example Excel data source used by the sample config.

R/data_functions/
  R functions that prepare datasets.

R/plot_functions/
  R functions that return ggplot objects.

run_charts.R
  Main entry point.
```

## Config Workbook Sheets

`settings_global`
: Project-wide defaults such as output root, year range, palette, font, width, and height.

`settings_sector`
: Sector-level defaults. These override global settings.

`plots`
: One row per chart output. Defines the sector, plot function, data source, title, subtitle, and SVG filename.

`data_sources`
: Defines whether each dataset comes from Excel or an R function.

`plot_args`
: Arguments passed to the plot function. Use long format: `plot_id`, `arg_name`, `arg_value`, `arg_type`.

`data_args`
: Arguments passed to data-preparation functions.

`run_control`
: Optional filters for running only selected sectors or plot IDs.

`palettes`
: Named colour palettes that can be referenced from global, sector, or plot settings.

## Inheritance

Settings are resolved in this order:

```text
settings_global
  -> settings_sector
    -> plots
      -> plot_args / data_args
```

More specific settings override broader settings.

## Adding a New Plot

1. Add or reuse a data source in `data_sources`.
2. Add one row in `plots`.
3. Add the needed rows in `plot_args`.
4. If the data source is an R function, add any needed rows in `data_args`.
5. Set `active = TRUE`.
6. Run `source("run_charts.R")`.

## Adding a New Plot Function

Create a new `.R` file in `R/plot_functions/`.

The function should:

- accept `data` as its first argument
- use string column names, e.g. `.data[[x]]`
- return a ggplot object
- not save files itself

Example:

```r
plot_my_chart <- function(data, x, y, group = NULL, palette = NULL, ...) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_col()
}
```

## Adding a New Data Function

Create a new `.R` file in `R/data_functions/`.

The function should:

- return a data frame
- accept `...` so shared settings do not break it
- avoid writing files unless caching is intentionally added

Example:

```r
prep_my_data <- function(sector, year_start, year_end, ...) {
  data.frame(
    sector = sector,
    year = year_start:year_end,
    value = seq_along(year_start:year_end)
  )
}
```

## Required R Packages

Install these once on each user's machine:

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "readxl",
  "rlang",
  "scales",
  "svglite",
  "tidyr"
))
```

## Notes for SharePoint

- Keep paths relative to the project root.
- Do not use per-user output folders unless the team later needs them.
- Let SharePoint version history handle output file history where possible.
- If two users run at the same time, the latest saved SVG will overwrite the previous one.
