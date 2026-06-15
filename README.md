# AutoSOPI Chart Builder

AutoSOPI is an R-based chart generation project for building SOPI charts as SVG files. It is designed for primary industry reporting and for use from a SharePoint-synced folder, without Shiny, servers, or a database.

The project separates chart code from chart configuration. R functions generate the charts, while Excel workbooks define the release, sectors, data sources, plot settings, function arguments, colours, and output locations.

## What This Project Does

AutoSOPI helps analysts generate repeatable SOPI chart outputs for sectors such as:

- Macro
- Seafood
- Forestry
- Dairy
- Meat and Wool
- Horticulture
- Arable
- other primary sector categories as needed

Each SOPI release can contain multiple sector charts. A chart can use data from an Excel sheet or from an R data-preparation function.

Outputs are written as SVG files to:

```text
outputs/<release_year>/<release_round>/<sector>/
```

For example:

```text
outputs/2026/June/Seafood/seafood_fig_1.svg
```

## Current Workflow

The current workflow is:

1. Keep the project in a SharePoint-synced folder.
2. Define the SOPI release and charts in an Excel config workbook.
3. Let R read the config workbook.
4. Let R pull data from Excel sheets or R data functions.
5. Let R apply SOPI styling and metadata-driven colours.
6. Save SVG chart outputs into the release output folder.

Run the default config from R:

```r
source("run_charts.R")
```

By default this reads:

```text
config/chart_config.xlsx
```

Run a release-specific config:

```r
Sys.setenv(AUTOSOPI_CONFIG = "config/releases/2026/June/chart_config.xlsx")
source("run_charts.R")
```

or from a terminal:

```sh
Rscript run_charts.R config/releases/2026/June/chart_config.xlsx
```

## Project Structure

```text
config/
  chart_config.xlsx
    Current technical config workbook read by R.

  templates/
    config_builder_template.xlsx
      Prototype user-friendly config builder workbook.

    README_config_builder.md
      Notes for using the config builder prototype.

    vba/ConfigBuilder.bas
      VBA module that builds technical config sheets and exports release configs.

    vba/frmChartWizard.frm
      UserForm for adding charts through dropdowns and buttons.

    vba/frmDataSourceWizard.frm
      UserForm for adding Excel or R-function data sources.

data/
  raw/
    Excel data sources used by configured charts.

metadata/
  sopi_metadata.xlsx
    Sector, forecast-group, label, and colour metadata.

R/
  00_packages.R
  01_utils.R
  02_styling.R
  03_config.R
  04_data_sources.R
  05_outputs.R
  06_runner.R

  data_functions/
    R functions that generate or prepare chart data.

  plot_functions/
    R functions that return ggplot chart objects.

outputs/
  <release_year>/<release_round>/<sector>/
    Generated SVG chart outputs.

run_charts.R
  Main entry point for running configured charts.
```

## Config Options

There are two ways to manage chart configuration.

### Technical Config

The file `config/chart_config.xlsx` is the machine-readable workbook used by R. It contains these sheets:

`release_settings`
: SOPI release-level defaults such as release year, release round, forecast years, output root, font, width, and height.

`settings_sector`
: Sector-level defaults such as active flag, palette, and output subfolder.

`plots`
: One row per chart output. Defines sector, plot function, data source, output filename, title, subtitle, and sort order.

`data_sources`
: Defines whether each dataset comes from an Excel sheet or an R function.

`plot_args`
: Long-format arguments passed to plot functions.

`data_args`
: Long-format arguments passed to data-preparation functions.

`run_control`
: Optional filters for running selected sectors or plot IDs.

`palettes`
: Named manual palettes that can be referenced from settings.

### User-Friendly Builder Prototype

The file `config/templates/config_builder_template.xlsx` is a prototype for a friendlier Excel interface. The idea is that non-R users edit simple sheets such as:

- `Release Setup`
- `Charts`
- `Data Sources`
- `Data Args`
- `Sector Defaults`
- `Chart Defaults`
- `Run Control`

The VBA files in `config/templates/vba/` then create clickable forms, build the technical config sheets, and export:

```text
config/releases/<release_year>/<release_round>/chart_config.xlsx
```

To try the prototype:

1. Open `config/templates/config_builder_template.xlsx`.
2. Save it as `config_builder.xlsm`.
3. Open the VBA editor with `Alt + F11`.
4. Import these VBA files:
   - `config/templates/vba/ConfigBuilder.bas`
   - `config/templates/vba/frmChartWizard.frm`
   - `config/templates/vba/frmDataSourceWizard.frm`
5. Save the workbook.
6. Run `InstallBuilderButtons`.
7. Click `Add Data Source` to register Excel sheets or R data functions.
8. Click `Add Chart` to select sector, plot type, data source, fields, labels, forecast settings, and palette settings.
9. Click `Build R Config`.
10. Click `Export Chart Config`.

If a `.frm` file fails to import with "form class ... is not supported in VBE", use the latest files in `config/templates/vba/`. The form files must start with Excel's UserForm class GUID, not `VB.Form`.

Do not paste `.frm` contents into a code module. Import `.frm` files using `File > Import File...`; otherwise Excel tries to compile form layout text such as `VERSION 5.00` as VBA code.

## Release Organisation

SOPI releases happen twice a year, in June and December. The recommended structure is one generated config per release:

```text
config/releases/
  2026/
    June/
      chart_config.xlsx
    December/
      chart_config.xlsx
```

Outputs follow the same release logic:

```text
outputs/
  2026/
    June/
      Seafood/
      Meat and Wool/
      Forestry/
    December/
      Seafood/
      Meat and Wool/
      Forestry/
```

## Metadata And Colours

Shared SOPI metadata lives in:

```text
metadata/sopi_metadata.xlsx
```

The runner loads this once per run and uses it as a shared styling resource. Plot functions should not read this workbook directly. Instead, the runner injects common style arguments such as:

- `palette`
- `palette_fill`
- `palette_line`
- `labels`

For sector/category charts, AutoSOPI tries to use metadata-driven forecast-group colours. If the `forecast_palette` sheet only provides one repeated colour for a sector, the styling helper falls back to the distinct `forecast_group_color` values from the `metadata` sheet.

If a plotted category is missing from metadata, the palette is completed with ggplot hue colours so the chart can still render.

## Inheritance Rules

Settings are resolved from broad to specific:

```text
release_settings
  -> settings_sector
    -> plots
      -> plot_args / data_args
```

More specific settings override broader settings.

For example, release-level `forecast_start_year` and `forecast_end_year` are mapped internally to plot-function arguments `forecast_start` and `forecast_end`. Only add plot-specific forecast years when a chart is an exception to the release default.

## Adding A New Chart

1. Add or reuse a data source in `data_sources`.
2. Add one row in `plots`.
3. Add the required rows in `plot_args`.
4. If the data source is an R function, add required rows in `data_args`.
5. Set `active = TRUE`.
6. Run `source("run_charts.R")`.

If using the builder prototype, add the chart to the `Charts` sheet instead and let VBA generate the technical sheets.

## Adding A Plot Function

Create a new `.R` file in:

```text
R/plot_functions/
```

Plot functions should:

- accept `data` as the first argument
- use string column names, for example `.data[[x]]`
- accept shared styling arguments where relevant, especially `palette`, `palette_fill`, `palette_line`, and `labels`
- return a ggplot object
- not save files themselves

Example:

```r
plot_my_chart <- function(data, x, y, group = NULL, palette = NULL, ...) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_col()
}
```

## Adding A Data Function

Create a new `.R` file in:

```text
R/data_functions/
```

Data functions should:

- return a data frame
- accept `...` so shared settings do not break them
- avoid writing files unless caching is intentionally added

Example:

```r
prep_my_data <- function(sector, historical_start_year, historical_end_year, ...) {
  data.frame(
    sector = sector,
    year = historical_start_year:historical_end_year,
    value = seq_along(historical_start_year:historical_end_year)
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

## SharePoint Notes

- Keep paths relative to the project root.
- Store project files, config workbooks, metadata, source data, and outputs in SharePoint.
- Use one release config per SOPI year and round.
- Avoid per-user output folders unless the team later needs that separation.
- If two users run the same release at the same time, the latest saved SVG will overwrite the previous one.
- Let SharePoint version history handle workbook and output history where possible.
