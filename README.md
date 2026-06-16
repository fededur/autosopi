# SOPI Graphs

SOPI Graphs is a small R project for creating SOPI charts as SVG files.

It is designed to work from a SharePoint-synced folder. The Shiny app runs locally on the user's machine; there is no server and no database.

Most users can control charts from either the Shiny app or the Excel config builder. R creates the final SVG files.

## What It Is For

Use SOPI Graphs to create repeatable charts for SOPI sector reporting, including:

- Macro
- Seafood
- Forestry
- Dairy
- Meat and Wool
- Horticulture
- Arable
- other sectors as needed

Each SOPI release can have several charts per sector.

Data can come from:

- an Excel sheet
- an R data function

Outputs are usually saved to a SharePoint-synced output folder outside the project.
The app still organises them by release and sector, for example:

```text
<SharePoint output folder>/2026/June/Seafood/seafood_fig_1.svg
```

## Normal Workflow

For the Shiny workflow:

1. Run the local Shiny app.
2. Set up the SOPI release and output folder in `Overview`.
3. Select or load the chart data and data period in `Data`.
4. Set common chart appearance in `General Style`.
5. Select the plot function and chart-specific arguments in `Chart`.
6. Preview the chart.
7. Save the confirmed SVG.

For the Excel config workflow:

1. Open the config builder workbook.
2. Set up the SOPI release, for example `2026` and `June`.
3. Add the data sources.
4. Add the charts.
5. Build the R config.
6. Export the release config.
7. Run the charts from R.
8. Use the SVG files from the `outputs` folder.

The detailed builder instructions are here:

```text
config/templates/README_config_builder.md
```

## Release Folders

SOPI releases happen twice a year: June and December.

Configs are saved like this:

```text
config/releases/
  2026/
    June/
      chart_config.xlsx
    December/
      chart_config.xlsx
```

Chart outputs are saved like this:

```text
<SharePoint output folder>/
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

## Main Folders

`config/`
: Stores chart config workbooks. The release-specific configs live in `config/releases/`.

`config/templates/`
: Stores the Excel config builder template and VBA module.

`data/raw/`
: Stores Excel data files used by charts.

`metadata/`
: Stores shared SOPI metadata such as sector categories, labels, and colours.

`R/`
: Stores the R code that reads configs, prepares data, applies styling, and creates charts.

`R/data_functions/`
: Stores R functions that create or prepare chart data.

`R/plot_functions/`
: Stores R functions that create charts.

`outputs/`
: Stores the generated SVG chart files.

## Using The Config Builder

The config builder is the easiest way to make or update chart config files.

To try it:

1. Open `config/templates/config_builder_template.xlsx`.
2. Save it as `config_builder.xlsm`.
3. Import `config/templates/vba/ConfigBuilder.bas`.
4. Run `InstallBuilderButtons`.
5. Use the buttons on the `START HERE` sheet.

The main buttons are:

- `Add Data Source`
- `Add Chart`
- `Delete Data Source`
- `Delete Chart`
- `Refresh Plot Functions`
- `Refresh Data Functions`
- `Build R Config`
- `Export Chart Config`
- `Refresh Plot List`

The builder can list:

- plot functions from `R/plot_functions/`
- data functions from `R/data_functions/`
- sheet names from selected Excel workbooks
- existing data sources from the `Data Sources` sheet

When adding a chart, the sector is selected from the standard SOPI sector list: Macro, Dairy, Meat and Wool, Forestry, Horticulture, Seafood, Arable, and Other foods.

Adding a chart or data source appends a new row unless the ID already exists. If the ID exists, the builder asks before replacing that row. Delete buttons remove unwanted rows from the friendly sheets before the release config is rebuilt and exported.

`Refresh Plot List` writes a current plot summary by sector onto the `START HERE` sheet.

## Using The Shiny App

Run the app from the project root:

```r
source("run_shiny_app.R")
```

The app has five tabs:

`Overview`
: Set release, sector, output file, and the SharePoint output base folder. This tab shows the resolved chart output folder, lists SVGs already available for the selected release and sector, previews selected outputs, and allows unwanted output files to be deleted.

`Data`
: Choose an R data function or Excel sheet, set the data period, and preview the loaded data.

`General Style`
: Set common chart style settings such as font family and base font size. These settings apply across chart types.

`Chart`
: Choose the plot function, forecast settings, fields, and chart-specific arguments.

`Preview And Save`
: Review the chart, set the SVG export width and height, and save the confirmed SVG.

The output base folder should normally be the synced SharePoint folder where final SOPI SVGs are stored. Users paste the full folder path into `SharePoint output base folder`. SOPI Graphs then appends release year, release round, and sector to create the current chart output folder. The output file list on `Overview` lets users inspect existing outputs in that folder. SVG files can be previewed in the app. A selected output file can be deleted after ticking the delete confirmation box.

## Running The Charts

Run the default config from R:

```r
source("run_charts.R")
```

This uses:

```text
config/chart_config.xlsx
```

To run a release-specific config from a terminal:

```sh
Rscript run_charts.R config/releases/2026/June/chart_config.xlsx
```

You can also run a release-specific config from R:

```r
Sys.setenv(AUTOSOPI_CONFIG = "config/releases/2026/June/chart_config.xlsx")
source("run_charts.R")
```

## Colours And Labels

Shared chart metadata lives here:

```text
metadata/sopi_metadata.xlsx
```

This metadata can provide:

- sector names
- forecast groups
- chart labels
- colour palettes

The chart functions should use the metadata colours and labels where possible. If a category does not have a colour in the metadata, the project fills in extra colours so the chart can still be created.

## Adding A New Chart

For normal use, add charts through the config builder.

The basic steps are:

1. Add or reuse a data source.
2. Add a chart.
3. Choose the plot function.
4. Choose the fields used by the chart.
5. Build and export the config.
6. Run the charts.

## For R Users

Add new plot functions here:

```text
R/plot_functions/
```

A plot function should:

- accept `data` as the first argument
- use column names passed as text
- accept shared styling arguments such as `palette`, `palette_fill`, `palette_line`, and `labels`
- return a ggplot object
- not save files itself

Add new data functions here:

```text
R/data_functions/
```

A data function should:

- return a data frame
- accept `...`
- document its output columns with `@sopi_fields`
- avoid writing files unless caching is intentional

Example:

```r
#' Prepare my chart data
#'
#' @sopi_fields year, group, revenue, volume
prep_my_data <- function(...) {
  data.frame(year = 2026, group = "Example", revenue = 1, volume = 1)
}
```

After adding new R functions, open the builder workbook and click the relevant refresh button.

## Required R Packages

Install these once on each user's machine:

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "readxl",
  "rlang",
  "scales",
  "shiny",
  "svglite",
  "tidyr"
))
```

## SharePoint Notes

- Store the project, configs, metadata, and data in SharePoint.
- Store final chart outputs in a SharePoint-synced output folder selected in the Shiny app.
- The output folder can be outside the R project folder.
- Use one config per SOPI year and release round.
- Avoid separate user folders unless the team later needs them.
- If two users run the same release at the same time, the latest saved SVG will overwrite the previous one.
- Use SharePoint version history for workbook and output history.
