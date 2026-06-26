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

Outputs are usually saved to a SharePoint-synced SOPI releases folder outside the project.
The app organises them by release and sector using editable path templates, for example:

```text
<SOPI releases root>/2026/June/Graphs/Seafood/seafood_fig_1.svg
```

## Normal Workflow

For the Shiny workflow:

1. Run the local Shiny app.
2. Set up the SOPI release, local SOPI releases root, and path templates in `Overview`.
3. Select or load the chart data in `Data`.
4. Set common chart appearance in `General Style`.
5. Select the plot function and chart-specific arguments in `Chart`.
6. Refresh the chart preview in `Chart`.
7. Set the SVG export width and height in `Chart`.
8. Save the confirmed SVG and update the release `chart_config.xlsx`.

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

Chart outputs are usually saved like this:

```text
<SOPI releases root>/
  2026/
    June/
      Graphs/
        Seafood/
        Meat and Wool/
        Forestry/
    December/
      Graphs/
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

The app has four tabs:

`Overview`
: Set release, sector, the local SOPI releases root, and the folder templates for graph outputs and manual Excel data. This tab shows the resolved chart output folder, lists SVGs already available for the selected release and sector, previews selected outputs, and allows unwanted output files to be deleted.

`Data`
: Choose an R data function or Excel sheet and preview the loaded data. For Excel sources, the app defaults to the release/sector workbook and lists the workbook sheets as a dropdown.

`General Style`
: Set common chart style settings such as font family and base font size. These settings apply across chart types.

`Chart`
: Choose the plot function, forecast settings, fields, and chart-specific arguments. This tab also contains `Refresh Preview`, the chart visual, editable graph output folder template, SVG filename, resolved save path, SVG export width and height, and the save/update config controls.

The local SOPI releases root should be the synced SharePoint folder that contains release folders such as `2026/June`. Each user can have a different local sync path. This local path is used by the app, but it is not written into the shared `chart_config.xlsx`.

The default graph output folder template is:

```text
{year}/{release}/Graphs/{sector}
```

With local root `SOPI_releases`, year `2026`, release `June`, and sector `Macro`, this resolves to:

```text
SOPI_releases/2026/June/Graphs/Macro
```

The shared config stores the portable equivalent:

```text
{SOPI_RELEASES_ROOT}/{year}/{release}/Graphs/{sector}
```

To make the app default to your SharePoint-synced folder every time it starts, add `SOPI_RELEASES_ROOT` to your existing `.Renviron` file in the project root.

Use `~` to make the user folder dynamic. R expands `~` to the current user's home folder:

```text
SOPI_RELEASES_ROOT="~/SharePoint/SOPI_releases"
```

On Windows you can also use:

```text
SOPI_RELEASES_ROOT="${USERPROFILE}/SharePoint/SOPI_releases"
```

The real `.Renviron` file is ignored by Git because it is different for each user.

`run_shiny_app.R`, `run_charts.R`, and `run_release_report.R` all load `.Renviron` automatically. The Shiny app also sets `SOPI_RELEASES_ROOT` for the current R session when a user changes `Local SOPI releases root` in the Overview tab.

Manual Excel data should normally live in a SharePoint-synced data folder, organised as one workbook per sector and release:

```text
<SOPI releases root>/
  2026/
    June/
      Data/
        Macro/
          Macro.xlsx
        Seafood/
          Seafood.xlsx
        Forestry/
          Forestry.xlsx
        Dairy/
          Dairy.xlsx
        Meat and Wool/
          Meat and Wool.xlsx
        Horticulture/
          Horticulture.xlsx
        Arable/
          Arable.xlsx
        Other foods/
          Other foods.xlsx
```

The default manual data workbook template is:

```text
{year}/{release}/Data/{sector}/{sector}.xlsx
```

If the folder structure changes later, update the templates in `Overview`; the rest of the app will use the new resolved paths. The Excel sheet selector is populated from the selected workbook.

When `Update release chart_config.xlsx` is ticked, the Shiny app writes to:

```text
config/releases/<release year>/<June or December>/chart_config.xlsx
```

The app updates existing `plot_id` and `data_source_id` rows when those IDs already exist. New IDs are appended. The workbook can then be used by `run_charts.R` to regenerate the same chart later.

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
$env:SOPI_RELEASES_ROOT = "$HOME\SharePoint\SOPI_releases"
Rscript run_charts.R config/releases/2026/June/chart_config.xlsx
```

After the SVGs have been generated, build a release figure report with Quarto:

```sh
Rscript run_release_report.R 2026 June
```

By default, the report runner uses the same local releases root as the app:

```text
~/Documents/outputs/SOPI_releases
```

If your SharePoint-synced release folder is somewhere else, pass it as the third argument:

```sh
Rscript run_release_report.R 2026 June "~/SharePoint/SOPI_releases"
```

Quarto must be installed and available on `PATH` to render the HTML file. If Quarto is not available, the project still writes the `.qmd` report file in the release `Report` folder.

The report is written beside the release `Graphs` folder:

```text
<SOPI releases root>/2026/June/Report/sopi_release_figures.qmd
<SOPI releases root>/2026/June/Report/sopi_release_figures.html
```

The report includes only SVG files that exist in the release output folders. It starts a new page for each sector and places the sector figures in config order with captions. Each figure block also includes a small table of the plot parameters used to create the figure.

You can also pass the config path directly:

```sh
Rscript run_release_report.R config/releases/2026/June/chart_config.xlsx
```

With a direct config path, pass a custom releases root as the second argument:

```sh
Rscript run_release_report.R config/releases/2026/June/chart_config.xlsx "~/SharePoint/SOPI_releases"
```

If there is only one release config under `config/releases/`, this also works:

```sh
Rscript run_release_report.R
```

If the report says no SVG files were found, check the paths it expects from R:

```r
source("R/08_report.R")
diagnose_release_report_files("config/releases/2026/June/chart_config.xlsx")
```

You can also run a release-specific config from R:

```r
Sys.setenv(SOPI_RELEASES_ROOT = "~/SharePoint/SOPI_releases")
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

The Shiny app also lets users create or reuse custom palettes while building a chart. Custom palettes are saved in the same workbook, on the `custom_palettes` sheet, using these columns:

```text
palette | item | hex | sector | notes | updated_at
```

When a chart uses a saved custom palette, the app writes the palette name into the chart arguments and copies the palette rows into that release's `chart_config.xlsx`. This keeps the release config runnable, while the metadata workbook remains the shared palette library.

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

The formal plotting contract is documented here:

```text
docs/SOPI_PLOTTING_PROTOCOL.md
```

Add new plot functions here:

```text
R/plot_functions/
```

A plot function should:

- accept `data` as the first argument
- be named `plot_*`
- use column names passed as character strings
- use standard field arguments such as `x`, `y`, `y_col`, `y_line`, `group`, `driver`, and `total`
- use standard label arguments such as `x_label`, `y_label`, `y_col_label`, `y_line_label`, `col_label`, `line_label`, and `labels`
- use standard colour arguments such as `palette`, `palette_fill`, and `palette_line`
- use `family` and `base_size` for typography
- return a ggplot object
- not save files itself

Supported legacy aliases include `date_var`, `y_lab`, `fontsize`, `fill_palette`, and `colour_palette`, but new functions should use the standard names.

To check plot function compatibility from R:

```r
source("R/00_packages.R")
source("R/01_utils.R")
source("R/02_styling.R")
source("R/07_plot_protocol.R")
source_directory("R/plot_functions")

sopi_plot_protocol_report()
```

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
  "forcats",
  "ggplot2",
  "lubridate",
  "openxlsx",
  "readxl",
  "rlang",
  "scales",
  "shiny",
  "svglite",
  "tibble",
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
