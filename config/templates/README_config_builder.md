# SOPI Graphs Config Builder

This workbook helps you create SOPI chart configuration files without editing R code.

Use it when you want to say:

- which SOPI release you are working on
- which sectors are included
- where the chart data comes from
- which charts should be produced
- where the final SVG files should be saved

## One-Time Setup

Do this once when creating a new macro-enabled builder workbook.

1. Open `config_builder_template.xlsx`.
2. Save it as `config_builder.xlsm`.
3. Press `Alt + F11`.
4. In the VBA editor, choose `File > Import File...`.
5. Import this file only:
   - `vba/ConfigBuilder.bas`
6. Save the workbook.
7. Run the macro called `InstallBuilderButtons`.

After this, go back to the `START HERE` sheet. You should see these buttons:

- `Add Chart`
- `Add Data Source`
- `Refresh Plot Functions`
- `Refresh Data Functions`
- `Build R Config`
- `Export Chart Config`

## Normal Use

Use this process for each SOPI release.

1. Open the builder workbook.
2. Go to `Release Setup`.
3. Set the release year, release round, forecast years, output folder, and chart size.
4. Go back to `START HERE`.
5. Click `Refresh Plot Functions`.
6. Click `Refresh Data Functions`.
7. Click `Add Data Source` for each dataset you need.
8. Click `Add Chart` for each chart you want.
9. Click `Build R Config`.
10. Check the `Validation` sheet.
11. If there are no errors, click `Export Chart Config`.

The exported file will be saved here:

```text
config/releases/<year>/<June or December>/chart_config.xlsx
```

For example:

```text
config/releases/2026/June/chart_config.xlsx
```

## Adding A Data Source

Click `Add Data Source`.

Choose one of these source types:

`excel`
: Use this when the data is in an Excel workbook. The builder asks for the workbook path, then shows the available sheet names so you can select one.

`function`
: Use this when R creates or prepares the data. The builder shows the available R data functions from the project.

For R function data sources, the builder can also suggest the chart fields if the R function has a roxygen-style field line, for example:

```r
#' @sopi_fields year, group, revenue, volume
prep_my_data <- function(...) {
  ...
}
```

After adding or changing this line, click `Refresh Data Functions`.

Use clear names for data sources, for example:

```text
seafood_fig_1_data
meat_wool_exports
forestry_ranked_markets
```

## Adding A Chart

Click `Add Chart`.

The builder will ask you to select or enter:

- sector
- plot ID
- plot function
- data source
- output SVG filename
- x/date/year field
- group/category field, if needed
- column value field, if needed
- line value field, if needed
- axis labels
- break settings

When choosing the data source, the builder lists the data sources already added in the `Data Sources` sheet.

## What The Refresh Buttons Do

Click `Refresh Plot Functions` when new chart functions have been added to:

```text
R/plot_functions/
```

Click `Refresh Data Functions` when new data functions have been added to:

```text
R/data_functions/
```

These buttons update the dropdown lists used by the builder. `Refresh Data Functions` also reads any `@sopi_fields` lines and stores the known output fields in the `Lists` sheet.

## If Something Goes Wrong

If Excel says `Invalid outside procedure`, check whether a form file was pasted or imported into the wrong place.

In the VBA editor:

1. Look at the project list on the left.
2. Delete any failed form modules called `frmChartWizard` or `frmDataSourceWizard`.
3. Delete any normal module that starts with lines like `VERSION 5.00` or `Begin {C62A69F0...`.
4. Import `vba/ConfigBuilder.bas` only.
5. Run `Debug > Compile VBAProject`.

The current builder does not need `.frm` files. It uses the buttons installed by `ConfigBuilder.bas`.

If Excel asks how to import the module, use:

```text
File > Import File...
```

Do not copy and paste code manually. Import the `.bas` file.

If you see this text inside a normal module:

```text
VERSION 5.00
Begin {C62A69F0...
```

delete that module. It is form layout text, not normal VBA code.

## Running The Charts

After exporting the config, run the charts from R:

```r
Sys.setenv(AUTOSOPI_CONFIG = "config/releases/2026/June/chart_config.xlsx")
source("run_charts.R")
```

Change the year and release round to match the config you exported.
