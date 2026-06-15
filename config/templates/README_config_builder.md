# AutoSOPI Config Builder Prototype

This folder contains a first VBA-based approach for making chart configuration easier for non-R users.

## Files

`config_builder_template.xlsx`
: Friendly workbook layout. Users edit sheets such as `Release Setup`, `Charts`, `Data Sources`, `Data Args`, `Sector Defaults`, `Chart Defaults`, and `Run Control`.

`vba/ConfigBuilder.bas`
: VBA module that builds the technical config sheets used by R and exports a release-specific `chart_config.xlsx`.

## Setup

1. Open `config_builder_template.xlsx`.
2. Save it as `config_builder.xlsm`.
3. Press `Alt + F11` to open the VBA editor.
4. Use `File > Import File...` and import `vba/ConfigBuilder.bas`.
5. Save the workbook.
6. Run the macro `InstallBuilderButtons` once.

After that, the `START HERE` sheet will have buttons for:

- `Build R Config`
- `Export Chart Config`

## User Workflow

1. Edit `Release Setup`.
2. Add one row per chart in `Charts`.
3. Add any Excel or function data sources in `Data Sources`.
4. Add function arguments in `Data Args` where needed.
5. Use `Sector Defaults` and `Chart Defaults` to avoid repeating settings.
6. Click `Build R Config`.
7. Check `Validation`.
8. Click `Export Chart Config`.

The export macro writes:

```text
config/releases/<release_year>/<release_round>/chart_config.xlsx
```

For example:

```text
config/releases/2026/June/chart_config.xlsx
```

## Running A Generated Config

From R:

```r
Sys.setenv(AUTOSOPI_CONFIG = "config/releases/2026/June/chart_config.xlsx")
source("run_charts.R")
```

Or from a terminal:

```sh
Rscript run_charts.R config/releases/2026/June/chart_config.xlsx
```

The old default still works:

```r
source("run_charts.R")
```

That uses `config/chart_config.xlsx`.
