# AutoSOPI Config Builder Prototype

This folder contains a first VBA-based approach for making chart configuration easier for non-R users.

## Files

`config_builder_template.xlsx`
: Friendly workbook layout. Users edit sheets such as `Release Setup`, `Charts`, `Data Sources`, `Data Args`, `Sector Defaults`, `Chart Defaults`, and `Run Control`.

`vba/ConfigBuilder.bas`
: VBA module that builds the technical config sheets used by R and exports a release-specific `chart_config.xlsx`.

`vba/frmChartWizard.frm`
: UserForm for adding a chart by clicking through sector, data source, plot function, data fields, labels, forecast settings, and palette settings.

`vba/frmDataSourceWizard.frm`
: UserForm for adding Excel-backed or R-function-backed data sources.

## Setup

1. Open `config_builder_template.xlsx`.
2. Save it as `config_builder.xlsm`.
3. Press `Alt + F11` to open the VBA editor.
4. Use `File > Import File...` and import these files:
   - `vba/ConfigBuilder.bas`
   - `vba/frmChartWizard.frm`
   - `vba/frmDataSourceWizard.frm`
5. Save the workbook.
6. Run the macro `InstallBuilderButtons` once.

Do not copy and paste the text from a `.frm` file into a code module. The `.frm` file contains both form layout text and VBA code, so it must be imported as a file through `File > Import File...`.

After that, the `START HERE` sheet will have buttons for:

- `Add Chart`
- `Add Data Source`
- `Build R Config`
- `Export Chart Config`

## User Workflow

1. Edit `Release Setup`.
2. Click `Add Data Source` if the data source is not already listed.
3. Click `Add Chart`.
4. Select the sector, plot function, and data source.
5. Click `Load Fields` to populate the field dropdowns from the selected Excel data source.
6. Select the x field, group field, column value, line value, labels, and chart options.
7. Click `Save Chart`.
8. Click `Build R Config`.
9. Check `Validation`.
10. Click `Export Chart Config`.

You can still edit the friendly sheets directly if that is faster for bulk updates.

## Troubleshooting VBA Imports

If Excel says:

```text
The form class contained in ... is not supported in VBE. The file can't be loaded.
```

make sure you are importing the latest `.frm` files from this folder. Excel VBA UserForms must start with the UserForm class GUID:

```text
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F}
```

Older draft form files used a `VB.Form` declaration, which Excel VBE rejects.

If the code window shows lines such as `VERSION 5.00` or `Begin {C62A69F0...}`, the form was pasted into a module instead of imported. Remove that pasted module and import the `.frm` file instead.

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
