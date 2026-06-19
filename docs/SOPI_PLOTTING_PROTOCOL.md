# SOPI Plotting Protocol

This protocol defines the standard argument names and data expectations for plot
functions used by SOPI Graphs. New plot functions should follow this protocol so
the Shiny app can discover the right fields, build previews, save SVG files, and
write reusable `chart_config.xlsx` files.

## Function Shape

Every plot function should:

- live in `R/plot_functions/`
- be named `plot_*`
- accept `data` as the first argument
- return a `ggplot` object
- avoid saving files directly
- accept column names as character strings
- avoid hard-coded sector paths, output paths, or SharePoint paths

Recommended skeleton:

```r
plot_example <- function(
    data,
    x = NULL,
    y = NULL,
    y_col = NULL,
    y_line = NULL,
    group = NULL,
    x_freq = c("auto", "yearly", "quarterly", "monthly"),
    y_label = NULL,
    y_col_label = NULL,
    y_line_label = NULL,
    labels = NULL,
    palette = NULL,
    palette_fill = NULL,
    palette_line = NULL,
    forecast = FALSE,
    forecast_start = NULL,
    forecast_end = NULL,
    family = "DIN",
    base_size = 10.5,
    ...
) {
  x_freq <- match.arg(x_freq)
  # return ggplot2 object
}
```

## Standard Data Arguments

Use these names where possible:

| Argument | Meaning | Notes |
|---|---|---|
| `data` | Input data frame | Always first argument. |
| `x` | Main x-axis field | Date, month, quarter, year, or category. |
| `y` | Main y-value field | Use for one-measure charts. |
| `y_col` | Column/bar value field | Use for combo charts with bars and lines. |
| `y_line` | Line value field | Use for combo charts with bars and lines. |
| `group` | Category/group field | Optional. If omitted, plot should treat data as one series where possible. |
| `driver` | Driver/contribution category | For decomposition/contribution charts. |
| `total` | Total value field | For contribution charts requiring total context. |

Prefer character strings:

```r
plot_example(data, x = "date", y_col = "revenue", group = "sopi_forecast_group")
```

Avoid requiring unquoted column names in new functions. Existing functions may
still support tidy evaluation, but the app and config workflow work best with
strings.

## Standard Time Arguments

| Argument | Meaning |
|---|---|
| `x_freq` | `"auto"`, `"yearly"`, `"quarterly"`, or `"monthly"`. |
| `period_type` | `"calendar"` or `"financial"` where relevant. |
| `financial_start_month` | First month of financial year, default `7`. |
| `forecast` | Logical. Show forecast shading or forecast treatment. |
| `forecast_start` | Forecast start year/date. |
| `forecast_end` | Forecast end year/date. |

Do not use `year_start` or `year_end` for chart display logic. If a chart needs
historical filtering, use explicit chart arguments such as `historical_start`
and `historical_end`, or do the filtering in a data-preparation function.

## Standard Label Arguments

| Argument | Meaning |
|---|---|
| `title` | Optional chart title. |
| `subtitle` | Optional chart subtitle. |
| `x_label` | X-axis label. |
| `y_label` | Main y-axis label for one-measure charts. |
| `y_col_label` | Primary axis label for bars/columns. |
| `y_line_label` | Secondary axis label for lines. |
| `col_label` | Legend label for column/bar values. |
| `line_label` | Legend label for line values. |
| `labels` | Named vector mapping data category values to display labels. |

Prefer `y_label` for new one-axis functions. Existing functions that use `y_lab`
should be treated as legacy aliases.

## Standard Colour Arguments

| Argument | Meaning |
|---|---|
| `palette` | Named vector for general/group colours. |
| `palette_fill` | Named vector for bar/area/fill colours. |
| `palette_line` | Named vector for line colours. |

Use named vectors, where names match the raw category values in `group`.

```r
c("Beef and Veal" = "#5B8DEF", "Lamb" = "#74C476")
```

Legacy aliases still supported by the app/runner:

- `fill_palette`
- `colour_palette`

Use the standard names for new functions.

## Standard Style Arguments

| Argument | Meaning |
|---|---|
| `family` | Font family, default `"DIN"`. |
| `base_size` | Base font size, default `10.5`. |

Prefer `base_size`. Existing functions using `fontsize` should be treated as
legacy aliases.

## Standard Axis Break Arguments

| Argument | Meaning |
|---|---|
| `primary_min_breaks` | Minimum breaks on primary axis. |
| `primary_max_breaks` | Maximum breaks on primary axis. |
| `secondary_min_breaks` | Minimum breaks on secondary axis. |
| `secondary_max_breaks` | Maximum breaks on secondary axis. |

For one-axis charts, use `y_min_breaks` and `y_max_breaks` only if the function
is not a primary/secondary combo chart.

## Data Standards

Prepared chart data should be rectangular and tidy:

- one row per observation
- date fields should be `Date` where possible
- monthly dates should use the first day of the month
- year fields should be numeric/integer
- category fields should contain values that match metadata `forecast_group_key`
  where sector metadata colours/labels are expected
- numeric chart fields should be numeric before plotting

Recommended common field names:

| Field | Meaning |
|---|---|
| `date` | Date or month field. |
| `year` | Calendar or season end year. |
| `sopi_forecast_group` | SOPI category matching metadata. |
| `revenue` | Revenue value. |
| `volume` | Volume/quantity value. |
| `value` | Generic value for one-measure charts. |

## Compatibility Tiers

Use these tiers when reviewing plot functions:

- **Standard**: follows the protocol and should work directly in the app.
- **Compatible**: uses one or two supported aliases such as `y_lab`, `fontsize`,
  `fill_palette`, or `colour_palette`.
- **Specific**: tied to a particular dataset shape. It can still be used, but
  the app may need function-specific field controls or defaults.

New functions should aim for **Standard**.
