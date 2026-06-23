get_monthly_omt_data <- function(
    sector = NULL,
    forecast_group = NULL
) {
  data <- get_omt_data(
    sector = sector,
    forecast_group = forecast_group,
    date_column = "Month Start Date",
    group_column = "SOPI Forecast Group",
    columns_list = list(
      "Primary Industry Sector" = "NZHSC",
      "SOPI Forecast Group" = "NZHSC",
      "Month Start Date" = "Time"
    ),
    measures_list = list(
      revenue = "'Export Measures'[Export Free On Board ($NZ)]"
    )
  )

  return(data)
}
