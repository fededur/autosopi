#' Prepare fake sector time series data
#'
#' @sopi_fields year, product, sector, export_value_nzd, export_volume_tonnes
#' @return A data frame with columns: year, product, sector, export_value_nzd, export_volume_tonnes.
prep_fake_sector_timeseries <- function(
    sector,
    historical_start_year = NULL,
    historical_end_year = NULL,
    year_start = historical_start_year,
    year_end = historical_end_year,
    products = c("Product A", "Product B", "Product C"),
    seed = 1,
    ...
) {
  set.seed(seed)

  years <- seq.int(as.integer(year_start), as.integer(year_end))
  df <- expand.grid(
    year = years,
    product = products,
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(.data$year, .data$product)

  product_index <- match(df$product, products)
  year_index <- df$year - min(df$year) + 1

  df |>
    dplyr::mutate(
      sector = sector,
      export_value_nzd = round((80 + product_index * 35 + year_index * 8 + stats::rnorm(dplyr::n(), 0, 8)) * 1000000, 0),
      export_volume_tonnes = round((12000 + product_index * 6000 + year_index * 900 + stats::rnorm(dplyr::n(), 0, 1200)), 0)
    )
}
