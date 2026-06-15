#' Prepare fake ranked market data
#'
#' @sopi_fields sector, year, market, export_value_nzd
#' @return A data frame with columns: sector, year, market, export_value_nzd.
prep_fake_ranked_markets <- function(
    sector,
    historical_start_year = NULL,
    historical_end_year = NULL,
    year_start = historical_start_year,
    year_end = historical_end_year,
    markets = c("China", "Australia", "United States", "Japan", "Korea", "EU", "UK", "Singapore"),
    seed = 2,
    ...
) {
  set.seed(seed)

  data.frame(
    sector = sector,
    year = as.integer(year_end),
    market = markets,
    export_value_nzd = round(sort(stats::runif(length(markets), 20, 220), decreasing = TRUE) * 1000000, 0),
    stringsAsFactors = FALSE
  )
}
