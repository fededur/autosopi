prep_fake_ranked_markets <- function(
    sector,
    year_start,
    year_end,
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
