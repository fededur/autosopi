#' Prepare fake Meat and Wool time series data
#'
#' @sopi_fields year, group, sector, revenue, volume, export_value_nzd, export_volume_tonnes
#' @return A data frame with columns: year, group, sector, revenue, volume, export_value_nzd, export_volume_tonnes.
prep_fake_meat_wool_timeseries <- function(
    sector = "Meat and Wool",
    historical_start_year = NULL,
    historical_end_year = NULL,
    year_start = historical_start_year,
    year_end = historical_end_year,
    categories = NULL,
    project_root = getwd(),
    metadata_file = "sopi_metadata.xlsx",
    metadata_sheet = "metadata",
    seed = 20,
    ...
) {
  set.seed(seed)

  if (is.null(year_start) || is.null(year_end)) {
    stop("historical_start_year and historical_end_year are required.", call. = FALSE)
  }

  if (is.null(categories)) {
    metadata <- read_metadata(
      path = file.path(project_root, "metadata", metadata_file),
      sheet = metadata_sheet
    )

    categories <- metadata |>
      dplyr::filter(.data$sector_key == sector, .data$level == "forecast_group") |>
      dplyr::filter(.data$include %in% c(TRUE, "TRUE", "true", "Yes", "yes", 1)) |>
      dplyr::pull(.data$forecast_group_key) |>
      unique()
  }

  if (length(categories) == 0) {
    stop("No Meat and Wool forecast-group categories found in metadata.", call. = FALSE)
  }

  years <- seq.int(as.integer(year_start), as.integer(year_end))

  df <- expand.grid(
    year = years,
    group = categories,
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(.data$year, .data$group)

  category_index <- match(df$group, categories)
  year_index <- df$year - min(df$year) + 1

  base_revenue <- c(
    `Beef and Veal` = 4200,
    Lamb = 3600,
    Mutton = 650,
    Wool = 480,
    Venison = 350,
    `Other Meat` = 280,
    `Animal Co-Products` = 620,
    `Animal Fats and Oils` = 180,
    `Animal Products for Feed` = 220,
    `Carpets and Other Wool Products` = 140,
    `Hides, Leather and Dressed Skins` = 300
  )

  base_volume <- c(
    `Beef and Veal` = 520000,
    Lamb = 410000,
    Mutton = 115000,
    Wool = 85000,
    Venison = 30000,
    `Other Meat` = 45000,
    `Animal Co-Products` = 160000,
    `Animal Fats and Oils` = 55000,
    `Animal Products for Feed` = 70000,
    `Carpets and Other Wool Products` = 12000,
    `Hides, Leather and Dressed Skins` = 25000
  )

  revenue_base <- base_revenue[df$group]
  volume_base <- base_volume[df$group]

  revenue_base[is.na(revenue_base)] <- 250 + category_index[is.na(revenue_base)] * 90
  volume_base[is.na(volume_base)] <- 20000 + category_index[is.na(volume_base)] * 8000

  df |>
    dplyr::mutate(
      sector = sector,
      revenue = round((revenue_base * (1 + 0.025 * year_index) + stats::rnorm(dplyr::n(), 0, revenue_base * 0.06)), 1),
      volume = round((volume_base * (1 + 0.01 * year_index) + stats::rnorm(dplyr::n(), 0, volume_base * 0.05)), 0),
      export_value_nzd = revenue * 1000000,
      export_volume_tonnes = volume
    )
}
