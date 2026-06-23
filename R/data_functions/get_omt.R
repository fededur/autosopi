omt_default_columns <- function() {
  list(
    "Primary Industry Sector" = "NZHSC",
    "SOPI Forecast Group" = "NZHSC",
    "Month Start Date" = "Time"
  )
}

omt_default_measures <- function() {
  list(
    revenue = "'Export Measures'[Export Free On Board ($NZ)]"
  )
}

omt_named_list <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(list())
  }

  if (is.list(value)) {
    return(value)
  }

  if (is.character(value) && is.null(names(value)) && length(value) == 1) {
    lines <- trimws(strsplit(value, "\n", fixed = TRUE)[[1]])
    lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

    keys <- character()
    values <- character()

    for (line in lines) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) < 2) next

      key <- trimws(parts[[1]])
      mapped_value <- trimws(paste(parts[-1], collapse = "="))
      if (!nzchar(key) || !nzchar(mapped_value)) next

      keys <- c(keys, key)
      values <- c(values, mapped_value)
    }

    value <- stats::setNames(values, keys)
  }

  if (is.null(names(value)) || !any(nzchar(names(value)))) {
    stop("OMT list arguments must be named, for example: label = DAX expression.", call. = FALSE)
  }

  parsed <- as.list(unname(value))
  names(parsed) <- names(value)

  lapply(parsed, function(item) {
    if (!is.character(item) || length(item) != 1) {
      return(item)
    }

    item <- trimws(item)
    lower <- tolower(item)
    if (lower %in% c("true", "t", "yes", "y", "1")) return(TRUE)
    if (lower %in% c("false", "f", "no", "n", "0")) return(FALSE)

    item
  })
}

omt_filter_in <- function(table, column, values) {
  values <- values[!is.na(values) & nzchar(trimws(as.character(values)))]
  if (length(values) == 0) {
    return(NULL)
  }

  quoted_values <- paste0('"', values, '"', collapse = ",")
  sprintf("'%s'[%s] in {%s}", table, column, quoted_values)
}

get_omt_data <- function(
    dataset_id = "36a78684-827e-4296-8983-1e78343fe6f0",
    columns_list = omt_default_columns(),
    measures_list = omt_default_measures(),
    filters_list = NULL,
    sector = NULL,
    forecast_group = NULL,
    date_column = NULL,
    group_column = NULL
) {
  columns_list <- omt_named_list(columns_list)
  measures_list <- omt_named_list(measures_list)
  filters_list <- omt_named_list(filters_list)

  if (length(columns_list) == 0) {
    columns_list <- omt_default_columns()
  }

  if (length(measures_list) == 0) {
    measures_list <- omt_default_measures()
  }

  sector <- setdiff(as.character(sector), "All sectors")

  if (!is.null(sector) && length(sector) > 0 && !"Primary Industry Sector" %in% names(filters_list)) {
    sector_filter <- omt_filter_in("NZHSC", "Primary Industry Sector", as.character(sector))
    if (!is.null(sector_filter)) {
      filters_list[["Primary Industry Sector"]] <- sector_filter
    }
  }

  if (!is.null(forecast_group) && length(forecast_group) > 0 && !"SOPI Forecast Group" %in% names(filters_list)) {
    forecast_group_filter <- omt_filter_in("NZHSC", "SOPI Forecast Group", as.character(forecast_group))
    if (!is.null(forecast_group_filter)) {
      filters_list[["SOPI Forecast Group"]] <- forecast_group_filter
    }
  }

  data <- getPwrBI(
    dataset_id = dataset_id,
    mpi_tenant_id = get_app_token("mpi_tenant_id"),
    local_r_code_app_id = get_app_token("local_r_code_app_id"),
    columns_list = columns_list,
    measures_list = measures_list,
    filters_list = filters_list
  )

  if (!is.null(date_column) && date_column %in% names(data)) {
    data <- dplyr::rename(data, date = dplyr::all_of(date_column))
  }

  if (!is.null(group_column) && group_column %in% names(data)) {
    data <- dplyr::rename(data, group = dplyr::all_of(group_column))
  }

  data
}
