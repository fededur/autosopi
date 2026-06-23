read_config_sheet <- function(path, sheet) {
  readxl::read_excel(path, sheet = sheet, .name_repair = "unique_quiet") |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), empty_to_na))
}

empty_config_sheet <- function(columns) {
  stats::setNames(
    as.data.frame(rep(list(character()), length(columns)), stringsAsFactors = FALSE),
    columns
  )
}

read_chart_config <- function(path) {
  sheets <- readxl::excel_sheets(path)
  required <- c(
    "settings_sector",
    "plots",
    "data_sources",
    "plot_args",
    "data_args",
    "run_control",
    "palettes"
  )

  settings_sheet <- if ("release_settings" %in% sheets) {
    "release_settings"
  } else if ("settings_global" %in% sheets) {
    "settings_global"
  } else {
    NA_character_
  }

  if (is.na(settings_sheet)) {
    stop("Missing config sheet: release_settings", call. = FALSE)
  }

  required <- c(
    settings_sheet,
    "settings_sector",
    "plots",
    "data_sources",
    "plot_args",
    "data_args",
    "run_control",
    "palettes"
  )

  missing <- setdiff(required, sheets)
  if (length(missing) > 0) {
    stop("Missing config sheet(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  config <- setNames(
    lapply(required, function(sheet) read_config_sheet(path, sheet)),
    required
  )

  if (!"release_settings" %in% names(config)) {
    config$release_settings <- config[[settings_sheet]]
  }

  if ("data_transforms" %in% sheets) {
    config$data_transforms <- read_config_sheet(path, "data_transforms")
  } else {
    config$data_transforms <- empty_config_sheet(c("data_source_id", "active", "transform_function", "notes"))
  }

  if ("transform_args" %in% sheets) {
    config$transform_args <- read_config_sheet(path, "transform_args")
  } else {
    config$transform_args <- empty_config_sheet(c("data_source_id", "arg_name", "arg_value", "arg_type", "notes"))
  }

  validate_config(config)
  config
}

validate_config <- function(config) {
  duplicate_plots <- config$plots$plot_id[duplicated(config$plots$plot_id)]
  if (length(duplicate_plots) > 0) {
    stop("Duplicate plot_id values: ", paste(unique(duplicate_plots), collapse = ", "), call. = FALSE)
  }

  duplicate_sources <- config$data_sources$data_source_id[
    duplicated(config$data_sources$data_source_id)
  ]
  if (length(duplicate_sources) > 0) {
    stop("Duplicate data_source_id values: ", paste(unique(duplicate_sources), collapse = ", "), call. = FALSE)
  }

  missing_sources <- setdiff(config$plots$data_source_id, config$data_sources$data_source_id)
  if (length(missing_sources) > 0) {
    stop("Plot rows reference missing data_source_id values: ", paste(missing_sources, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}

build_run_plan <- function(config) {
  run_control <- settings_from_table(config$run_control)
  sector_filter <- run_control$sector_filter
  plot_filter <- run_control$plot_id_filter

  active_sectors <- config$settings_sector |>
    dplyr::filter(.data$active %in% c(TRUE, "TRUE", "true", "Yes", "yes", 1)) |>
    dplyr::pull(.data$sector)

  plan <- config$plots |>
    dplyr::filter(.data$active %in% c(TRUE, "TRUE", "true", "Yes", "yes", 1)) |>
    dplyr::filter(.data$sector %in% active_sectors)

  if (!is.null(sector_filter)) {
    plan <- plan |>
      dplyr::filter(.data$sector %in% sector_filter)
  }

  if (!is.null(plot_filter)) {
    plan <- plan |>
      dplyr::filter(.data$plot_id %in% plot_filter)
  }

  plan |>
    dplyr::arrange(.data$sector, .data$sort_order, .data$plot_id)
}

resolve_job_settings <- function(job, config) {
  global <- settings_from_table(config$release_settings)

  sector_row <- config$settings_sector |>
    dplyr::filter(.data$sector == job$sector)

  sector <- if (nrow(sector_row) == 0) {
    list()
  } else {
    as.list(sector_row[1, ])
  }

  plot_row <- as.list(job)

  source_row <- config$data_sources |>
    dplyr::filter(.data$data_source_id == job$data_source_id) |>
    dplyr::slice(1)

  transform_row <- config$data_transforms |>
    dplyr::filter(.data$data_source_id == job$data_source_id) |>
    dplyr::filter(.data$active %in% c(TRUE, "TRUE", "true", "Yes", "yes", 1)) |>
    dplyr::slice(1)

  data_args <- merge_args(
    global,
    sector,
    args_from_table(config$data_args, "data_source_id", job$data_source_id)
  )

  plot_args <- merge_args(
    global,
    sector,
    plot_row,
    args_from_table(config$plot_args, "plot_id", job$plot_id)
  )

  transform_args <- merge_args(
    global,
    sector,
    args_from_table(config$transform_args, "data_source_id", job$data_source_id)
  )

  plot_args <- apply_release_plot_aliases(plot_args)
  data_args <- apply_release_data_aliases(data_args)
  transform_args <- apply_release_data_aliases(transform_args)

  list(
    global = global,
    sector = sector,
    plot = plot_row,
    data_source = source_row,
    data_transform = transform_row,
    data_args = data_args,
    transform_args = transform_args,
    plot_args = plot_args
  )
}

apply_release_plot_aliases <- function(args) {
  if (is.null(args$forecast_start) && !is.null(args$forecast_start_year)) {
    args$forecast_start <- args$forecast_start_year
  }

  if (is.null(args$forecast_end) && !is.null(args$forecast_end_year)) {
    args$forecast_end <- args$forecast_end_year
  }

  args
}

apply_release_data_aliases <- function(args) {
  if (is.null(args$year_start) && !is.null(args$historical_start_year)) {
    args$year_start <- args$historical_start_year
  }

  if (is.null(args$year_end) && !is.null(args$historical_end_year)) {
    args$year_end <- args$historical_end_year
  }

  args
}
