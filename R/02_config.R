read_config_sheet <- function(path, sheet) {
  readxl::read_excel(path, sheet = sheet, .name_repair = "unique_quiet") |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), empty_to_na))
}

read_chart_config <- function(path) {
  sheets <- readxl::excel_sheets(path)
  required <- c(
    "settings_global",
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
  global <- settings_from_table(config$settings_global)

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

  list(
    global = global,
    sector = sector,
    plot = plot_row,
    data_source = source_row,
    data_args = data_args,
    plot_args = plot_args
  )
}
