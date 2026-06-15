run_charts <- function(run_plan, config, project_root) {
  run_control <- settings_from_table(config$run_control)
  dry_run <- isTRUE(run_control$dry_run)
  metadata_resource <- load_metadata_resource(project_root)

  for (i in seq_len(nrow(run_plan))) {
    job <- run_plan[i, ]
    message("Running ", job$plot_id, " [", job$sector, "]")

    started_at <- Sys.time()
    status <- "success"
    message_text <- ""
    output_path <- NA_character_

    tryCatch({
      resolved <- resolve_job_settings(job, config)

      data <- get_plot_data(
        data_source = resolved$data_source,
        data_args = resolved$data_args,
        project_root = project_root
      )

      plot_args <- clean_plot_args(
        args = resolved$plot_args,
        config = config,
        project_root = project_root,
        data = data,
        metadata_resource = metadata_resource
      )
      plot_args$data <- data

      plot <- call_named_function(job$plot_function, plot_args)

      output_root <- resolved$global$output_root %||% "outputs"
      output_subfolder <- resolved$sector$output_subfolder %||% job$sector
      output_file <- job$output_file
      output_path <- build_output_path(
        project_root = project_root,
        output_root = output_root,
        release_year = resolved$global$release_year,
        release_round = resolved$global$release_round,
        sector_folder = output_subfolder,
        output_file = output_file
      )

      if (!dry_run) {
        save_chart_svg(
          plot = plot,
          output_path = output_path,
          width = as.numeric(resolved$global$width %||% 9),
          height = as.numeric(resolved$global$height %||% 5)
        )
      }
    }, error = function(e) {
      status <<- "error"
      message_text <<- conditionMessage(e)
      message("  Error: ", message_text)
    })

    append_log(project_root, list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      plot_id = job$plot_id,
      sector = job$sector,
      status = status,
      output_path = output_path,
      elapsed_seconds = round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 2),
      message = message_text
    ))
  }
}

clean_plot_args <- function(args, config, project_root, data = NULL, metadata_resource = NULL) {
  sector <- args$sector
  group <- args$group

  framework_only <- c(
    "active", "sector", "plot_id", "plot_function", "data_source_id", "output_file",
    "sort_order", "notes", "source_type", "source_ref", "sheet", "range",
    "data_function", "cache", "output_root", "output_subfolder", "file_type",
    "width", "height", "dpi", "overwrite", "setting_name", "setting_value",
    "setting_type", "dry_run", "run_all_active", "sector_filter", "plot_id_filter",
    "save_logs", "release_year", "release_round", "historical_start_year",
    "historical_end_year", "forecast_start_year", "forecast_end_year"
  )

  args <- args[setdiff(names(args), framework_only)]

  args$palette <- resolve_palette_arg(args$palette, config)
  args$palette_fill <- resolve_palette_arg(args$palette_fill, config)
  args$palette_line <- resolve_palette_arg(args$palette_line, config)

  categories <- NULL
  if (!is.null(data) && !is.null(group) && group %in% names(data)) {
    categories <- unique(as.character(data[[group]]))
  }

  metadata_style <- style_from_metadata(
    metadata_resource = metadata_resource,
    sector = sector,
    categories = categories
  )

  if (is.null(args$palette)) {
    args$palette <- metadata_style$palette
  }

  if (is.null(args$palette_fill)) {
    args$palette_fill <- args$palette
  }

  if (is.null(args$palette_line)) {
    args$palette_line <- args$palette
  }

  if (is.null(args$labels)) {
    args$labels <- metadata_style$labels
  }

  if (!is.null(categories)) {
    args$palette <- complete_palette(categories, args$palette)
    args$palette_fill <- complete_palette(categories, args$palette_fill)
    args$palette_line <- complete_palette(categories, args$palette_line)
    args$labels <- complete_labels(categories, args$labels)
  }

  args
}

resolve_palette_arg <- function(palette, config) {
  if (is.null(palette) || length(palette) == 0) return(NULL)

  if (is.character(palette) && length(palette) == 1) {
    if (is.na(palette) || !nzchar(trimws(palette))) return(NULL)

    config_palette <- palette_from_config(config, palette)
    if (!is.null(config_palette)) return(config_palette)
  }

  palette
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

build_output_path <- function(
    project_root,
    output_root,
    release_year,
    release_round,
    sector_folder,
    output_file) {
  release_parts <- c(release_year, release_round)
  release_parts <- release_parts[!vapply(release_parts, is.null, logical(1))]
  release_parts <- as.character(release_parts)
  release_parts <- release_parts[!is.na(release_parts) & nzchar(release_parts)]
  release_parts <- gsub("[^A-Za-z0-9_-]+", "_", release_parts)

  do.call(
    file.path,
    c(
      list(project_root, output_root),
      as.list(release_parts),
      list(sector_folder, output_file)
    )
  )
}
