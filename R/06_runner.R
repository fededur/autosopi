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
        data_transform = resolved$data_transform,
        transform_args = resolved$transform_args,
        project_root = project_root
      )

      plot_args <- clean_plot_args(
        args = merge_args(resolved$plot_args, list(plot_function = job$plot_function)),
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
          width = as.numeric(resolved$plot_args$width %||% resolved$global$width %||% 230),
          height = as.numeric(resolved$plot_args$height %||% resolved$global$height %||% 130)
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
  driver <- args$driver
  plot_function <- args$plot_function
  use_metadata_palette <- isTRUE(args$use_metadata_palette)

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
  args$use_metadata_palette <- NULL

  if (use_metadata_palette) {
    args$palette <- NULL
    args$palette_fill <- NULL
    args$palette_line <- NULL
  }

  palette_arg_names <- sopi_palette_arg_names(include_aliases = TRUE)
  for (arg_name in intersect(palette_arg_names, names(args))) {
    args[[arg_name]] <- resolve_palette_arg(args[[arg_name]], config, metadata_resource)
  }

  label_categories <- NULL
  if (!is.null(data) && !is.null(group) && group %in% names(data)) {
    label_categories <- unique(as.character(data[[group]]))
  }

  palette_categories <- label_categories
  if (identical(plot_function, "plot_net_contribution")) {
    driver_categories <- NULL
    if (!is.null(data) && !is.null(driver) && driver %in% names(data)) {
      driver_categories <- unique(as.character(data[[driver]]))
      driver_categories <- driver_categories[!is.na(driver_categories) & nzchar(driver_categories)]
    }
    point_label <- args$point_label %||% "Net contribution"
    palette_categories <- unique(c(driver_categories, point_label))
  }

  metadata_style <- style_from_metadata(
    metadata_resource = metadata_resource,
    sector = sector,
    categories = palette_categories
  )

  metadata_label_style <- style_from_metadata(
    metadata_resource = metadata_resource,
    sector = sector,
    categories = label_categories
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
    args$labels <- metadata_label_style$labels
  }

  if (identical(plot_function, "plot_net_contribution") && is.null(args$fill_labels)) {
    args$fill_labels <- metadata_style$labels
  }

  if (!is.null(palette_categories)) {
    if (identical(plot_function, "plot_net_contribution")) {
      args$palette <- complete_palette_by_position(palette_categories, args$palette)
      args$palette_fill <- complete_palette_by_position(palette_categories, args$palette_fill)
      args$palette_line <- complete_palette_by_position(palette_categories, args$palette_line)
    } else {
      args$palette <- complete_palette(palette_categories, args$palette)
      args$palette_fill <- complete_palette(palette_categories, args$palette_fill)
      args$palette_line <- complete_palette(palette_categories, args$palette_line)
    }
  }

  if (!is.null(label_categories)) {
    args$labels <- complete_labels(label_categories, args$labels)
  }

  if (identical(plot_function, "plot_net_contribution") && !is.null(palette_categories)) {
    args$fill_labels <- complete_labels(palette_categories, args$fill_labels)
  }

  args
}

resolve_palette_arg <- function(palette, config, metadata_resource = NULL) {
  if (is.null(palette) || length(palette) == 0) return(NULL)

  if (is.character(palette) && length(palette) == 1) {
    if (is.na(palette) || !nzchar(trimws(palette))) return(NULL)

    config_palette <- palette_from_config(config, palette)
    if (!is.null(config_palette)) return(config_palette)

    metadata_palette <- palette_from_custom_metadata(metadata_resource, palette)
    if (!is.null(metadata_palette)) return(metadata_palette)
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
  context <- list(
    release_year = release_year,
    release_round = release_round,
    sector = sector_folder
  )
  has_release_tokens <- grepl("\\{year\\}|\\{release_year\\}|\\{release\\}|\\{release_round\\}", output_root)
  has_sector_token <- grepl("\\{sector\\}", output_root)

  release_parts <- c(release_year, release_round)
  release_parts <- release_parts[!vapply(release_parts, is.null, logical(1))]
  release_parts <- as.character(release_parts)
  release_parts <- release_parts[!is.na(release_parts) & nzchar(release_parts)]
  release_parts <- gsub("[^A-Za-z0-9_-]+", "_", release_parts)
  if (has_release_tokens) {
    release_parts <- character()
  }

  sector_parts <- if (has_sector_token) {
    character()
  } else {
    sector_folder
  }

  do.call(
    file.path,
    c(
      list(resolve_project_path(project_root, output_root, context)),
      as.list(release_parts),
      as.list(sector_parts),
      list(output_file)
    )
  )
}
