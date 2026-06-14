run_charts <- function(run_plan, config, project_root) {
  run_control <- settings_from_table(config$run_control)
  dry_run <- isTRUE(run_control$dry_run)

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

      plot_args <- clean_plot_args(resolved$plot_args, config)
      plot_args$data <- data

      plot <- call_named_function(job$plot_function, plot_args)

      output_root <- resolved$global$output_root %||% "outputs"
      output_subfolder <- resolved$sector$output_subfolder %||% job$sector
      output_file <- job$output_file
      output_path <- file.path(project_root, output_root, output_subfolder, output_file)

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

clean_plot_args <- function(args, config) {
  framework_only <- c(
    "active", "sector", "plot_id", "plot_function", "data_source_id", "output_file",
    "sort_order", "notes", "source_type", "source_ref", "sheet", "range",
    "data_function", "cache", "output_root", "output_subfolder", "file_type",
    "width", "height", "dpi", "overwrite", "setting_name", "setting_value",
    "setting_type", "dry_run", "run_all_active", "sector_filter", "plot_id_filter",
    "save_logs"
  )

  args <- args[setdiff(names(args), framework_only)]

  if (!is.null(args$palette) && is.character(args$palette) && length(args$palette) == 1) {
    pal <- palette_from_config(config, args$palette)
    if (!is.null(pal)) args$palette <- pal
  }

  if (!is.null(args$palette_fill) && is.character(args$palette_fill) && length(args$palette_fill) == 1) {
    pal <- palette_from_config(config, args$palette_fill)
    if (!is.null(pal)) args$palette_fill <- pal
  }

  if (!is.null(args$palette_line) && is.character(args$palette_line) && length(args$palette_line) == 1) {
    pal <- palette_from_config(config, args$palette_line)
    if (!is.null(pal)) args$palette_line <- pal
  }

  args
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}
