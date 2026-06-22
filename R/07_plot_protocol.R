sopi_plot_protocol <- function() {
  list(
    required = c("data"),
    data = c("x", "y", "y_col", "y_line", "group", "driver", "total"),
    data_aliases = c("date_var"),
    time = c(
      "x_freq",
      "period_type",
      "financial_start_month",
      "forecast",
      "forecast_start",
      "forecast_end"
    ),
    labels = c(
      "title",
      "subtitle",
      "x_label",
      "y_label",
      "y_col_label",
      "y_line_label",
      "col_label",
      "line_label",
      "labels"
    ),
    palettes = c("palette", "palette_fill", "palette_line"),
    palette_aliases = c("fill_palette", "colour_palette"),
    style = c("family", "base_size"),
    style_aliases = c("fontsize"),
    layout = c("col_position", "primary_axis", "sort_col", "sort_line", "sort_desc", "sort", "legend_order", "col_order", "line_order"),
    axis_breaks = c(
      "primary_min_breaks",
      "primary_max_breaks",
      "secondary_min_breaks",
      "secondary_max_breaks",
      "y_min_breaks",
      "y_max_breaks",
      "n_breaks",
      "x_breaks"
    ),
    axis_labels = c("y_col_accuracy", "y_line_accuracy", "y_col_scale", "y_line_scale", "y_scale"),
    label_aliases = c("y_lab")
  )
}

sopi_palette_arg_names <- function(include_aliases = TRUE) {
  protocol <- sopi_plot_protocol()
  if (include_aliases) {
    c(protocol$palettes, protocol$palette_aliases)
  } else {
    protocol$palettes
  }
}

sopi_style_arg_names <- function(include_aliases = TRUE) {
  protocol <- sopi_plot_protocol()
  if (include_aliases) {
    c(protocol$style, protocol$style_aliases)
  } else {
    protocol$style
  }
}

sopi_plot_standard_arg_names <- function(include_aliases = TRUE) {
  protocol <- sopi_plot_protocol()
  args <- c(
    protocol$required,
    protocol$data,
    protocol$time,
    protocol$labels,
    protocol$palettes,
    protocol$style,
    protocol$layout,
    protocol$axis_breaks,
    protocol$axis_labels
  )

  if (include_aliases) {
    args <- c(
      args,
      protocol$data_aliases,
      protocol$palette_aliases,
      protocol$style_aliases,
      protocol$label_aliases
    )
  }

  unique(args)
}

sopi_plot_protocol_status <- function(function_name) {
  if (!exists(function_name, mode = "function")) {
    stop("Function not found: ", function_name, call. = FALSE)
  }

  fn_args <- names(formals(get(function_name, mode = "function")))
  protocol <- sopi_plot_protocol()
  standard_args <- sopi_plot_standard_arg_names(include_aliases = FALSE)
  alias_args <- setdiff(sopi_plot_standard_arg_names(include_aliases = TRUE), standard_args)

  issues <- character()
  notes <- character()

  if (length(fn_args) == 0 || !identical(fn_args[[1]], "data")) {
    issues <- c(issues, "First argument should be data.")
  }

  if (!"data" %in% fn_args) {
    issues <- c(issues, "Missing data argument.")
  }

  if (!any(c("x", "date_var") %in% fn_args)) {
    notes <- c(notes, "No standard x/date argument detected.")
  }

  if (!any(c("y", "y_col", "y_line") %in% fn_args)) {
    notes <- c(notes, "No standard y/y_col/y_line argument detected.")
  }

  aliases_used <- intersect(fn_args, alias_args)
  if (length(aliases_used) > 0) {
    notes <- c(notes, paste("Uses supported legacy aliases:", paste(aliases_used, collapse = ", ")))
  }

  if (!any(protocol$palettes %in% fn_args) && any(protocol$palette_aliases %in% fn_args)) {
    notes <- c(notes, "Uses palette aliases but not standard palette arguments.")
  }

  if (!"base_size" %in% fn_args && "fontsize" %in% fn_args) {
    notes <- c(notes, "Uses fontsize alias instead of base_size.")
  }

  status <- if (length(issues) > 0) {
    "specific"
  } else if (length(notes) > 0) {
    "compatible"
  } else {
    "standard"
  }

  data.frame(
    function_name = function_name,
    status = status,
    issues = paste(issues, collapse = " | "),
    notes = paste(notes, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

sopi_plot_protocol_report <- function(function_names = NULL) {
  if (is.null(function_names)) {
    function_names <- ls(pattern = "^plot_", envir = .GlobalEnv)
  }

  reports <- lapply(function_names, sopi_plot_protocol_status)
  dplyr::bind_rows(reports)
}
