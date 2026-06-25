get_plot_data <- function(data_source, data_args, project_root, data_transform = NULL, transform_args = list()) {
  source_type <- data_source$source_type[[1]]

  data <- if (identical(source_type, "excel")) {
    read_excel_data(data_source, project_root, data_args)
  } else if (identical(source_type, "function")) {
    data_function <- data_source$data_function[[1]]
    data_args$project_root <- project_root
    call_named_function(data_function, data_args)
  } else {
    stop("Unsupported source_type: ", source_type, call. = FALSE)
  }

  apply_data_transform(data, data_transform, transform_args, project_root)
}

apply_data_transform <- function(data, data_transform = NULL, transform_args = list(), project_root = NULL) {
  if (is.null(data_transform) || nrow(data_transform) == 0) {
    return(data)
  }

  if (!"transform_step" %in% names(data_transform)) {
    data_transform$transform_step <- "1"
  }

  if (is.data.frame(transform_args) && !"transform_step" %in% names(transform_args)) {
    transform_args$transform_step <- "1"
  }

  data_transform <- data_transform |>
    dplyr::mutate(.transform_step_num = suppressWarnings(as.numeric(.data$transform_step))) |>
    dplyr::arrange(.data$.transform_step_num, .data$transform_step) |>
    dplyr::select(-".transform_step_num")

  out <- data

  for (i in seq_len(nrow(data_transform))) {
    transform_function <- data_transform$transform_function[[i]]
    if (is_blank(transform_function)) {
      next
    }

    step <- as.character(data_transform$transform_step[[i]])
    step_args <- if (is.data.frame(transform_args)) {
      args_from_transform_table(transform_args, data_transform$data_source_id[[i]], step)
    } else {
      transform_args
    }

    step_args$data <- out
    step_args$project_root <- project_root
    out <- call_named_function(transform_function, step_args)
  }

  out
}

args_from_transform_table <- function(tbl, data_source_id, transform_step) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(list())
  }

  rows <- tbl |>
    dplyr::filter(.data$data_source_id == data_source_id) |>
    dplyr::filter(.data$transform_step == transform_step)

  if (nrow(rows) == 0) {
    return(list())
  }

  values <- Map(parse_scalar, rows$arg_value, rows$arg_type)
  names(values) <- rows$arg_name
  values[!vapply(values, is.null, logical(1))]
}

read_excel_data <- function(data_source, project_root, context = list()) {
  source_ref <- data_source$source_ref[[1]]
  sheet <- data_source$sheet[[1]]
  range <- data_source$range[[1]]

  if (is_blank(source_ref)) stop("Excel data source is missing source_ref.", call. = FALSE)
  if (is_blank(sheet)) stop("Excel data source is missing sheet.", call. = FALSE)

  path <- resolve_project_path(project_root, source_ref, context)
  if (!file.exists(path)) {
    stop("Excel data file not found: ", path, call. = FALSE)
  }

  if (is_blank(range)) {
    readxl::read_excel(path, sheet = sheet)
  } else {
    readxl::read_excel(path, sheet = sheet, range = range)
  }
}
