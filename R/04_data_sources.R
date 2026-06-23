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

  transform_function <- data_transform$transform_function[[1]]
  if (is_blank(transform_function)) {
    return(data)
  }

  transform_args$data <- data
  transform_args$project_root <- project_root
  call_named_function(transform_function, transform_args)
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
