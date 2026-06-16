get_plot_data <- function(data_source, data_args, project_root) {
  source_type <- data_source$source_type[[1]]

  if (identical(source_type, "excel")) {
    read_excel_data(data_source, project_root, data_args)
  } else if (identical(source_type, "function")) {
    data_function <- data_source$data_function[[1]]
    data_args$project_root <- project_root
    call_named_function(data_function, data_args)
  } else {
    stop("Unsupported source_type: ", source_type, call. = FALSE)
  }
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
