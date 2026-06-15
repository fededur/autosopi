source_directory <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(sort(files), source))
}

empty_to_na <- function(x) {
  x <- as.character(x)
  x[trimws(x) == ""] <- NA_character_
  x
}

is_blank <- function(x) {
  length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))
}

parse_scalar <- function(value, type = "character") {
  if (is_blank(value)) return(NULL)

  value <- as.character(value)
  type <- if (is_blank(type)) "character" else as.character(type)

  switch(
    type,
    character = value,
    numeric = as.numeric(value),
    integer = as.integer(value),
    logical = tolower(value) %in% c("true", "t", "yes", "y", "1"),
    character_vector = trimws(strsplit(value, ",", fixed = TRUE)[[1]]),
    numeric_vector = as.numeric(trimws(strsplit(value, ",", fixed = TRUE)[[1]])),
    name = value,
    value
  )
}

args_from_table <- function(tbl, id_col, id_value) {
  if (nrow(tbl) == 0 || is.null(id_value) || is.na(id_value)) return(list())

  rows <- tbl |>
    dplyr::filter(.data[[id_col]] == id_value)

  if (nrow(rows) == 0) return(list())

  values <- Map(parse_scalar, rows$arg_value, rows$arg_type)
  names(values) <- rows$arg_name
  values[!vapply(values, is.null, logical(1))]
}

settings_from_table <- function(tbl) {
  if (nrow(tbl) == 0) return(list())

  values <- Map(parse_scalar, tbl$setting_value, tbl$setting_type)
  names(values) <- tbl$setting_name
  values[!vapply(values, is.null, logical(1))]
}

merge_args <- function(...) {
  lists <- list(...)
  Reduce(function(x, y) utils::modifyList(x, y), lists, init = list())
}

call_named_function <- function(function_name, args) {
  if (!exists(function_name, mode = "function")) {
    stop("Function not found: ", function_name, call. = FALSE)
  }

  fn <- get(function_name, mode = "function")
  fn_args <- names(formals(fn))

  if (!"..." %in% fn_args) {
    args <- args[names(args) %in% fn_args]
  }

  do.call(fn, args)
}
