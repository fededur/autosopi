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

is_absolute_path <- function(path) {
  !is_blank(path) && grepl("^([A-Za-z]:|/|\\\\\\\\)", path)
}

path_context_value <- function(context, name) {
  value <- context[[name]]
  if (is.null(value) || length(value) == 0 || is.na(value)) "" else as.character(value[[1]])
}

render_path_template <- function(path, context = list()) {
  if (is_blank(path)) return(path)

  rendered <- as.character(path)
  replacements <- c(
    year = path_context_value(context, "release_year"),
    release_year = path_context_value(context, "release_year"),
    release = path_context_value(context, "release_round"),
    release_round = path_context_value(context, "release_round"),
    sector = path_context_value(context, "sector")
  )

  for (name in names(replacements)) {
    rendered <- gsub(paste0("\\{", name, "\\}"), replacements[[name]], rendered, fixed = FALSE)
  }

  rendered
}

sopi_releases_root <- function() {
  root <- Sys.getenv("SOPI_RELEASES_ROOT", unset = "")
  if (is_blank(root)) NULL else root
}

resolve_project_path <- function(project_root, path, context = list()) {
  if (is_blank(path)) return(path)

  path <- render_path_template(path, context)

  if (grepl("\\{SOPI_RELEASES_ROOT\\}", path)) {
    root <- sopi_releases_root()
    if (is.null(root)) {
      stop("SOPI_RELEASES_ROOT is not set for portable SharePoint path: ", path, call. = FALSE)
    }

    path <- gsub("\\{SOPI_RELEASES_ROOT\\}", root, path)
  }

  if (is_absolute_path(path)) path else file.path(project_root, path)
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
    named_character_vector = {
      lines <- trimws(strsplit(value, "\n", fixed = TRUE)[[1]])
      lines <- lines[nzchar(lines)]
      keys <- character()
      values <- character()

      for (line in lines) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        if (length(parts) < 2) next

        key <- trimws(parts[[1]])
        mapped_value <- trimws(paste(parts[-1], collapse = "="))

        if (!nzchar(key) || !nzchar(mapped_value)) next

        keys <- c(keys, key)
        values <- c(values, mapped_value)
      }

      if (length(keys) == 0) NULL else stats::setNames(values, keys)
    },
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
