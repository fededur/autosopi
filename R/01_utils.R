source_directory <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(sort(files), source))
}

get_app_token <- function(app) {
  env_var <- toupper(app)
  tok <- Sys.getenv(env_var, unset = NA_character_)

  if (is.na(tok) || !nzchar(tok)) {
    stop("Missing ", env_var, ". Set it in .Renviron and restart R.", call. = FALSE)
  }

  tok
}

buildDaxQuery <- function(
    columns = list(),
    filters = list(),
    measures = list()) {

  columns_part <- paste(
    lapply(names(columns), function(col) {
      paste0("\t'", columns[[col]], "'[", col, "]")
    }),
    collapse = ",\n"
  )

  filters_part <- if (length(filters) > 0) {
    paste(
      lapply(seq_along(filters), function(i) {
        filter_condition <- filters[[i]]
        filter_column <- names(filters)[i]
        paste0("FILTER('", columns[[filter_column]], "', ", filter_condition, ")")
      }),
      collapse = ",\n\t"
    )
  } else {
    ""
  }

  if ("SOPI Export Price $NZ/KG" %in% names(measures)) {
    calculated_column <- paste(
      "\n\t\tVAR FOB = SUM('Export'[ExportFOB])",
      "\n\t\tVAR QNY = SUM('Export'[ExportQuantity])",
      "\n\t\tRETURN\n\t\t IF(ISBLANK(QNY) || QNY == 0,\n\t\t  BLANK(),\n\t\t  FOB/QNY)",
      sep = ""
    )
    measures[["SOPI Export Price $NZ/KG"]] <- calculated_column
  }

  measures_part <- paste(
    paste0("\"", names(measures), "\", ", measures),
    collapse = ",\n\t"
  )

  paste(
    "EVALUATE",
    "SUMMARIZECOLUMNS(",
    columns_part,
    if (nchar(filters_part) > 0) paste0(",\n\t", filters_part) else "",
    if (nchar(measures_part) > 0) paste0(",\n\t", measures_part) else "",
    ")",
    sep = "\n"
  )
}

getPwrBI <- function(
    mpi_tenant_id = get_app_token("mpi_tenant_id"),
    local_r_code_app_id = get_app_token("local_r_code_app_id"),
    dataset_id = "36a78684-827e-4296-8983-1e78343fe6f0",
    columns_list = NULL,
    filters_list = NULL,
    measures_list = NULL,
    dax_query = NULL) {

  powerbi_rest_api_resource_url <- "https://analysis.windows.net/powerbi/api"
  dataset_read_scope <- "Dataset.Read.All"

  query_args <- list(
    columns = columns_list,
    filters = filters_list,
    measures = measures_list
  )

  if (is.null(dax_query)) {
    dax_query <- do.call(buildDaxQuery, query_args)
  }

  api_url <- paste(
    "https://api.powerbi.com/v1.0/myorg",
    "datasets",
    dataset_id,
    "executeQueries",
    sep = "/"
  )

  req_body <- list(
    queries = list(list(query = dax_query)),
    serializerSettings = list(includeNulls = TRUE)
  )

  mpi_tenant_login_url <- paste("https://login.microsoftonline.com", mpi_tenant_id, sep = "/")
  mpi_tenant_auth_url <- paste0(mpi_tenant_login_url, "/oauth2/v2.0/authorize")
  mpi_tenant_token_url <- paste0(mpi_tenant_login_url, "/oauth2/v2.0/token")
  overall_dataset_read_scope <- paste(powerbi_rest_api_resource_url, dataset_read_scope, sep = "/")

  local_r_code_client <- httr2::oauth_client(
    local_r_code_app_id,
    token_url = mpi_tenant_token_url
  )

  req <- httr2::request(api_url) |>
    httr2::req_body_json(req_body) |>
    httr2::req_oauth_auth_code(
      client = local_r_code_client,
      auth_url = mpi_tenant_auth_url,
      scope = overall_dataset_read_scope
    )

  resp <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(resp)
  resp_error <- resp_body$results[[1]]$error

  if (!is.null(resp_error)) {
    error_code <- resp_error$code
    error_message <- resp_error$message
    warning(paste(error_code, error_message, "Result might have been truncated", sep = "\n"))
  }

  result_rows <- resp_body$results[[1]]$tables[[1]]$rows

  df <- do.call(rbind, result_rows) |>
    as.data.frame() |>
    tidyr::unnest(dplyr::everything())

  extract_col_name <- function(col_reference) {
    stringr::str_extract(col_reference, "(?<=\\[).+(?=\\]$)")
  }

  dplyr::rename_with(df, extract_col_name)
}

getWebData <- function(url) {
  file_ext <- sub(".*/[^/]+\\.(.*)", ".\\1", url)

  dir_path <- tempfile()
  dir.create(file.path(dir_path, "tmp_data"), recursive = TRUE)
  tmp_fp <- file.path(dir_path, "tmp_data", paste0("tmp", file_ext))
  cat(file = tmp_fp)

  if (file_ext == ".csv") {
    download.file(url, tmp_fp)
    dat <- read.csv(tmp_fp, stringsAsFactors = FALSE, na.strings = c("", ".", "NA"), check.names = FALSE)
  }

  if (file_ext %in% c(".xlsx", ".xls")) {
    download.file(url, tmp_fp, mode = "wb")
    tab_names <- readxl::excel_sheets(path = tmp_fp)
    dat <- lapply(tab_names, function(x) readxl::read_excel(path = tmp_fp, sheet = x))
    names(dat) <- tab_names
  }

  if (file_ext == ".zip") {
    download.file(url, tmp_fp)
    tmp_unzipped <- utils::unzip(tmp_fp, exdir = file.path(dir_path, "tmp_data"))
    dat_list <- lapply(
      tmp_unzipped,
      read.csv,
      stringsAsFactors = FALSE,
      na.strings = c("", ".", "NA"),
      check.names = FALSE
    )

    if (length(dat_list) > 1) {
      names(dat_list) <- gsub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", tmp_unzipped)
      dat <- dat_list
    } else {
      dat <- as.data.frame(do.call(rbind, dat_list))
    }
  }

  unlink(dir_path, recursive = TRUE)

  dat
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

expand_user_path <- function(path) {
  if (is_blank(path)) return(path)

  path <- as.character(path)
  user_profile <- Sys.getenv("USERPROFILE", unset = "")
  if (nzchar(trimws(user_profile))) {
    path <- sub("^~(?=$|[/\\\\])", gsub("\\\\", "/", user_profile), path, perl = TRUE)
  }

  path.expand(path)
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
  if (is_blank(root)) NULL else expand_user_path(root)
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

  path <- expand_user_path(path)

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
