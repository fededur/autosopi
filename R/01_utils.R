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

read_metadata <- function(path, sheet = 1) {
  raw <- read_xlsx(path, sheet = sheet)
  
  clean <- raw %>%
    janitor::clean_names() %>%
    mutate(
      include = coalesce(as.logical(include), TRUE),
      level = case_when(
        forecast_group_key == "Total" ~ "grand_total",
        str_detect(forecast_group_key, regex("^Total ", ignore_case = TRUE)) ~ "sector_total",
        TRUE ~ "forecast_group"
      )
    )
  
  return(clean)
}

build_color_palette <- function(categories, colors, other_category = NULL, other_color = NULL) {
  
  if (length(categories) != length(colors)) {
    warning(sprintf("Length mismatch: %d categories but %d colors provided.", length(categories), length(colors)))
  }
  
  palette <- setNames(colors[seq_along(categories)], categories)
  
  if (!is.null(other_category) && !is.null(other_color)) {
    palette[other_category] <- other_color
  }
  
  return(palette)
}

get_palette <- function(
    level = c("sector", "forecast_group"),
    sector = NULL,
    ref = c("label", "key"),
    metadata_table,
    include_total = FALSE) {
  
  library(dplyr)
  library(stringr)
  library(rlang)
  
  level <- match.arg(level)
  
  ref <- match.arg(ref)
  
  df <- metadata_table
  
  if (!is.null(sector)) {
    df <- df %>% filter(sector_key %in% sector)
  }
  
  label_col <- switch(level,
                      sector = if (ref == "label") "sector_label" else "sector_key",
                      forecast_group = if (ref == "label") "forecast_group_label" else "forecast_group_key"
  )
  
  color_col <- switch(level,
                      sector = "sector_color",
                      forecast_group = "forecast_group_color"
  )
  
  if (level == "sector") {
    sector_total <- df %>% filter(level == "sector_total")
    df <- sector_total %>%
      distinct(across(c(sector_key, sector_label, sector_color))) %>%
      transmute(label = .data[[label_col]], color = .data[[color_col]])
    
    if (include_total) {
      grand_total <- metadata_table %>% filter(level == "grand_total")
      grand <- grand_total %>%
        transmute(label = .data[[label_col]], color = .data[[color_col]])
      df <- bind_rows(grand, df)
    }
    
  } else if (level == "forecast_group") {
    fg <- df %>% filter(level == "forecast_group")
    
    if (include_total) {
      st <- df %>% filter(level == "sector_total")
      fg <- bind_rows(st, fg)
    }
    
    df <- fg %>%
      transmute(label = .data[[label_col]], color = .data[[color_col]])
  }
  
  if (!include_total) {
    df <- df %>% filter(!str_detect(tolower(label), "total"))
  }
  
  pal <- setNames(df$color, df$label)
  pal[!duplicated(names(pal))]
}