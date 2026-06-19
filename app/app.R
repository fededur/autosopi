required_app_packages <- c("shiny", "dplyr", "forcats", "ggplot2", "lubridate", "openxlsx", "readxl", "rlang", "scales", "svglite", "tibble", "tidyr")
missing_app_packages <- required_app_packages[
  !vapply(required_app_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_app_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_app_packages, collapse = ", "),
    "\nInstall them before running the Shiny app.",
    call. = FALSE
  )
}

library(shiny)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  if (file.exists(current) && !dir.exists(current)) {
    current <- dirname(current)
  }

  repeat {
    if (
      file.exists(file.path(current, "run_charts.R")) &&
        dir.exists(file.path(current, "R", "plot_functions")) &&
        dir.exists(file.path(current, "R", "data_functions"))
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_root <- find_project_root()

source(file.path(project_root, "R", "00_packages.R"))
source(file.path(project_root, "R", "01_utils.R"))
source(file.path(project_root, "R", "02_styling.R"))
source(file.path(project_root, "R", "03_config.R"))
source(file.path(project_root, "R", "04_data_sources.R"))
source(file.path(project_root, "R", "05_outputs.R"))
source(file.path(project_root, "R", "06_runner.R"))

source_directory(file.path(project_root, "R", "data_functions"))
source_directory(file.path(project_root, "R", "plot_functions"))

sopi_sectors <- c(
  "Macro",
  "Dairy",
  "Meat and Wool",
  "Forestry",
  "Horticulture",
  "Seafood",
  "Arable",
  "Other foods"
)

list_r_function_names <- function(path, include_plot_aliases = FALSE) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  names <- character()

  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    top_level <- lines[!grepl("^\\s", lines)]

    function_defs <- sub(
      "^([A-Za-z.][A-Za-z0-9._]*)\\s*(<-|=)\\s*function\\s*\\(.*$",
      "\\1",
      grep(
        "^([A-Za-z.][A-Za-z0-9._]*)\\s*(<-|=)\\s*function\\s*\\(",
        top_level,
        value = TRUE
      )
    )

    names <- c(names, function_defs)

    if (include_plot_aliases) {
      aliases <- sub(
        "^(plot[A-Za-z0-9._]*|[A-Za-z0-9._]*_plot)\\s*(<-|=)\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*$",
        "\\1",
        grep(
          "^(plot[A-Za-z0-9._]*|[A-Za-z0-9._]*_plot)\\s*(<-|=)\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*$",
          top_level,
          value = TRUE
        )
      )
      names <- c(names, aliases)
    }
  }

  sort(unique(names[nzchar(names)]))
}

plot_function_names <- list_r_function_names(
  file.path(project_root, "R", "plot_functions"),
  include_plot_aliases = TRUE
)

data_function_names <- list_r_function_names(
  file.path(project_root, "R", "data_functions"),
  include_plot_aliases = FALSE
)
data_function_names <- data_function_names[vapply(data_function_names, function(function_name) {
  if (!exists(function_name, mode = "function")) return(FALSE)
  first_arg <- names(formals(get(function_name, mode = "function")))[[1]]
  !identical(first_arg, "data")
}, logical(1))]

parse_extra_args <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(list())

  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  args <- list()
  for (line in lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) next

    key <- trimws(parts[[1]])
    value <- trimws(paste(parts[-1], collapse = "="))
    if (!nzchar(key)) next

    args[[key]] <- parse_guess(value)
  }

  args
}

parse_guess <- function(value) {
  if (!nzchar(value)) return(NULL)
  lower <- tolower(value)

  if (lower %in% c("true", "t", "yes", "y")) return(TRUE)
  if (lower %in% c("false", "f", "no", "n")) return(FALSE)

  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.na(numeric_value) && grepl("^-?[0-9.]+$", value)) return(numeric_value)

  value
}

is_hex_colour <- function(value) {
  grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", trimws(value))
}

parse_custom_palette_text <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character())

  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  items <- character()
  colours <- character()

  for (line in lines) {
    parts <- if (grepl("=", line, fixed = TRUE)) {
      strsplit(line, "=", fixed = TRUE)[[1]]
    } else {
      strsplit(line, ",", fixed = TRUE)[[1]]
    }

    if (length(parts) < 2) next

    item <- trimws(parts[[1]])
    colour <- trimws(paste(parts[-1], collapse = if (grepl("=", line, fixed = TRUE)) "=" else ","))

    if (!nzchar(item) || !nzchar(colour)) next
    if (!is_hex_colour(colour)) {
      stop("Invalid hex colour for '", item, "': ", colour, call. = FALSE)
    }

    items <- c(items, item)
    colours <- c(colours, colour)
  }

  if (length(items) == 0) return(character())

  palette <- stats::setNames(colours, items)
  palette[!duplicated(names(palette))]
}

format_palette_text <- function(palette) {
  if (is.null(palette) || length(palette) == 0) return("")
  paste(paste(names(palette), unname(palette), sep = " = "), collapse = "\n")
}

palette_choices_from_metadata <- function(metadata_resource) {
  if (is.null(metadata_resource) || is.null(metadata_resource$custom_palettes)) {
    return(character())
  }

  palettes <- unique(metadata_resource$custom_palettes$palette)
  sort(palettes[!is.na(palettes) & nzchar(palettes)])
}

selected_palette_name <- function(input) {
  mode <- input$palette_mode %||% "metadata"

  if (identical(mode, "saved")) {
    name <- input$saved_palette
  } else if (identical(mode, "custom")) {
    name <- input$custom_palette_name
  } else {
    name <- NULL
  }

  if (is.null(name) || !nzchar(trimws(name))) NULL else trimws(name)
}

custom_palette_for_preview <- function(input, data = NULL) {
  mode <- input$palette_mode %||% "metadata"

  if (identical(mode, "custom")) {
    palette <- parse_custom_palette_text(input$custom_palette_text)
    if (length(palette) == 0) return(NULL)
    return(palette)
  }

  if (identical(mode, "saved")) {
    return(selected_palette_name(input))
  }

  NULL
}

add_palette_args_for_function <- function(args, palette, function_name) {
  if (is.null(palette)) return(args)

  fn_formals <- names(formals(get(function_name, mode = "function")))
  palette_args <- intersect(
    c("palette", "palette_fill", "palette_line", "fill_palette", "colour_palette"),
    fn_formals
  )

  for (arg_name in palette_args) {
    args[[arg_name]] <- palette
  }

  args
}

safe_input_id <- function(prefix, arg_name) {
  paste(prefix, gsub("[^A-Za-z0-9_]+", "_", arg_name), sep = "__")
}

function_extra_args <- function(function_name, exclude) {
  if (!exists(function_name, mode = "function")) return(character())

  args <- names(formals(get(function_name, mode = "function")))
  setdiff(args, c(exclude, "..."))
}

formal_default_text <- function(function_name, arg_name) {
  default <- formals(get(function_name, mode = "function"))[[arg_name]]

  if (is.symbol(default) && identical(as.character(default), "")) {
    return("")
  }

  if (is.null(default)) {
    return("")
  }

  if (is.character(default) && length(default) == 1) {
    return(default)
  }

  if (is.numeric(default) && length(default) == 1) {
    return(default)
  }

  if (is.logical(default) && length(default) == 1) {
    return(default)
  }

  ""
}

function_argument_inputs <- function(function_name, prefix, exclude) {
  args <- function_extra_args(function_name, exclude)

  if (length(args) == 0) {
    return(tags$p(class = "sopi-note", "No extra arguments detected for this function."))
  }

  tagList(lapply(args, function(arg) {
    default <- formal_default_text(function_name, arg)
    id <- safe_input_id(prefix, arg)

    if (is.logical(default)) {
      checkboxInput(id, arg, value = default)
    } else if (is.numeric(default)) {
      numericInput(id, arg, value = default)
    } else {
      textInput(id, arg, value = as.character(default))
    }
  }))
}

collect_function_arguments <- function(input, function_name, prefix, exclude) {
  args <- function_extra_args(function_name, exclude)
  values <- list()

  for (arg in args) {
    id <- safe_input_id(prefix, arg)
    value <- input[[id]]

    if (is.null(value)) next
    if (is.character(value) && !nzchar(trimws(value))) next

    values[[arg]] <- if (is.character(value)) parse_guess(value) else value
  }

  values
}

project_args <- function(input) {
  list(
    sector = input$sector,
    release_year = input$release_year,
    release_round = input$release_round,
    family = input$font_family
  )
}

data_context_args <- function(input) {
  list(
    sector = input$sector
  )
}

chart_context_args <- function(input) {
  list(
    sector = input$sector,
    family = input$font_family,
    base_size = input$base_size,
    fontsize = input$base_size,
    forecast_start_year = input$forecast_start_year,
    forecast_end_year = input$forecast_end_year,
    forecast_start = input$forecast_start_year,
    forecast_end = input$forecast_end_year
  )
}

empty_config <- function() {
  list(
    palettes = data.frame(
      palette = character(),
      item = character(),
      hex = character(),
      notes = character()
    )
  )
}

field_choices <- function(data) {
  if (is.null(data)) return(character())
  names(data)
}

optional_choices <- function(data) {
  c("None" = "", field_choices(data))
}

default_releases_root <- function(project_root) {
  env_root <- Sys.getenv("SOPI_RELEASES_ROOT", unset = "")
  if (nzchar(trimws(env_root))) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  user_profile <- Sys.getenv("USERPROFILE", unset = "")
  if (nzchar(trimws(user_profile))) {
    return(normalizePath(file.path(user_profile, "Documents", "outputs", "SOPI_releases"), winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path(project_root, "SOPI_releases"), winslash = "/", mustWork = FALSE)
}

safe_config_id <- function(value, fallback = "chart") {
  value <- if (is.null(value) || !nzchar(trimws(value))) fallback else value
  value <- tools::file_path_sans_ext(basename(value))
  value <- tolower(gsub("[^A-Za-z0-9_]+", "_", value))
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) fallback else value
}

normalize_svg_filename <- function(value) {
  value <- if (is.null(value) || !nzchar(trimws(value))) "preview_chart.svg" else trimws(value)
  if (!grepl("\\.svg$", value, ignore.case = TRUE)) {
    value <- paste0(value, ".svg")
  }
  value
}

is_absolute_path <- function(path) {
  grepl("^([A-Za-z]:|/|\\\\\\\\)", path)
}

resolve_output_base_path <- function(project_root, path) {
  if (is.null(path) || !nzchar(trimws(path))) {
    return(normalizePath(file.path(project_root, "outputs"), winslash = "/", mustWork = FALSE))
  }

  path <- trimws(path)

  if (is_absolute_path(path)) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(project_root, path), winslash = "/", mustWork = FALSE)
  }
}

path_context_from_input <- function(input) {
  list(
    year = input$release_year,
    release_year = input$release_year,
    release = input$release_round,
    release_round = input$release_round,
    sector = input$sector
  )
}

render_app_path_template <- function(template, input) {
  if (is.null(template) || !nzchar(trimws(template))) return("")

  rendered <- trimws(template)
  context <- path_context_from_input(input)

  for (name in names(context)) {
    rendered <- gsub(paste0("\\{", name, "\\}"), as.character(context[[name]]), rendered)
  }

  rendered
}

resolve_release_path <- function(project_root, releases_root, relative_template, input) {
  rendered <- render_app_path_template(relative_template, input)

  if (is_absolute_path(rendered)) {
    return(normalizePath(rendered, winslash = "/", mustWork = FALSE))
  }

  releases_root <- resolve_output_base_path(project_root, releases_root)
  normalizePath(file.path(releases_root, rendered), winslash = "/", mustWork = FALSE)
}

portable_release_path <- function(relative_template) {
  if (is.null(relative_template) || !nzchar(trimws(relative_template))) {
    return("{SOPI_RELEASES_ROOT}")
  }

  relative_template <- trimws(relative_template)
  if (grepl("^\\{SOPI_RELEASES_ROOT\\}", relative_template)) {
    return(relative_template)
  }

  paste0("{SOPI_RELEASES_ROOT}/", gsub("^[/\\\\]+", "", relative_template))
}

build_output_folder <- function(project_root, releases_root, output_folder_template, input) {
  resolve_release_path(project_root, releases_root, output_folder_template, input)
}

build_manual_data_workbook_path <- function(project_root, releases_root, manual_data_workbook_template, input) {
  resolve_release_path(project_root, releases_root, manual_data_workbook_template, input)
}

selected_excel_path <- function(input, project_root) {
  if (isTRUE(input$use_release_sector_workbook)) {
    return(build_manual_data_workbook_path(
      project_root = project_root,
      releases_root = input$releases_root,
      manual_data_workbook_template = input$manual_data_workbook_template,
      input = input
    ))
  }

  path <- input$excel_path
  if (is.null(path) || !nzchar(trimws(path))) return(path)
  if (is_absolute_path(path)) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(project_root, path), winslash = "/", mustWork = FALSE)
  }
}

build_app_output_path <- function(project_root, releases_root, output_folder_template, input, output_file) {
  file.path(
    build_output_folder(project_root, releases_root, output_folder_template, input),
    output_file
  )
}

list_output_files <- function(folder) {
  if (!dir.exists(folder)) {
    return(data.frame(
      name = character(),
      path = character(),
      size_kb = numeric(),
      modified = character()
    ))
  }

  files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]

  if (length(files) == 0) {
    return(data.frame(
      name = character(),
      path = character(),
      size_kb = numeric(),
      modified = character()
    ))
  }

  info <- file.info(files)
  data.frame(
    name = gsub("\\\\", "/", sub(paste0("^", normalizePath(folder, winslash = "/", mustWork = FALSE), "/?"), "", normalizePath(files, winslash = "/", mustWork = FALSE))),
    path = normalizePath(files, winslash = "/", mustWork = FALSE),
    size_kb = round(info$size / 1024, 1),
    modified = format(info$mtime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
}

safe_output_delete <- function(path, folder) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  normalized_folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)

  if (!startsWith(normalized_path, paste0(normalized_folder, "/")) && normalized_path != normalized_folder) {
    stop("Refusing to delete a file outside the selected output folder.", call. = FALSE)
  }

  if (dir.exists(normalized_path)) {
    stop("Refusing to delete a folder. Select a file instead.", call. = FALSE)
  }

  unlink(normalized_path)
  normalized_path
}

release_config_path <- function(project_root, release_year, release_round) {
  round_folder <- gsub("[^A-Za-z0-9_-]+", "_", as.character(release_round))
  file.path(
    project_root,
    "config",
    "releases",
    as.character(release_year),
    round_folder,
    "chart_config.xlsx"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE)
}

empty_config_table <- function(columns) {
  stats::setNames(
    as.data.frame(rep(list(character()), length(columns)), stringsAsFactors = FALSE),
    columns
  )
}

read_config_table_or_empty <- function(path, sheet, columns) {
  if (!file.exists(path) || !sheet %in% readxl::excel_sheets(path)) {
    return(empty_config_table(columns))
  }

  tbl <- readxl::read_excel(path, sheet = sheet, .name_repair = "unique_quiet")
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)

  for (column in setdiff(columns, names(tbl))) {
    tbl[[column]] <- NA_character_
  }

  tbl <- tbl[, columns, drop = FALSE]
  tbl[] <- lapply(tbl, as.character)
  tbl
}

upsert_rows <- function(tbl, rows, key_col, key_value) {
  tbl <- tbl[is.na(tbl[[key_col]]) | tbl[[key_col]] != key_value, , drop = FALSE]
  rows <- rows[, names(tbl), drop = FALSE]
  dplyr::bind_rows(tbl, rows)
}

upsert_table_by_keys <- function(tbl, rows, key_col) {
  if (nrow(rows) == 0) return(tbl)
  tbl <- tbl[is.na(tbl[[key_col]]) | !tbl[[key_col]] %in% rows[[key_col]], , drop = FALSE]
  rows <- rows[, names(tbl), drop = FALSE]
  dplyr::bind_rows(tbl, rows)
}

next_sort_order <- function(plots, sector) {
  if (nrow(plots) == 0 || !"sort_order" %in% names(plots)) return(1)

  sector_orders <- suppressWarnings(as.numeric(plots$sort_order[plots$sector == sector]))
  sector_orders <- sector_orders[!is.na(sector_orders)]

  if (length(sector_orders) == 0) 1 else max(sector_orders) + 1
}

config_value_type <- function(value) {
  if (is.logical(value)) return("logical")
  if (is.numeric(value) && length(value) == 1 && !is.na(value) && value == as.integer(value)) return("integer")
  if (is.numeric(value)) return("numeric")
  "character"
}

format_config_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_character_)
  if (is.logical(value)) return(ifelse(isTRUE(value), "TRUE", "FALSE"))
  if (length(value) > 1) return(paste(value, collapse = ","))
  as.character(value)
}

args_to_config_rows <- function(id_col, id_value, args, notes = "") {
  args <- args[!vapply(args, is.null, logical(1))]
  args <- args[!vapply(args, function(value) {
    is.character(value) && length(value) == 1 && !nzchar(trimws(value))
  }, logical(1))]

  columns <- c(id_col, "arg_name", "arg_value", "arg_type", "notes")
  if (length(args) == 0) return(empty_config_table(columns))

  rows <- data.frame(
    id = id_value,
    arg_name = names(args),
    arg_value = vapply(args, format_config_value, character(1)),
    arg_type = vapply(args, config_value_type, character(1)),
    notes = notes,
    stringsAsFactors = FALSE
  )
  names(rows) <- columns
  rows
}

build_release_settings_table <- function(input) {
  values <- list(
    release_year = input$release_year,
    release_round = input$release_round,
    output_root = portable_release_path(input$output_folder_template),
    manual_data_workbook_template = portable_release_path(input$manual_data_workbook_template),
    file_type = "svg",
    family = input$font_family,
    base_size = input$base_size,
    overwrite = TRUE
  )

  notes <- c(
    release_year = "SOPI release calendar year",
    release_round = "SOPI release round: June or December",
    output_root = "Portable graph output folder template",
    manual_data_workbook_template = "Portable manual data workbook template",
    file_type = "Runner currently writes SVG",
    family = "Default chart font family",
    base_size = "Default chart base font size",
    overwrite = "Reserved for future use"
  )

  data.frame(
    setting_name = names(values),
    setting_value = vapply(values, format_config_value, character(1)),
    setting_type = vapply(values, config_value_type, character(1)),
    notes = unname(notes[names(values)]),
    stringsAsFactors = FALSE
  )
}

build_sector_settings_table <- function(existing = NULL) {
  columns <- c("sector", "active", "palette", "output_subfolder", "notes")

  if (!is.null(existing) && nrow(existing) > 0) {
    existing <- existing[, columns, drop = FALSE]
    missing <- setdiff(sopi_sectors, existing$sector)
  } else {
    existing <- empty_config_table(columns)
    missing <- sopi_sectors
  }

  if (length(missing) == 0) return(existing)

  dplyr::bind_rows(
    existing,
    data.frame(
      sector = missing,
      active = "TRUE",
      palette = NA_character_,
      output_subfolder = missing,
      notes = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

build_data_source_config <- function(input, data_source_id) {
  if (identical(input$data_source_type, "function")) {
    data.frame(
      data_source_id = data_source_id,
      source_type = "function",
      source_ref = NA_character_,
      sheet = NA_character_,
      range = NA_character_,
      data_function = input$data_function,
      cache = "FALSE",
      notes = "Created from Shiny app",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      data_source_id = data_source_id,
      source_type = "excel",
      source_ref = if (isTRUE(input$use_release_sector_workbook)) {
        portable_release_path(input$manual_data_workbook_template)
      } else {
        input$excel_path
      },
      sheet = input$excel_sheet,
      range = if (is_blank(input$excel_range)) NA_character_ else input$excel_range,
      data_function = NA_character_,
      cache = "FALSE",
      notes = "Created from Shiny app",
      stringsAsFactors = FALSE
    )
  }
}

build_data_args_config <- function(input, data_source_id) {
  if (!identical(input$data_source_type, "function")) {
    return(empty_config_table(c("data_source_id", "arg_name", "arg_value", "arg_type", "notes")))
  }

  args <- merge_args(
    data_context_args(input),
    collect_function_arguments(input, input$data_function, "data_arg", data_standard_exclusions()),
    parse_extra_args(input$data_extra_args)
  )

  args_to_config_rows("data_source_id", data_source_id, args, "Created from Shiny app")
}

build_plot_config <- function(input, plot_id, data_source_id, existing_plots) {
  existing_plot <- existing_plots[existing_plots$plot_id == plot_id, , drop = FALSE]
  sort_order <- if (nrow(existing_plot) > 0 && !is_blank(existing_plot$sort_order[[1]])) {
    existing_plot$sort_order[[1]]
  } else {
    as.character(next_sort_order(existing_plots, input$sector))
  }

  data.frame(
    plot_id = plot_id,
    sector = input$sector,
    active = "TRUE",
    plot_function = input$plot_function,
    data_source_id = data_source_id,
    output_file = normalize_svg_filename(input$output_file),
    title = NA_character_,
    subtitle = NA_character_,
    sort_order = sort_order,
    notes = "Created from Shiny app",
    stringsAsFactors = FALSE
  )
}

build_plot_args_config <- function(input, plot_id) {
  args <- merge_args(
    chart_context_args(input),
    collect_function_arguments(input, input$plot_function, "plot_arg", plot_standard_exclusions()),
    parse_extra_args(input$plot_extra_args)
  )

  palette_name <- selected_palette_name(input)
  if (!is.null(palette_name)) {
    args <- add_palette_args_for_function(args, palette_name, input$plot_function)
  } else {
    args$use_metadata_palette <- TRUE
  }

  args$x <- input$x_field
  args$date_var <- input$x_field

  if (!is_blank(input$group_field)) {
    args$group <- input$group_field
  }

  if (!is_blank(input$column_value)) {
    args$y_col <- input$column_value
    args$y <- input$column_value
  }

  if (!is_blank(input$line_value)) {
    args$y_line <- input$line_value
  }

  if (!is.null(input$driver_field) && !is_blank(input$driver_field)) {
    args$driver <- input$driver_field
  }

  if (!is.null(input$total_field) && !is_blank(input$total_field)) {
    args$total <- input$total_field
  }

  args$x_freq <- input$x_freq
  args$y_col_label <- input$column_axis_label
  args$y_line_label <- input$line_axis_label
  args$y_lab <- input$column_axis_label
  args$col_label <- input$column_legend_label
  args$line_label <- input$line_legend_label
  args$col_position <- input$column_position
  args$forecast <- isTRUE(input$forecast)
  args$primary_min_breaks <- input$primary_min_breaks
  args$primary_max_breaks <- input$primary_max_breaks
  args$secondary_min_breaks <- input$secondary_min_breaks
  args$secondary_max_breaks <- input$secondary_max_breaks
  args$width <- input$width
  args$height <- input$height

  args_to_config_rows("plot_id", plot_id, args, "Created from Shiny app")
}

build_selected_palette_rows <- function(input, metadata_resource) {
  palette_name <- selected_palette_name(input)
  if (is.null(palette_name)) {
    return(empty_config_table(c("palette", "item", "hex", "notes")))
  }

  if (identical(input$palette_mode, "custom")) {
    palette <- parse_custom_palette_text(input$custom_palette_text)
  } else {
    palette <- palette_from_custom_metadata(metadata_resource, palette_name)
  }

  if (is.null(palette) || length(palette) == 0) {
    return(empty_config_table(c("palette", "item", "hex", "notes")))
  }

  data.frame(
    palette = palette_name,
    item = names(palette),
    hex = unname(palette),
    notes = "Copied from metadata custom_palettes by Shiny app",
    stringsAsFactors = FALSE
  )
}

write_release_config_from_app <- function(input, project_root, plot_id, data_source_id) {
  config_path <- release_config_path(project_root, input$release_year, input$release_round)
  dir.create(dirname(config_path), recursive = TRUE, showWarnings = FALSE)

  sheet_columns <- list(
    release_settings = c("setting_name", "setting_value", "setting_type", "notes"),
    settings_sector = c("sector", "active", "palette", "output_subfolder", "notes"),
    plots = c("plot_id", "sector", "active", "plot_function", "data_source_id", "output_file", "title", "subtitle", "sort_order", "notes"),
    plot_args = c("plot_id", "arg_name", "arg_value", "arg_type", "notes"),
    data_sources = c("data_source_id", "source_type", "source_ref", "sheet", "range", "data_function", "cache", "notes"),
    data_args = c("data_source_id", "arg_name", "arg_value", "arg_type", "notes"),
    run_control = c("setting_name", "setting_value", "setting_type", "notes"),
    palettes = c("palette", "item", "hex", "notes")
  )

  tables <- lapply(names(sheet_columns), function(sheet) {
    read_config_table_or_empty(config_path, sheet, sheet_columns[[sheet]])
  })
  names(tables) <- names(sheet_columns)

  tables$release_settings <- upsert_table_by_keys(
    tables$release_settings,
    build_release_settings_table(input),
    "setting_name"
  )
  tables$settings_sector <- build_sector_settings_table(tables$settings_sector)
  tables$data_sources <- upsert_rows(
    tables$data_sources,
    build_data_source_config(input, data_source_id),
    "data_source_id",
    data_source_id
  )
  tables$data_args <- upsert_rows(
    tables$data_args,
    build_data_args_config(input, data_source_id),
    "data_source_id",
    data_source_id
  )
  tables$plots <- upsert_rows(
    tables$plots,
    build_plot_config(input, plot_id, data_source_id, tables$plots),
    "plot_id",
    plot_id
  )
  tables$plot_args <- upsert_rows(
    tables$plot_args,
    build_plot_args_config(input, plot_id),
    "plot_id",
    plot_id
  )
  selected_palette_rows <- build_selected_palette_rows(input, load_metadata_resource(project_root))
  if (nrow(selected_palette_rows) > 0) {
    tables$palettes <- upsert_rows(
      tables$palettes,
      selected_palette_rows,
      "palette",
      selected_palette_rows$palette[[1]]
    )
  }

  if (nrow(tables$run_control) == 0) {
    tables$run_control <- data.frame(
      setting_name = c("run_all_active", "sector_filter", "plot_id_filter", "dry_run", "save_logs"),
      setting_value = c("TRUE", NA_character_, NA_character_, "FALSE", "TRUE"),
      setting_type = c("logical", "character_vector", "character_vector", "logical", "logical"),
      notes = c(
        "Run active plots in active sectors",
        "Optional comma-separated sectors",
        "Optional comma-separated plot IDs",
        "Validate and build plots without saving SVGs",
        "Write logs/chart_run_log.csv"
      ),
      stringsAsFactors = FALSE
    )
  }

  wb <- openxlsx::createWorkbook()
  for (sheet in names(tables)) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tables[[sheet]], withFilter = TRUE)
  }

  openxlsx::saveWorkbook(wb, config_path, overwrite = TRUE)
  config_path
}

first_matching_field <- function(fields, pattern) {
  matches <- fields[grepl(pattern, fields, ignore.case = TRUE)]
  if (length(matches) == 0) "" else matches[[1]]
}

build_data <- function(input) {
  if (identical(input$data_source_type, "function")) {
    args <- merge_args(
      project_args(input),
      data_context_args(input),
      collect_function_arguments(input, input$data_function, "data_arg", data_standard_exclusions()),
      parse_extra_args(input$data_extra_args),
      list(project_root = project_root)
    )
    call_named_function(input$data_function, args)
  } else {
    path <- selected_excel_path(input, project_root)

    if (!file.exists(path)) {
      stop("Excel data file not found: ", path, call. = FALSE)
    }

    if (is_blank(input$excel_range)) {
      readxl::read_excel(path, sheet = input$excel_sheet)
    } else {
      readxl::read_excel(path, sheet = input$excel_sheet, range = input$excel_range)
    }
  }
}

build_plot <- function(input, data) {
  args <- merge_args(
    project_args(input),
    chart_context_args(input),
    collect_function_arguments(input, input$plot_function, "plot_arg", plot_standard_exclusions()),
    parse_extra_args(input$plot_extra_args)
  )
  fn_formals <- names(formals(get(input$plot_function, mode = "function")))
  preview_palette <- custom_palette_for_preview(input, data)

  if (!is.null(preview_palette)) {
    args <- add_palette_args_for_function(args, preview_palette, input$plot_function)
  } else {
    args$use_metadata_palette <- TRUE
  }

  args$x <- input$x_field
  args$date_var <- input$x_field

  if (!is_blank(input$group_field)) {
    args$group <- input$group_field
  }

  if (!is_blank(input$column_value)) {
    if ("y_col" %in% fn_formals) {
      args$y_col <- input$column_value
    }
    if ("y" %in% fn_formals) {
      args$y <- input$column_value
    }
  }

  if (!is_blank(input$line_value)) {
    args$y_line <- input$line_value
  }

  if (!is.null(input$driver_field) && !is_blank(input$driver_field)) {
    args$driver <- input$driver_field
  }

  if (!is.null(input$total_field) && !is_blank(input$total_field)) {
    args$total <- input$total_field
  }

  args$x_freq <- input$x_freq
  args$y_col_label <- input$column_axis_label
  args$y_line_label <- input$line_axis_label
  args$y_lab <- input$column_axis_label
  args$col_label <- input$column_legend_label
  args$line_label <- input$line_legend_label
  args$col_position <- input$column_position
  args$forecast <- isTRUE(input$forecast)
  args$primary_min_breaks <- input$primary_min_breaks
  args$primary_max_breaks <- input$primary_max_breaks
  args$secondary_min_breaks <- input$secondary_min_breaks
  args$secondary_max_breaks <- input$secondary_max_breaks

  plot_args <- clean_plot_args(
    args = args,
    config = empty_config(),
    project_root = project_root,
    data = data,
    metadata_resource = load_metadata_resource(project_root)
  )
  plot_args$data <- data

  call_named_function(input$plot_function, plot_args)
}

data_standard_exclusions <- function() {
  c(
    "sector",
    "project_root"
  )
}

plot_standard_exclusions <- function() {
  c(
    "data",
    "x",
    "date_var",
    "y",
    "y_col",
    "y_line",
    "group",
    "driver",
    "total",
    "x_freq",
    "family",
    "base_size",
    "fontsize",
    "forecast",
    "forecast_start",
    "forecast_end",
    "palette",
    "palette_fill",
    "palette_line",
    "fill_palette",
    "colour_palette",
    "labels",
    "y_col_label",
    "y_line_label",
    "y_lab",
    "col_label",
    "line_label",
    "col_position",
    "primary_min_breaks",
    "primary_max_breaks",
    "secondary_min_breaks",
    "secondary_max_breaks"
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f7f7f7; }
      .container-fluid { padding-left: 12px; padding-right: 12px; }
      h2 { margin-top: 8px; margin-bottom: 8px; font-size: 24px; }
      h4 { margin-top: 4px; margin-bottom: 8px; font-size: 16px; }
      .well { background: white; border-radius: 6px; padding: 10px; margin-bottom: 8px; }
      .form-group { margin-bottom: 8px; }
      .checkbox { margin-top: 4px; margin-bottom: 6px; }
      .btn { padding: 5px 10px; }
      .tab-content { padding-top: 8px; }
      .sopi-note { color: #555; font-size: 0.88em; margin-bottom: 6px; }
      .sopi-path { white-space: pre-wrap; max-height: 68px; overflow: auto; font-size: 0.88em; }
      .sopi-scroll { max-height: calc(100vh - 245px); overflow: auto; }
      .sopi-preview { max-height: calc(100vh - 255px); overflow: auto; background: white; border: 1px solid #ddd; padding: 8px; }
      .sopi-svg-preview { max-height: 320px; display: flex; align-items: center; justify-content: center; }
      .sopi-svg-preview svg { max-width: 100%; max-height: 280px; width: auto !important; height: auto !important; }
      table { font-size: 0.9em; }
    "))
  ),
  titlePanel("SOPI Graphs"),
  tabsetPanel(
    tabPanel(
      "Overview",
      fluidRow(
        column(
          5,
          wellPanel(
            h4("Release"),
            selectInput("sector", "Sector", choices = sopi_sectors, selected = "Seafood"),
            numericInput("release_year", "Release year", value = as.integer(format(Sys.Date(), "%Y")), min = 2000),
            selectInput("release_round", "Release round", choices = c("June", "December"), selected = "June")
          )
        ),
        column(
          7,
          wellPanel(
            h4("SharePoint Paths"),
            textInput(
              "releases_root",
              "Local SOPI releases root",
              value = default_releases_root(project_root)
            ),
            tags$p(class = "sopi-note", "This is the local synced SharePoint root on your machine. It is not saved into chart_config.xlsx."),
            textInput(
              "manual_data_workbook_template",
              "Manual data workbook template",
              value = "{year}/{release}/Data/{sector}/{sector}.xlsx"
            ),
            tags$p(class = "sopi-note", "Templates can use {year}, {release}, and {sector}. Config files save these as portable {SOPI_RELEASES_ROOT} paths."),
            actionButton("refresh_output_files", "Refresh Output Files"),
            tags$p(class = "sopi-note", "Current chart output folder:"),
            verbatimTextOutput("resolved_output_folder")
          )
        )
      ),
      fluidRow(
        column(
          5,
          wellPanel(
            h4("Output files"),
            tags$p(class = "sopi-note", "Shows files inside the selected output folder."),
            uiOutput("output_file_selector"),
            div(class = "sopi-scroll", tableOutput("output_file_table"))
          )
        ),
        column(
          7,
          wellPanel(
            h4("Selected file preview"),
            uiOutput("output_file_preview"),
            fluidRow(
              column(7, checkboxInput("confirm_delete_output", "Delete selected output file", value = FALSE)),
              column(5, actionButton("delete_output_file", "Delete File", class = "btn-danger"))
            ),
            verbatimTextOutput("output_file_status")
          )
        )
      )
    ),
    tabPanel(
      "Data",
      fluidRow(
        column(
          4,
          wellPanel(
            radioButtons("data_source_type", "Data source", choices = c("R function" = "function", "Excel sheet" = "excel")),
            conditionalPanel(
              "input.data_source_type == 'function'",
              selectInput("data_function", "Data function", choices = data_function_names),
              uiOutput("data_function_args"),
              textAreaInput("data_extra_args", "Extra data arguments", value = "seed = 42", rows = 3)
            ),
            conditionalPanel(
              "input.data_source_type == 'excel'",
              checkboxInput("use_release_sector_workbook", "Use release/sector SharePoint workbook", value = TRUE),
              tags$p(class = "sopi-note", "Workbook path:"),
              verbatimTextOutput("resolved_excel_path"),
              conditionalPanel(
                "!input.use_release_sector_workbook",
                textInput("excel_path", "Custom Excel file path", value = "")
              ),
              uiOutput("excel_sheet_selector"),
              textInput("excel_range", "Range, optional", value = "")
            ),
            actionButton("load_data", "Load Data", class = "btn-primary")
          )
        ),
        column(
          8,
          wellPanel(
            h4("Data preview"),
            verbatimTextOutput("data_status"),
            div(class = "sopi-scroll", tableOutput("data_preview"))
          )
        )
      )
    ),
    tabPanel(
      "General Style",
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Typography"),
            textInput("font_family", "Font family", value = "DIN"),
            numericInput("base_size", "Base font size", value = 10.5, min = 6, step = 0.5)
          )
        ),
        column(
          4,
          wellPanel(
            h4("Notes"),
            tags$p(class = "sopi-note", "These settings are common chart styling defaults. They are used when previewing and saving charts, regardless of the selected plot function."),
            tags$p(class = "sopi-note", "Chart-specific labels, axes, forecast settings, SVG export size, preview, and save controls are in the Chart tab.")
          )
        )
      )
    ),
    tabPanel(
      "Chart",
      fluidRow(
        column(
          4,
          wellPanel(
            selectInput("plot_function", "Plot function", choices = plot_function_names),
            uiOutput("field_selectors"),
            uiOutput("plot_function_args"),
            h4("Forecast"),
            checkboxInput("forecast", "Show forecast shading", value = TRUE),
            fluidRow(
              column(6, numericInput("forecast_start_year", "Forecast start year", value = 2026, min = 1990)),
              column(6, numericInput("forecast_end_year", "Forecast end year", value = 2030, min = 1990))
            ),
            fluidRow(
              column(6, selectInput("x_freq", "X frequency", choices = c("auto", "yearly", "quarterly", "monthly"), selected = "auto")),
              column(6, selectInput("column_position", "Column position", choices = c("stacked", "dodge"), selected = "stacked"))
            )
          )
        ),
        column(
          4,
          wellPanel(
            h4("Labels"),
            textInput("column_axis_label", "Column axis label", value = "Export revenue (NZ$ million)"),
            textInput("column_legend_label", "Column legend label", value = "Revenue"),
            textInput("line_axis_label", "Line axis label", value = "Export volume (tonnes)"),
            textInput("line_legend_label", "Line legend label", value = "Volume")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Advanced"),
            fluidRow(
              column(6, numericInput("primary_min_breaks", "Primary min breaks", value = 4, min = 2)),
              column(6, numericInput("primary_max_breaks", "Primary max breaks", value = 6, min = 2))
            ),
            fluidRow(
              column(6, numericInput("secondary_min_breaks", "Secondary min breaks", value = 4, min = 2)),
              column(6, numericInput("secondary_max_breaks", "Secondary max breaks", value = 6, min = 2))
            ),
            textAreaInput("plot_extra_args", "Extra plot arguments", rows = 3)
          )
        )
      ),
      fluidRow(
        column(
          8,
          wellPanel(
            h4("Colours"),
            fluidRow(
              column(
                4,
                selectInput(
                  "palette_mode",
                  "Palette source",
                  choices = c(
                    "Automatic sector metadata" = "metadata",
                    "Saved custom palette" = "saved",
                    "Create/update custom palette" = "custom"
                  ),
                  selected = "metadata"
                ),
                uiOutput("saved_palette_selector"),
                textInput("custom_palette_name", "Custom palette name", value = "")
              ),
              column(
                8,
                textAreaInput(
                  "custom_palette_text",
                  "Custom palette items",
                  value = "",
                  rows = 5,
                  placeholder = "Category A = #1f77b4\nCategory B = #ff7f0e"
                ),
                fluidRow(
                  column(4, actionButton("fill_palette_from_data", "Fill From Data")),
                  column(4, actionButton("load_saved_palette", "Load Saved")),
                  column(4, actionButton("save_custom_palette", "Save Palette", class = "btn-success"))
                ),
                tags$p(class = "sopi-note", "Use one item per line: category = #hex. Saved palettes are written to metadata/sopi_metadata.xlsx in the custom_palettes sheet.")
              )
            ),
            verbatimTextOutput("palette_status")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Palette preview"),
            uiOutput("palette_preview")
          )
        )
      ),
      fluidRow(
        column(
          8,
          wellPanel(
            h4("Preview"),
            actionButton("refresh_preview", "Refresh Preview", class = "btn-primary"),
            tags$p(class = "sopi-note", "Click after changing chart parameters or SVG size to rebuild the visual preview."),
            uiOutput("chart_preview_ui")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Save SVG"),
            textInput(
              "output_folder_template",
              "Graph output folder template",
              value = "{year}/{release}/Graphs/{sector}"
            ),
            textInput("output_file", "SVG filename", value = "preview_chart.svg"),
            tags$p(class = "sopi-note", "Resolved SVG save path:"),
            verbatimTextOutput("resolved_svg_path"),
            tags$p(class = "sopi-note", "Export size for this saved SVG."),
            fluidRow(
              column(6, numericInput("width", "Width", value = 9, min = 3, step = 0.5)),
              column(6, numericInput("height", "Height", value = 5, min = 3, step = 0.5))
            ),
            textInput("plot_id", "Plot ID", value = "preview_chart"),
            textInput("data_source_id", "Data source ID", value = "preview_chart_data"),
            checkboxInput("update_chart_config", "Update release chart_config.xlsx", value = TRUE),
            tags$p(class = "sopi-note", "Existing matching IDs are updated; new IDs are appended."),
            verbatimTextOutput("config_path_preview"),
            actionButton("save_svg", "Save Confirmed Chart", class = "btn-success"),
            tags$hr(),
            verbatimTextOutput("save_status")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output_refresh <- reactiveVal(0)
  palette_refresh <- reactiveVal(0)

  metadata_resource <- reactive({
    palette_refresh()
    load_metadata_resource(project_root)
  })

  output_folder <- reactive({
    build_output_folder(
      project_root = project_root,
      releases_root = input$releases_root,
      output_folder_template = input$output_folder_template,
      input = input
    )
  })

  output_files <- reactive({
    output_refresh()
    input$releases_root
    input$output_folder_template
    list_output_files(output_folder())
  })

  observeEvent(list(input$releases_root, input$output_folder_template, input$manual_data_workbook_template), {
    if (!is.null(input$releases_root) && nzchar(trimws(input$releases_root))) {
      Sys.setenv(SOPI_RELEASES_ROOT = normalizePath(input$releases_root, winslash = "/", mustWork = FALSE))
    }
    output_refresh(output_refresh() + 1)
  })

  observeEvent(list(input$release_year, input$release_round, input$sector), {
    output_refresh(output_refresh() + 1)
  }, ignoreInit = TRUE)

  output$saved_palette_selector <- renderUI({
    choices <- palette_choices_from_metadata(metadata_resource())

    if (length(choices) == 0) {
      return(tags$p(class = "sopi-note", "No saved custom palettes yet. Create one below, then click Save Palette."))
    }

    selectInput("saved_palette", "Saved custom palette", choices = choices, selected = choices[[1]])
  })

  current_group_categories <- reactive({
    data <- tryCatch(loaded_data(), error = function(e) NULL)
    if (is.null(data) || is_blank(input$group_field) || !input$group_field %in% names(data)) {
      return(character())
    }

    categories <- unique(as.character(data[[input$group_field]]))
    categories[!is.na(categories) & nzchar(categories)]
  })

  observeEvent(input$fill_palette_from_data, {
    categories <- current_group_categories()
    if (length(categories) == 0) {
      output$palette_status <- renderText("Load data and select a group/category field first.")
      return()
    }

    metadata_style <- style_from_metadata(
      metadata_resource = metadata_resource(),
      sector = input$sector,
      categories = categories
    )

    palette <- complete_palette(categories, metadata_style$palette)
    updateTextAreaInput(session, "custom_palette_text", value = format_palette_text(palette))

    if (is_blank(input$custom_palette_name)) {
      updateTextInput(
        session,
        "custom_palette_name",
        value = paste0(safe_config_id(input$sector, "sector"), "_custom")
      )
    }

    output$palette_status <- renderText("Filled palette from the current data categories.")
  })

  observeEvent(input$load_saved_palette, {
    palette_name <- input$saved_palette
    palette <- palette_from_custom_metadata(metadata_resource(), palette_name)

    if (is.null(palette) || length(palette) == 0) {
      output$palette_status <- renderText("No saved palette selected or the selected palette has no colours.")
      return()
    }

    updateTextInput(session, "custom_palette_name", value = palette_name)
    updateTextAreaInput(session, "custom_palette_text", value = format_palette_text(palette))
    updateSelectInput(session, "palette_mode", selected = "custom")
    output$palette_status <- renderText(paste("Loaded saved palette:", palette_name))
  })

  observeEvent(input$save_custom_palette, {
    result <- tryCatch({
      palette <- parse_custom_palette_text(input$custom_palette_text)
      if (length(palette) == 0) {
        stop("Add at least one palette item before saving.", call. = FALSE)
      }

      path <- write_custom_palette(
        project_root = project_root,
        palette_name = input$custom_palette_name,
        palette = palette,
        sector = input$sector,
        notes = "Created from Shiny app"
      )

      list(ok = TRUE, path = path, palette_name = trimws(input$custom_palette_name))
    }, error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    })

    if (isTRUE(result$ok)) {
      palette_refresh(palette_refresh() + 1)
      updateSelectInput(session, "palette_mode", selected = "saved")
      updateSelectInput(session, "saved_palette", selected = result$palette_name)
      output$palette_status <- renderText({
        paste("Saved palette:", result$palette_name, "\nMetadata workbook:", result$path)
      })
    } else {
      output$palette_status <- renderText(paste("Palette save failed:", result$message))
    }
  })

  output$palette_preview <- renderUI({
    palette <- tryCatch({
      mode <- input$palette_mode %||% "metadata"

      if (identical(mode, "custom")) {
        parse_custom_palette_text(input$custom_palette_text)
      } else if (identical(mode, "saved")) {
        palette_from_custom_metadata(metadata_resource(), input$saved_palette)
      } else {
        categories <- current_group_categories()
        if (length(categories) == 0) {
          style_from_metadata(metadata_resource(), input$sector)$palette
        } else {
          style_from_metadata(metadata_resource(), input$sector, categories = categories)$palette
        }
      }
    }, error = function(e) {
      NULL
    })

    if (is.null(palette) || length(palette) == 0) {
      return(tags$p(class = "sopi-note", "No palette to preview yet."))
    }

    tags$div(lapply(seq_along(palette), function(i) {
      tags$div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:4px;",
        tags$span(style = paste0("display:inline-block; width:18px; height:18px; border:1px solid #ccc; background:", unname(palette[[i]]), ";")),
        tags$span(names(palette)[[i]]),
        tags$code(unname(palette[[i]]))
      )
    }))
  })

  output$resolved_output_folder <- renderText({
    output_folder()
  })

  output$resolved_excel_path <- renderText({
    selected_excel_path(input, project_root)
  })

  excel_sheets <- reactive({
    path <- selected_excel_path(input, project_root)
    if (is.null(path) || !nzchar(trimws(path)) || !file.exists(path)) {
      return(character())
    }

    tryCatch(
      readxl::excel_sheets(path),
      error = function(e) character()
    )
  })

  output$excel_sheet_selector <- renderUI({
    sheets <- excel_sheets()

    if (length(sheets) == 0) {
      return(tags$p(class = "sopi-note", "No sheets found. Check that the workbook exists and is closed if SharePoint is syncing it."))
    }

    selectInput("excel_sheet", "Sheet", choices = sheets, selected = sheets[[1]])
  })

  output$config_path_preview <- renderText({
    release_config_path(project_root, input$release_year, input$release_round)
  })

  resolved_svg_path <- reactive({
    build_app_output_path(
      project_root = project_root,
      releases_root = input$releases_root,
      output_folder_template = input$output_folder_template,
      input = input,
      output_file = normalize_svg_filename(input$output_file)
    )
  })

  output$resolved_svg_path <- renderText({
    resolved_svg_path()
  })

  observeEvent(input$refresh_output_files, {
    output_refresh(output_refresh() + 1)
  })

  output$output_file_selector <- renderUI({
    files <- output_files()

    if (nrow(files) == 0) {
      return(tags$p(class = "sopi-note", paste("No files found in", output_folder())))
    }

    selectInput(
      "selected_output_file",
      "Select file",
      choices = stats::setNames(files$path, files$name),
      selected = files$path[[1]]
    )
  })

  output$output_file_table <- renderTable({
    files <- output_files()
    if (nrow(files) == 0) return(NULL)
    files[, c("name", "size_kb", "modified"), drop = FALSE]
  })

  output$output_file_preview <- renderUI({
    req(input$selected_output_file)
    path <- input$selected_output_file

    if (!file.exists(path)) {
      return(tags$p(class = "sopi-note", "The selected file no longer exists."))
    }

    ext <- tolower(tools::file_ext(path))

    if (ext == "svg") {
      svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
      return(tags$div(
        class = "sopi-preview sopi-svg-preview",
        HTML(svg)
      ))
    }

    if (ext %in% c("txt", "csv", "log", "md")) {
      lines <- readLines(path, warn = FALSE, n = 80)
      return(tags$pre(class = "sopi-preview", paste(lines, collapse = "\n")))
    }

    tags$p(class = "sopi-note", paste("Preview is not available for .", ext, " files. Select an SVG file to preview the chart.", sep = ""))
  })

  observeEvent(input$delete_output_file, {
    req(input$selected_output_file)

    if (!isTRUE(input$confirm_delete_output)) {
      output$output_file_status <- renderText("Tick the confirmation box before deleting.")
      return()
    }

    deleted_path <- tryCatch(
      safe_output_delete(input$selected_output_file, output_folder()),
      error = function(e) e
    )

    if (inherits(deleted_path, "error")) {
      output$output_file_status <- renderText(conditionMessage(deleted_path))
    } else {
      output$output_file_status <- renderText(paste("Deleted:", deleted_path))
      updateCheckboxInput(session, "confirm_delete_output", value = FALSE)
      output_refresh(output_refresh() + 1)
    }
  })

  output$data_function_args <- renderUI({
    req(input$data_function)
    tagList(
      tags$h4("Function arguments"),
      function_argument_inputs(input$data_function, "data_arg", data_standard_exclusions())
    )
  })

  loaded_data <- eventReactive(input$load_data, {
    build_data(input)
  })

  output$data_status <- renderPrint({
    data <- loaded_data()
    cat(nrow(data), "rows x", ncol(data), "columns\n")
    cat(paste(names(data), collapse = ", "))
  })

  output$data_preview <- renderTable({
    utils::head(loaded_data(), 10)
  })

  output$field_selectors <- renderUI({
    data <- loaded_data()
    fields <- field_choices(data)

    if (length(fields) == 0) {
      return(tags$p(class = "sopi-note", "Load data first to choose fields."))
    }

    tagList(
      selectInput("x_field", "X/date/year field", choices = fields, selected = fields[[1]]),
      selectInput("group_field", "Group/category field", choices = optional_choices(data), selected = if ("group" %in% fields) "group" else ""),
      selectInput("column_value", "Column/bar value", choices = optional_choices(data), selected = first_matching_field(fields, "revenue|value")),
      selectInput("line_value", "Line value", choices = optional_choices(data), selected = first_matching_field(fields, "volume")),
      if ("driver" %in% names(formals(get(input$plot_function, mode = "function")))) {
        selectInput("driver_field", "Driver field", choices = fields, selected = first_matching_field(fields, "driver"))
      },
      if ("total" %in% names(formals(get(input$plot_function, mode = "function")))) {
        selectInput("total_field", "Total field", choices = fields, selected = first_matching_field(fields, "total"))
      }
    )
  })

  output$plot_function_args <- renderUI({
    req(input$plot_function)
    tagList(
      tags$h4("Function arguments"),
      function_argument_inputs(input$plot_function, "plot_arg", plot_standard_exclusions())
    )
  })

  preview_plot <- eventReactive(input$refresh_preview, {
    req(loaded_data())
    build_plot(input, loaded_data())
  })

  preview_height_px <- reactive({
    width <- suppressWarnings(as.numeric(input$width))
    height <- suppressWarnings(as.numeric(input$height))

    if (is.na(width) || width <= 0 || is.na(height) || height <= 0) {
      return(420)
    }

    max(320, min(700, round(520 * height / width)))
  })

  output$chart_preview_ui <- renderUI({
    plotOutput("chart_preview", height = paste0(preview_height_px(), "px"))
  })

  output$chart_preview <- renderPlot({
    preview_plot()
  })

  observeEvent(input$save_svg, {
    data <- tryCatch(loaded_data(), error = function(e) NULL)
    if (is.null(data)) {
      output$save_status <- renderText("Save failed: load data first in the Data tab.")
      return()
    }

    output_file <- normalize_svg_filename(input$output_file)
    plot_id <- safe_config_id(input$plot_id, fallback = safe_config_id(output_file, "chart"))
    data_source_id <- safe_config_id(input$data_source_id, fallback = paste0(plot_id, "_data"))
    output_path <- resolved_svg_path()

    result <- tryCatch({
      palette_path <- NULL
      if (identical(input$palette_mode, "custom")) {
        palette <- parse_custom_palette_text(input$custom_palette_text)
        if (length(palette) > 0) {
          palette_path <- write_custom_palette(
            project_root = project_root,
            palette_name = input$custom_palette_name,
            palette = palette,
            sector = input$sector,
            notes = "Created from Shiny app"
          )
          palette_refresh(palette_refresh() + 1)
        }
      }

      plot <- build_plot(input, data)

      save_chart_svg(
        plot = plot,
        output_path = output_path,
        width = input$width,
        height = input$height
      )

      config_path <- NULL
      if (isTRUE(input$update_chart_config)) {
        config_path <- write_release_config_from_app(
          input = input,
          project_root = project_root,
          plot_id = plot_id,
          data_source_id = data_source_id
        )
      }

      list(ok = TRUE, output_path = output_path, config_path = config_path, palette_path = palette_path)
    }, error = function(e) {
      list(ok = FALSE, message = conditionMessage(e), output_path = output_path)
    })

    if (isTRUE(result$ok)) {
      output$save_status <- renderText({
        lines <- paste("Saved SVG:", result$output_path)
        if (!is.null(result$palette_path)) {
          lines <- c(lines, paste("Saved palette metadata:", result$palette_path))
        }
        if (!is.null(result$config_path)) {
          lines <- c(lines, paste("Updated config:", result$config_path))
        }
        paste(lines, collapse = "\n")
      })
      output_refresh(output_refresh() + 1)
    } else {
      output$save_status <- renderText({
        paste("Save failed:", result$message, "\nTarget SVG:", result$output_path)
      })
    }
  })
}

shinyApp(ui, server)
