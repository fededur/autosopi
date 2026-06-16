required_app_packages <- c("shiny", "dplyr", "ggplot2", "readxl", "scales", "svglite")
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

  paste(deparse(default), collapse = " ")
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
    sector = input$sector,
    historical_start_year = input$historical_start_year,
    historical_end_year = input$historical_end_year,
    year_start = input$historical_start_year,
    year_end = input$historical_end_year
  )
}

chart_context_args <- function(input) {
  list(
    sector = input$sector,
    family = input$font_family,
    base_size = input$base_size,
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

build_output_folder <- function(project_root, output_root, release_year, release_round, sector_folder) {
  output_root <- resolve_output_base_path(project_root, output_root)
  release_parts <- c(release_year, release_round, sector_folder)
  release_parts <- as.character(release_parts)
  release_parts <- release_parts[!is.na(release_parts) & nzchar(release_parts)]
  release_parts <- gsub("[^A-Za-z0-9_-]+", "_", release_parts)

  do.call(file.path, c(list(output_root), as.list(release_parts))) |>
    normalizePath(winslash = "/", mustWork = FALSE)
}

build_app_output_path <- function(project_root, output_root, release_year, release_round, sector_folder, output_file) {
  file.path(
    build_output_folder(project_root, output_root, release_year, release_round, sector_folder),
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
    path <- input$excel_path
    if (!grepl("^([A-Za-z]:)?[/\\\\]", path)) {
      path <- file.path(project_root, path)
    }

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

  args$x <- input$x_field

  if (!is_blank(input$group_field)) {
    args$group <- input$group_field
  }

  if (!is_blank(input$column_value)) {
    if ("y_col" %in% names(formals(get(input$plot_function, mode = "function")))) {
      args$y_col <- input$column_value
    }
    if ("y" %in% names(formals(get(input$plot_function, mode = "function")))) {
      args$y <- input$column_value
    }
  }

  if (!is_blank(input$line_value)) {
    args$y_line <- input$line_value
  }

  args$x_freq <- input$x_freq
  args$y_col_label <- input$column_axis_label
  args$y_line_label <- input$line_axis_label
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
    "historical_start_year",
    "historical_end_year",
    "year_start",
    "year_end",
    "project_root"
  )
}

plot_standard_exclusions <- function() {
  c(
    "data",
    "x",
    "y",
    "y_col",
    "y_line",
    "group",
    "x_freq",
    "family",
    "base_size",
    "forecast",
    "forecast_start",
    "forecast_end",
    "palette",
    "palette_fill",
    "palette_line",
    "labels",
    "y_col_label",
    "y_line_label",
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
            h4("Output"),
            textInput(
              "output_root",
              "SharePoint output base folder",
              value = normalizePath(file.path(project_root, "outputs"), winslash = "/", mustWork = FALSE)
            ),
            tags$p(class = "sopi-note", "Paste the synced SharePoint folder path. The app adds year, release, and sector folders below it."),
            textInput("output_file", "SVG filename", value = "preview_chart.svg"),
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
            h4("Data period"),
            numericInput("historical_start_year", "Historical start year", value = 2019, min = 1990),
            numericInput("historical_end_year", "Historical end year", value = 2030, min = 1990),
            conditionalPanel(
              "input.data_source_type == 'function'",
              selectInput("data_function", "Data function", choices = data_function_names),
              uiOutput("data_function_args"),
              textAreaInput("data_extra_args", "Extra data arguments", value = "seed = 42", rows = 3)
            ),
            conditionalPanel(
              "input.data_source_type == 'excel'",
              textInput("excel_path", "Excel file path", value = "data/raw/manual_data.xlsx"),
              textInput("excel_sheet", "Sheet", value = ""),
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
            tags$p(class = "sopi-note", "Chart-specific labels, axes, forecast settings, and extra arguments remain in the Chart tab. SVG export size is set in Preview And Save.")
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
            textAreaInput("plot_extra_args", "Extra plot arguments", rows = 3),
            actionButton("preview_plot", "Preview Chart", class = "btn-primary")
          )
        )
      )
    ),
    tabPanel(
      "Preview And Save",
      fluidRow(
        column(
          9,
          wellPanel(
            plotOutput("chart_preview", height = "calc(100vh - 180px)")
          )
        ),
        column(
          3,
          wellPanel(
            h4("Save SVG"),
            tags$p(class = "sopi-note", "Export size for this saved SVG."),
            fluidRow(
              column(6, numericInput("width", "Width", value = 9, min = 3, step = 0.5)),
              column(6, numericInput("height", "Height", value = 5, min = 3, step = 0.5))
            ),
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

  output_folder <- reactive({
    build_output_folder(
      project_root = project_root,
      output_root = input$output_root,
      release_year = input$release_year,
      release_round = input$release_round,
      sector_folder = input$sector
    )
  })

  output_files <- reactive({
    output_refresh()
    input$output_root
    list_output_files(output_folder())
  })

  observeEvent(input$output_root, {
    output_refresh(output_refresh() + 1)
  }, ignoreInit = TRUE)

  observeEvent(list(input$release_year, input$release_round, input$sector), {
    output_refresh(output_refresh() + 1)
  }, ignoreInit = TRUE)

  output$resolved_output_folder <- renderText({
    output_folder()
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
      selectInput("line_value", "Line value", choices = optional_choices(data), selected = first_matching_field(fields, "volume"))
    )
  })

  output$plot_function_args <- renderUI({
    req(input$plot_function)
    tagList(
      tags$h4("Function arguments"),
      function_argument_inputs(input$plot_function, "plot_arg", plot_standard_exclusions())
    )
  })

  preview_plot <- eventReactive(input$preview_plot, {
    req(loaded_data())
    build_plot(input, loaded_data())
  })

  output$chart_preview <- renderPlot({
    preview_plot()
  })

  observeEvent(input$save_svg, {
    plot <- preview_plot()
    req(plot)

    output_root <- input$output_root
    output_file <- input$output_file
    sector_folder <- input$sector

    output_path <- build_app_output_path(
      project_root = project_root,
      output_root = output_root,
      release_year = input$release_year,
      release_round = input$release_round,
      sector_folder = sector_folder,
      output_file = output_file
    )

    save_chart_svg(
      plot = plot,
      output_path = output_path,
      width = input$width,
      height = input$height
    )

    output$save_status <- renderText({
      paste("Saved:", output_path)
    })

    output_refresh(output_refresh() + 1)
  })
}

shinyApp(ui, server)
