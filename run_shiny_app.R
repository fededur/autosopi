if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Missing required R package: shiny. Install it with install.packages('shiny').", call. = FALSE)
}

shiny::runApp(file.path(getwd(), "app"))
