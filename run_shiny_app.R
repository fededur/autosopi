if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Missing required R package: shiny. Install it with install.packages('shiny').", call. = FALSE)
}

load_project_renviron <- function(project_root) {
  path <- file.path(project_root, ".Renviron")
  if (file.exists(path)) {
    readRenviron(path)
  }
}

load_project_renviron(getwd())

shiny::runApp(file.path(getwd(), "app"))
