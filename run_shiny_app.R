if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Missing required R package: shiny. Install it with install.packages('shiny').", call. = FALSE)
}

local_env <- file.path(getwd(), ".Renviron.local")
if (file.exists(local_env)) {
  readRenviron(local_env)
}

shiny::runApp(file.path(getwd(), "app"))
