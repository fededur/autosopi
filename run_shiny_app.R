if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Missing required R package: shiny. Install it with install.packages('shiny').", call. = FALSE)
}

load_project_renviron <- function(project_root) {
  path <- file.path(project_root, ".Renviron")
  if (file.exists(path)) {
    override_names <- c("SOPI_RELEASES_ROOT", "SOPI_RELEASE_YEAR", "SOPI_RELEASE_ROUND")
    overrides <- Sys.getenv(override_names, unset = NA_character_)

    readRenviron(path)

    overrides <- overrides[!is.na(overrides) & nzchar(overrides)]
    if (length(overrides) > 0) {
      do.call(Sys.setenv, as.list(overrides))
    }
  }
}

load_project_renviron(getwd())

shiny::runApp(file.path(getwd(), "app"))
