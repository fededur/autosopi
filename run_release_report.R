project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

load_project_renviron <- function(project_root) {
  path <- file.path(project_root, ".Renviron")
  if (file.exists(path)) {
    readRenviron(path)
  }
}

load_project_renviron(project_root)

source(file.path(project_root, "R", "00_packages.R"))
source(file.path(project_root, "R", "01_utils.R"))
source(file.path(project_root, "R", "02_styling.R"))
source(file.path(project_root, "R", "03_config.R"))
source(file.path(project_root, "R", "04_data_sources.R"))
source(file.path(project_root, "R", "05_outputs.R"))
source(file.path(project_root, "R", "07_plot_protocol.R"))
source(file.path(project_root, "R", "06_runner.R"))
source(file.path(project_root, "R", "08_report.R"))

release_config_path <- function(project_root, release_year, release_round) {
  file.path(
    project_root,
    "config",
    "releases",
    as.character(release_year),
    gsub("[^A-Za-z0-9_-]+", "_", as.character(release_round)),
    "chart_config.xlsx"
  )
}

release_config_candidates <- function(project_root) {
  list.files(
    file.path(project_root, "config", "releases"),
    pattern = "^chart_config[.]xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )
}

default_releases_root <- function(project_root) {
  user_profile <- Sys.getenv("USERPROFILE", unset = "")
  if (nzchar(trimws(user_profile))) {
    return(normalizePath(file.path(expand_user_path(user_profile), "Documents", "outputs", "SOPI_releases"), winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path(project_root, "SOPI_releases"), winslash = "/", mustWork = FALSE)
}

set_report_releases_root <- function(args, project_root) {
  root <- Sys.getenv("SOPI_RELEASES_ROOT", unset = "")

  if (length(args) >= 3 && grepl("^[0-9]{4}$", args[[1]])) {
    root <- args[[3]]
  } else if (length(args) >= 2 && !grepl("^[0-9]{4}$", args[[1]])) {
    root <- args[[2]]
  }

  if (!nzchar(trimws(root))) {
    root <- default_releases_root(project_root)
  }

  Sys.setenv(SOPI_RELEASES_ROOT = normalizePath(expand_user_path(root), winslash = "/", mustWork = FALSE))
}

resolve_report_config_path <- function(args, project_root) {
  default_config_path <- file.path(project_root, "config", "chart_config.xlsx")

  if (length(args) >= 2 && grepl("^[0-9]{4}$", args[[1]])) {
    return(release_config_path(project_root, args[[1]], args[[2]]))
  }

  if (length(args) >= 1 && nzchar(args[[1]])) {
    config_path <- args[[1]]
    if (!grepl("^([A-Za-z]:)?[/\\\\]", config_path)) {
      config_path <- file.path(project_root, config_path)
    }
    return(config_path)
  }

  env_config <- Sys.getenv("AUTOSOPI_CONFIG", unset = "")
  if (nzchar(env_config)) {
    if (!grepl("^([A-Za-z]:)?[/\\\\]", env_config)) {
      env_config <- file.path(project_root, env_config)
    }
    return(env_config)
  }

  env_year <- Sys.getenv("SOPI_RELEASE_YEAR", unset = "")
  env_round <- Sys.getenv("SOPI_RELEASE_ROUND", unset = "")
  if (nzchar(env_year) && nzchar(env_round)) {
    return(release_config_path(project_root, env_year, env_round))
  }

  candidates <- release_config_candidates(project_root)
  if (length(candidates) == 1) {
    return(candidates[[1]])
  }

  if (length(candidates) > 1) {
    stop(
      "Multiple release configs found. Pass the release year and round, for example:\n",
      "Rscript run_release_report.R 2026 June",
      call. = FALSE
    )
  }

  default_config_path
}

args <- commandArgs(trailingOnly = TRUE)
config_path <- resolve_report_config_path(args, project_root)
set_report_releases_root(args, project_root)

if (!file.exists(config_path)) {
  stop("Release config file not found: ", config_path, call. = FALSE)
}

message("Using config: ", normalizePath(config_path, winslash = "/", mustWork = FALSE))
message("Using SOPI releases root: ", Sys.getenv("SOPI_RELEASES_ROOT"))

result <- build_release_figure_report(
  config_path = config_path,
  project_root = project_root,
  render = TRUE
)

message("Report folder: ", result$report_dir)
message("Quarto file: ", result$qmd_path)
if (!is.null(result$html_path)) {
  message("HTML report: ", result$html_path)
}
message("Figures included: ", nrow(result$figures))
