project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "00_packages.R"))
source(file.path(project_root, "R", "01_utils.R"))
source(file.path(project_root, "R", "02_styling.R"))
source(file.path(project_root, "R", "03_config.R"))
source(file.path(project_root, "R", "04_data_sources.R"))
source(file.path(project_root, "R", "05_outputs.R"))
source(file.path(project_root, "R", "07_plot_protocol.R"))
source(file.path(project_root, "R", "06_runner.R"))
source(file.path(project_root, "R", "08_report.R"))

args <- commandArgs(trailingOnly = TRUE)
default_config_path <- file.path(project_root, "config", "chart_config.xlsx")
config_path <- if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  Sys.getenv("AUTOSOPI_CONFIG", default_config_path)
}

if (!grepl("^([A-Za-z]:)?[/\\\\]", config_path)) {
  config_path <- file.path(project_root, config_path)
}

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
