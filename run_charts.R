project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "00_packages.R"))
source(file.path(project_root, "R", "01_utils.R"))
source(file.path(project_root, "R", "02_styling.R"))
source(file.path(project_root, "R", "03_config.R"))
source(file.path(project_root, "R", "04_data_sources.R"))
source(file.path(project_root, "R", "05_outputs.R"))
source(file.path(project_root, "R", "06_runner.R"))

source_directory(file.path(project_root, "R", "data_functions"))
source_directory(file.path(project_root, "R", "plot_functions"))

config_path <- file.path(project_root, "config", "chart_config.xlsx")
config <- read_chart_config(config_path)
run_plan <- build_run_plan(config)

if (nrow(run_plan) == 0) {
  stop("No active plots matched the run_control filters.", call. = FALSE)
}

run_charts(run_plan, config, project_root)
