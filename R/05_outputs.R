save_chart_svg <- function(plot, output_path, width = 230, height = 130) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = output_path,
    plot = plot,
    width = width,
    height = height,
    units = "mm",
    device = svglite::svglite
  )

  output_path
}

append_log <- function(project_root, row) {
  log_path <- file.path(project_root, "logs", "chart_run_log.csv")
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  row_df <- data.frame(row, stringsAsFactors = FALSE)

  if (file.exists(log_path)) {
    existing <- read.csv(log_path, stringsAsFactors = FALSE)
    utils::write.csv(dplyr::bind_rows(existing, row_df), log_path, row.names = FALSE)
  } else {
    utils::write.csv(row_df, log_path, row.names = FALSE)
  }
}
