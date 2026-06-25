normalise_report_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

path_from_parts <- function(parts) {
  if (length(parts) == 0) return("")
  do.call(file.path, as.list(parts))
}

release_report_dir_from_graph <- function(graph_path) {
  parts <- strsplit(normalise_report_path(graph_path), "/", fixed = TRUE)[[1]]
  graph_index <- which(tolower(parts) == "graphs")

  if (length(graph_index) > 0) {
    return(file.path(path_from_parts(parts[seq_len(graph_index[[1]] - 1)]), "Report"))
  }

  file.path(dirname(dirname(graph_path)), "Report")
}

relative_report_path <- function(path, report_dir) {
  path <- normalise_report_path(path)
  report_dir <- normalise_report_path(report_dir)

  if (requireNamespace("xfun", quietly = TRUE)) {
    return(xfun::relative_path(path, report_dir))
  }

  path_parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  dir_parts <- strsplit(report_dir, "/", fixed = TRUE)[[1]]
  common <- 0L
  max_common <- min(length(path_parts), length(dir_parts))

  while (common < max_common && identical(path_parts[[common + 1L]], dir_parts[[common + 1L]])) {
    common <- common + 1L
  }

  up <- rep("..", length(dir_parts) - common)
  down <- path_parts[(common + 1L):length(path_parts)]
  paste(c(up, down), collapse = "/")
}

markdown_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\[", "\\\\[", x)
  x <- gsub("\\]", "\\\\]", x)
  x
}

markdown_path <- function(path) {
  gsub(" ", "%20", gsub("\\\\", "/", path), fixed = TRUE)
}

figure_caption <- function(row) {
  title <- row$title %||% NA_character_
  subtitle <- row$subtitle %||% NA_character_

  caption <- if (!is.na(title) && nzchar(trimws(title))) {
    title
  } else if (!is.na(row$plot_id) && nzchar(trimws(row$plot_id))) {
    row$plot_id
  } else {
    tools::file_path_sans_ext(basename(row$output_path))
  }

  if (!is.na(subtitle) && nzchar(trimws(subtitle))) {
    caption <- paste(caption, subtitle, sep = " - ")
  }

  markdown_escape(caption)
}

build_release_report_plan <- function(config, project_root) {
  run_plan <- build_run_plan(config)
  if (nrow(run_plan) == 0) {
    stop("No active plots found in the release config.", call. = FALSE)
  }

  rows <- lapply(seq_len(nrow(run_plan)), function(i) {
    job <- run_plan[i, , drop = FALSE]
    resolved <- resolve_job_settings(job, config)
    global <- resolved$global
    sector <- resolved$sector

    sector_folder <- sector$output_subfolder %||% job$sector
    output_path <- build_output_path(
      project_root = project_root,
      output_root = global$output_root,
      release_year = global$release_year,
      release_round = global$release_round,
      sector_folder = sector_folder,
      output_file = job$output_file
    )

    data.frame(
      sector = as.character(job$sector),
      plot_id = as.character(job$plot_id),
      title = as.character(job$title %||% NA_character_),
      subtitle = as.character(job$subtitle %||% NA_character_),
      sort_order = suppressWarnings(as.numeric(job$sort_order)),
      output_file = as.character(job$output_file),
      output_path = normalise_report_path(output_path),
      exists = file.exists(output_path),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows) |>
    dplyr::filter(.data$exists) |>
    dplyr::arrange(.data$sector, .data$sort_order, .data$plot_id)
}

write_release_report_qmd <- function(report_plan, report_dir, release_year, release_round) {
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

  qmd_path <- file.path(report_dir, "sopi_release_figures.qmd")
  sectors <- unique(report_plan$sector)
  lines <- c(
    "---",
    sprintf("title: \"SOPI %s %s Figures\"", release_year, release_round),
    "format:",
    "  html:",
    "    toc: true",
    "    embed-resources: false",
    "engine: knitr",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "<style>",
    ".sector-page { page-break-before: always; break-before: page; }",
    ".sector-page:first-of-type { page-break-before: auto; break-before: auto; }",
    "figure { margin-bottom: 36px; }",
    "figcaption { margin-top: 8px; color: #555; font-size: 0.95em; }",
    "img { max-width: 100%; height: auto; }",
    "</style>",
    ""
  )

  for (sector in sectors) {
    sector_rows <- report_plan[report_plan$sector == sector, , drop = FALSE]
    lines <- c(
      lines,
      "::: {.sector-page}",
      "",
      paste0("# ", sector),
      ""
    )

    for (i in seq_len(nrow(sector_rows))) {
      row <- sector_rows[i, , drop = FALSE]
      image_path <- markdown_path(relative_report_path(row$output_path, report_dir))
      lines <- c(
        lines,
        sprintf("![%s](%s)", figure_caption(row), image_path),
        ""
      )
    }

    lines <- c(lines, ":::", "")
  }

  writeLines(lines, qmd_path, useBytes = TRUE)
  normalise_report_path(qmd_path)
}

render_release_report <- function(qmd_path) {
  if (!nzchar(Sys.which("quarto"))) {
    warning("Quarto was not found on PATH. The .qmd report was written but not rendered.", call. = FALSE)
    return(NULL)
  }

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(dirname(qmd_path))
  status <- system2("quarto", c("render", basename(qmd_path)), stdout = TRUE, stderr = TRUE)
  output_path <- file.path(dirname(qmd_path), "sopi_release_figures.html")

  if (!file.exists(output_path)) {
    stop("Quarto render did not create the expected HTML report:\n", paste(status, collapse = "\n"), call. = FALSE)
  }

  normalise_report_path(output_path)
}

build_release_figure_report <- function(config_path, project_root = getwd(), render = TRUE) {
  config <- read_chart_config(config_path)
  global <- settings_from_table(config$release_settings)
  report_plan <- build_release_report_plan(config, project_root)

  if (nrow(report_plan) == 0) {
    stop("No existing SVG files were found for active plots in the release config.", call. = FALSE)
  }

  report_dir <- release_report_dir_from_graph(report_plan$output_path[[1]])
  qmd_path <- write_release_report_qmd(
    report_plan = report_plan,
    report_dir = report_dir,
    release_year = global$release_year %||% "",
    release_round = global$release_round %||% ""
  )

  html_path <- if (isTRUE(render)) render_release_report(qmd_path) else NULL

  list(
    report_dir = normalise_report_path(report_dir),
    qmd_path = qmd_path,
    html_path = html_path,
    figures = report_plan
  )
}
