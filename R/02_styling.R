read_metadata <- function(path, sheet = 1) {
  raw <- readxl::read_xlsx(path, sheet = sheet)

  clean <- raw |>
    clean_metadata_names() |>
    dplyr::mutate(
      include = dplyr::coalesce(as.logical(.data$include), TRUE),
      level = dplyr::case_when(
        .data$forecast_group_key == "Total" ~ "grand_total",
        grepl("^Total ", .data$forecast_group_key, ignore.case = TRUE) ~ "sector_total",
        TRUE ~ "forecast_group"
      )
    )

  clean
}

clean_metadata_names <- function(data) {
  clean_names <- tolower(names(data))
  clean_names <- gsub("[^a-z0-9]+", "_", clean_names)
  clean_names <- gsub("^_|_$", "", clean_names)

  names(data) <- clean_names

  data
}

build_color_palette <- function(categories, colors, other_category = NULL, other_color = NULL) {
  if (length(categories) != length(colors)) {
    warning(sprintf(
      "Length mismatch: %d categories but %d colors provided.",
      length(categories),
      length(colors)
    ))
  }

  palette <- stats::setNames(colors[seq_along(categories)], categories)

  if (!is.null(other_category) && !is.null(other_color)) {
    palette[other_category] <- other_color
  }

  palette
}

complete_palette <- function(categories, palette = NULL) {
  categories <- unique(as.character(categories))

  if (is.null(palette)) {
    palette <- character()
  }

  palette <- palette[!is.na(names(palette)) & nzchar(names(palette))]
  missing_categories <- setdiff(categories, names(palette))

  if (length(missing_categories) > 0) {
    extra_colors <- scales::hue_pal()(length(missing_categories))
    names(extra_colors) <- missing_categories
    palette <- c(palette, extra_colors)
  }

  palette[categories]
}

infer_date_frequency <- function(x) {
  dates <- sort(unique(as.Date(x[!is.na(x)])))

  if (length(dates) < 2) {
    return("yearly")
  }

  median_gap <- stats::median(as.numeric(diff(dates)), na.rm = TRUE)

  if (is.na(median_gap)) {
    "yearly"
  } else if (median_gap <= 32) {
    "monthly"
  } else if (median_gap <= 95) {
    "quarterly"
  } else {
    "yearly"
  }
}

parse_flexible_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))

  if (is.numeric(x)) {
    non_missing <- x[!is.na(x)]

    if (
      length(non_missing) > 0 &&
        all(non_missing == floor(non_missing)) &&
        all(non_missing >= 20000 & non_missing <= 100000)
    ) {
      return(as.Date(x, origin = "1899-12-30"))
    }

    return(x)
  }

  if (!is.character(x)) return(x)

  values <- trimws(x)
  values[values == ""] <- NA_character_
  parsed <- rep(as.Date(NA), length(values))

  formats <- c(
    "%d/%m/%Y %H:%M:%S", "%d/%m/%y %H:%M:%S",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%y %H:%M:%S",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S", "%Y/%m/%dT%H:%M:%S",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%y %H:%M:%S",
    "%d/%m/%Y", "%d/%m/%y",
    "%d-%m-%Y", "%d-%m-%y",
    "%d.%m.%Y", "%d.%m.%y",
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d",
    "%d %b %Y", "%d %B %Y",
    "%b %d %Y", "%B %d %Y",
    "%m/%d/%Y", "%m/%d/%y"
  )

  for (fmt in formats) {
    needs_parse <- is.na(parsed) & !is.na(values)
    if (!any(needs_parse)) break

    parsed[needs_parse] <- as.Date(values[needs_parse], format = fmt)
  }

  if (all(is.na(values) | !is.na(parsed))) {
    return(parsed)
  }

  x
}

get_palette <- function(
    level = c("sector", "forecast_group"),
    sector = NULL,
    ref = c("label", "key"),
    metadata_table,
    include_total = FALSE) {
  level <- match.arg(level)
  ref <- match.arg(ref)

  df <- metadata_table

  if (!is.null(sector)) {
    df <- df |>
      dplyr::filter(.data$sector_key %in% sector)
  }

  label_col <- switch(
    level,
    sector = if (ref == "label") "sector_label" else "sector_key",
    forecast_group = if (ref == "label") "forecast_group_label" else "forecast_group_key"
  )

  color_col <- switch(
    level,
    sector = "sector_color",
    forecast_group = "forecast_group_color"
  )

  if (level == "sector") {
    sector_total <- df |>
      dplyr::filter(.data$level == "sector_total")

    df <- sector_total |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c("sector_key", "sector_label", "sector_color")))) |>
      dplyr::transmute(label = .data[[label_col]], color = .data[[color_col]])

    if (include_total) {
      grand <- metadata_table |>
        dplyr::filter(.data$level == "grand_total") |>
        dplyr::transmute(label = .data[[label_col]], color = .data[[color_col]])

      df <- dplyr::bind_rows(grand, df)
    }
  } else if (level == "forecast_group") {
    fg <- df |>
      dplyr::filter(.data$level == "forecast_group")

    if (include_total) {
      st <- df |>
        dplyr::filter(.data$level == "sector_total")

      fg <- dplyr::bind_rows(st, fg)
    }

    df <- fg |>
      dplyr::transmute(label = .data[[label_col]], color = .data[[color_col]])
  }

  if (!include_total) {
    df <- df |>
      dplyr::filter(!grepl("total", tolower(.data$label)))
  }

  pal <- stats::setNames(df$color, df$label)
  pal[!duplicated(names(pal))]
}

get_forecast_palette <- function(
    sector,
    forecast_palette_table,
    ref = c("key", "label"),
    fill = "actual",
    include_total = FALSE) {
  ref <- match.arg(ref)

  df <- forecast_palette_table |>
    clean_metadata_names() |>
    dplyr::filter(.data$sector_key %in% sector) |>
    dplyr::filter(.data$include %in% c(TRUE, "TRUE", "true", "Yes", "yes", 1))

  if (!is.null(fill)) {
    df <- df |>
      dplyr::filter(.data$fill %in% fill)
  }

  if (!include_total) {
    df <- df |>
      dplyr::filter(!grepl("total", tolower(.data$forecast_group_key)))
  }

  label_col <- if (ref == "label") "forecast_group_label" else "forecast_group_key"

  df <- df |>
    dplyr::transmute(label = .data[[label_col]], color = .data$color) |>
    dplyr::filter(!is.na(.data$label), nzchar(.data$label), !is.na(.data$color), nzchar(.data$color))

  pal <- stats::setNames(df$color, df$label)
  pal[!duplicated(names(pal))]
}

palette_from_config <- function(config, palette_name) {
  if (is.null(palette_name) || is.na(palette_name)) return(NULL)
  if (is.null(config$palettes) || nrow(config$palettes) == 0) return(NULL)

  rows <- config$palettes |>
    dplyr::filter(.data$palette == palette_name)

  if (nrow(rows) == 0) return(NULL)

  stats::setNames(rows$hex, rows$item)
}

metadata_path <- function(project_root, filename = "sopi_metadata.xlsx") {
  file.path(project_root, "metadata", filename)
}

load_metadata_resource <- function(project_root, metadata_file = "sopi_metadata.xlsx") {
  path <- metadata_path(project_root, metadata_file)

  if (!file.exists(path)) {
    return(list(path = path, metadata = NULL, forecast_palette = NULL))
  }

  sheets <- readxl::excel_sheets(path)

  list(
    path = path,
    metadata = if ("metadata" %in% sheets) read_metadata(path, sheet = "metadata") else NULL,
    forecast_palette = if ("forecast_palette" %in% sheets) {
      readxl::read_xlsx(path, sheet = "forecast_palette") |>
        clean_metadata_names()
    } else {
      NULL
    }
  )
}

get_forecast_labels <- function(
    sector,
    forecast_palette_table,
    ref = c("key", "label"),
    fill = "actual",
    include_total = FALSE) {
  ref <- match.arg(ref)

  if (is.null(forecast_palette_table) || nrow(forecast_palette_table) == 0) {
    return(NULL)
  }

  df <- forecast_palette_table |>
    clean_metadata_names() |>
    dplyr::filter(.data$sector_key %in% sector) |>
    dplyr::filter(.data$include %in% c(TRUE, "TRUE", "true", "Yes", "yes", 1))

  if (!is.null(fill) && "fill" %in% names(df)) {
    df <- df |>
      dplyr::filter(.data$fill %in% fill)
  }

  if (!include_total) {
    df <- df |>
      dplyr::filter(!grepl("total", tolower(.data$forecast_group_key)))
  }

  name_col <- if (ref == "label") "forecast_group_label" else "forecast_group_key"

  df <- df |>
    dplyr::transmute(name = .data[[name_col]], label = .data$forecast_group_label) |>
    dplyr::filter(!is.na(.data$name), nzchar(.data$name), !is.na(.data$label), nzchar(.data$label))

  labels <- stats::setNames(df$label, df$name)
  labels[!duplicated(names(labels))]
}

complete_labels <- function(categories, labels = NULL) {
  categories <- unique(as.character(categories))

  if (is.null(labels)) {
    labels <- character()
  }

  labels <- labels[!is.na(names(labels)) & nzchar(names(labels))]
  missing_categories <- setdiff(categories, names(labels))

  if (length(missing_categories) > 0) {
    labels <- c(labels, stats::setNames(missing_categories, missing_categories))
  }

  labels[categories]
}

style_from_metadata <- function(
    metadata_resource,
    sector,
    categories = NULL,
    ref = "key",
    fill = "actual",
    include_total = FALSE) {
  if (
    is.null(metadata_resource) ||
      is.null(metadata_resource$forecast_palette) ||
      is.null(sector) ||
      is.na(sector)
  ) {
    return(list(palette = NULL, labels = NULL))
  }

  palette <- get_forecast_palette(
    sector = sector,
    forecast_palette_table = metadata_resource$forecast_palette,
    ref = ref,
    fill = fill,
    include_total = include_total
  )

  labels <- get_forecast_labels(
    sector = sector,
    forecast_palette_table = metadata_resource$forecast_palette,
    ref = ref,
    fill = fill,
    include_total = include_total
  )

  if (!is.null(categories)) {
    palette <- complete_palette(categories, palette)
    labels <- complete_labels(categories, labels)
  }

  list(palette = palette, labels = labels)
}

palette_from_metadata <- function(
    project_root,
    sector,
    level = "forecast_group",
    ref = "key",
    include_total = FALSE,
    metadata_file = "sopi_metadata.xlsx",
    sheet = "metadata") {
  path <- metadata_path(project_root, metadata_file)

  if (!file.exists(path) || is.null(sector) || is.na(sector)) {
    return(NULL)
  }

  metadata <- read_metadata(path, sheet = sheet)

  get_palette(
    level = level,
    sector = sector,
    ref = ref,
    metadata_table = metadata,
    include_total = include_total
  )
}

palette_from_forecast_metadata <- function(
    project_root,
    sector,
    categories = NULL,
    ref = "key",
    fill = "actual",
    include_total = FALSE,
    metadata_file = "sopi_metadata.xlsx",
    sheet = "forecast_palette") {
  path <- metadata_path(project_root, metadata_file)

  if (!file.exists(path) || is.null(sector) || is.na(sector)) {
    return(NULL)
  }

  forecast_palette <- readxl::read_xlsx(path, sheet = sheet) |>
    clean_metadata_names()

  pal <- get_forecast_palette(
    sector = sector,
    forecast_palette_table = forecast_palette,
    ref = ref,
    fill = fill,
    include_total = include_total
  )

  if (!is.null(categories)) {
    pal <- complete_palette(categories, pal)
  }

  pal
}

theme_sopi <- function(family = "Calibri", base_size = 10.5, ...) {
  default_theme <- list(
    text = if (!is.null(family)) ggplot2::element_text(family = family) else ggplot2::element_text(),
    axis.line = ggplot2::element_line(color = "#dad9d9"),
    axis.ticks = ggplot2::element_line(color = "#dad9d9"),
    axis.ticks.length = grid::unit(1, "mm"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    legend.position.inside = c(1, 1),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"),
    legend.box.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0, unit = "mm"),
    legend.key.spacing = grid::unit(1, "mm"),
    legend.key.height = grid::unit(4, "mm"),
    plot.margin = ggplot2::margin(t = 20, b = 10)
  )

  user_theme <- list(...)
  merged_theme <- utils::modifyList(default_theme, user_theme)

  ggplot2::theme_minimal(base_size = base_size) + do.call(ggplot2::theme, merged_theme)
}

get_nice_breaks <- function(max_val, min_breaks = 3, max_breaks = 6) {
  if (is.na(max_val) || max_val <= 0) {
    return(list(
      rounded_max = 1,
      breaks = c(0, 1),
      accuracy = 1
    ))
  }

  magnitude <- 10^floor(log10(max_val))
  steps <- c(0.1, 0.2, 0.25, 0.5, 1, 2, 2.5, 5, 10)

  if (max_val < 1) magnitude <- magnitude / 10
  if (max_val < 0.1) magnitude <- magnitude / 10

  for (s in steps) {
    step <- s * magnitude
    max_nice <- ceiling(max_val / step) * step
    breaks <- seq(0, max_nice, by = step)

    if (length(breaks) >= min_breaks && length(breaks) <= max_breaks) {
      accuracy <- if (step < 1) {
        10^(-ceiling(abs(log10(step))))
      } else {
        1
      }

      return(list(
        rounded_max = max_nice,
        breaks = breaks,
        accuracy = accuracy
      ))
    }
  }

  breaks <- pretty(c(0, max_val), n = 5)
  step <- diff(breaks)[1]

  accuracy <- if (step < 1) {
    10^(-ceiling(abs(log10(step))))
  } else {
    1
  }

  list(
    rounded_max = max(breaks),
    breaks = breaks,
    accuracy = accuracy
  )
}
