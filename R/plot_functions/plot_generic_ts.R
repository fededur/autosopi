plot_generic_ts <- function(
    data,
    x,
    x_freq = c("auto", "yearly", "quarterly", "monthly"),
    period_type = c("calendar", "financial"),
    financial_start_month = 7,
    y_line = NULL,
    y_col  = NULL,
    group = NULL,
    y_line_label = NULL,
    y_col_label  = NULL,
    x_label  = NULL,
    x_breaks = NULL,
    x_n_breaks = NULL,
    y_line_accuracy = NULL,
    y_col_accuracy  = NULL,
    y_line_scale = 1,
    y_col_scale  = 1,
    n_breaks = NULL,
    primary_min_breaks = 3,
    primary_max_breaks = 6,
    secondary_min_breaks = 3,
    secondary_max_breaks = 6,
    primary_axis = c("line", "column"),
    line_label = NULL,
    col_label  = NULL,
    labels  = NULL,
    palette = NULL,
    palette_fill = NULL,
    palette_line = NULL,
    forecast = FALSE,
    forecast_start = NULL,
    forecast_end = NULL,
    forecast_label = "Forecast",
    forecast_label_fontsize = 8,
    forecast_max_mult = 1.1,
    forecast_label_pos = 0.9,
    col_position = c("stacked", "dodge"),
    family = "DIN",
    base_size = 10.5,
    sort_col = c("none", "asc", "desc"),
    sort_line = c("none", "asc", "desc"),
    legend_order = NULL,
    col_order = NULL,
    line_order = NULL
) {
  
  sort_col  <- match.arg(sort_col)
  sort_line <- match.arg(sort_line)
  x_freq    <- match.arg(x_freq)
  period_type <- match.arg(period_type)
  col_position <- match.arg(col_position)
  primary_axis <- match.arg(primary_axis)

  clean_axis_scale <- function(value, arg_name) {
    if (is.null(value) || length(value) == 0 || is.na(value)) return(1)
    value <- suppressWarnings(as.numeric(value))
    if (!is.finite(value) || value <= 0) {
      stop(arg_name, " must be a positive number.", call. = FALSE)
    }
    value
  }

  y_line_scale <- clean_axis_scale(y_line_scale, "y_line_scale")
  y_col_scale <- clean_axis_scale(y_col_scale, "y_col_scale")

  clean_break_count <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) return(NULL)
    value <- suppressWarnings(as.integer(value))
    if (!is.finite(value) || value < 2) {
      stop("n_breaks must be an integer greater than or equal to 2.", call. = FALSE)
    }
    value
  }

  n_breaks <- clean_break_count(n_breaks)
  x_n_breaks <- clean_break_count(x_n_breaks)
  if (!is.null(n_breaks)) {
    primary_min_breaks <- n_breaks
    primary_max_breaks <- n_breaks
    secondary_min_breaks <- n_breaks
    secondary_max_breaks <- n_breaks
  }
  
  # =========================
  # DATE HELPERS — BASE R ONLY
  # =========================
  
  get_year <- function(d) as.integer(format(d, "%Y"))
  get_month <- function(d) as.integer(format(d, "%m"))
  
  to_date_arg <- function(value, side = c("start", "end")) {
    side <- match.arg(side)
    
    if (is.null(value)) return(NULL)
    if (inherits(value, "Date")) return(value)
    if (inherits(value, c("POSIXct", "POSIXlt"))) return(as.Date(value))
    
    if (is.numeric(value)) {
      if (side == "start") {
        return(as.Date(paste0(value, "-01-01")))
      } else {
        return(as.Date(paste0(value, "-12-31")))
      }
    }
    
    as.Date(value)
  }
  
  month_start <- function(d) {
    as.Date(sprintf("%04d-%02d-01", get_year(d), get_month(d)))
  }
  
  add_months_base <- function(d, n) {
    d <- month_start(d)
    
    y <- get_year(d)
    m <- get_month(d)
    
    total_month <- m + n
    new_y <- y + (total_month - 1) %/% 12
    new_m <- (total_month - 1) %% 12 + 1
    
    as.Date(sprintf("%04d-%02d-01", new_y, new_m))
  }
  
  calendar_year_start <- function(d) {
    as.Date(sprintf("%04d-01-01", get_year(d)))
  }
  
  calendar_quarter_start <- function(d) {
    m <- get_month(d)
    q_start_month <- ((m - 1) %/% 3) * 3 + 1
    
    as.Date(sprintf("%04d-%02d-01", get_year(d), q_start_month))
  }
  
  financial_year_start <- function(d, start_month = 7) {
    y <- get_year(d)
    m <- get_month(d)
    
    start_year <- ifelse(m < start_month, y - 1, y)
    
    as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  }
  
  financial_quarter_start <- function(d, start_month = 7) {
    fy_start <- financial_year_start(d, start_month)
    
    months_since_start <-
      (get_year(d) - get_year(fy_start)) * 12 +
      (get_month(d) - get_month(fy_start))
    
    quarter_offset <- floor(months_since_start / 3) * 3
    
    add_months_base(fy_start, quarter_offset)
  }
  
  snap_period_start <- function(d, x_freq, period_type, financial_start_month = 7) {
    if (x_freq == "monthly") {
      month_start(d)
    } else if (x_freq == "quarterly" && period_type == "calendar") {
      calendar_quarter_start(d)
    } else if (x_freq == "quarterly" && period_type == "financial") {
      financial_quarter_start(d, financial_start_month)
    } else if (x_freq == "yearly" && period_type == "calendar") {
      calendar_year_start(d)
    } else if (x_freq == "yearly" && period_type == "financial") {
      financial_year_start(d, financial_start_month)
    } else {
      d
    }
  }
  
  snap_period_end <- function(d, x_freq, period_type, financial_start_month = 7) {
    start <- snap_period_start(
      d = d,
      x_freq = x_freq,
      period_type = period_type,
      financial_start_month = financial_start_month
    )
    
    if (x_freq == "monthly") {
      add_months_base(start, 1) - 1
    } else if (x_freq == "quarterly") {
      add_months_base(start, 3) - 1
    } else if (x_freq == "yearly") {
      add_months_base(start, 12) - 1
    } else {
      d
    }
  }
  
  df <- data
  
  # =========================
  # NAMES
  # =========================
  
  x_name <- if (is.character(x)) x else deparse(substitute(x))
  
  y_line_name <- if (is.null(y_line)) {
    NULL
  } else if (is.character(y_line)) {
    y_line
  } else {
    deparse(substitute(y_line))
  }
  
  y_col_name <- if (is.null(y_col)) {
    NULL
  } else if (is.character(y_col)) {
    y_col
  } else {
    deparse(substitute(y_col))
  }
  
  group_name <- if (is.null(group)) {
    NULL
  } else if (is.character(group)) {
    group
  } else {
    deparse(substitute(group))
  }
  
  has_group <- !is.null(group_name)
  
  if (!has_group) {
    group_name <- ".generic_ts_group"
    df[[group_name]] <- "All"
  }
  
  df[[x_name]] <- parse_flexible_date(df[[x_name]])
  
  has_line <- !is.null(y_line_name)
  has_col  <- !is.null(y_col_name)
  
  if (!has_line && !has_col) {
    stop("At least one of y_line or y_col must be provided")
  }

  if (!has_line) {
    primary_axis <- "column"
  } else if (!has_col) {
    primary_axis <- "line"
  }

  line_is_primary <- identical(primary_axis, "line")
  col_is_primary <- identical(primary_axis, "column")
  
  label_lookup <- labels
  group_vals <- unique(as.character(df[[group_name]]))
  
  is_date <- inherits(df[[x_name]], c("Date", "POSIXct", "POSIXlt"))
  is_discrete_x <- !is_date && !is.numeric(df[[x_name]]) && !is.integer(df[[x_name]])

  parse_x_break_values <- function(breaks, is_date) {
    if (is.null(breaks) || length(breaks) == 0) return(NULL)

    if (inherits(breaks, "Date")) return(breaks)
    if (inherits(breaks, c("POSIXct", "POSIXlt"))) return(as.Date(breaks))
    if (is.numeric(breaks)) {
      if (is_date && all(!is.na(breaks)) && all(breaks >= 1000 & breaks <= 9999)) {
        return(as.Date(paste0(as.integer(breaks), "-01-01")))
      }
      return(breaks)
    }

    if (!is.character(breaks)) return(breaks)

    values <- trimws(unlist(strsplit(breaks, "[,;\\n]", perl = TRUE)))
    values <- values[nzchar(values)]
    if (length(values) == 0) return(NULL)

    if (is_date) {
      interval_pattern <- "^[0-9]+\\s+(day|days|week|weeks|month|months|year|years)$"
      if (length(values) == 1 && grepl(interval_pattern, values, ignore.case = TRUE)) {
        return(values)
      }

      numeric_values <- suppressWarnings(as.numeric(values))
      if (all(!is.na(numeric_values)) && all(numeric_values >= 1000 & numeric_values <= 9999)) {
        return(as.Date(paste0(as.integer(numeric_values), "-01-01")))
      }

      parsed_dates <- parse_flexible_date(values)
      if (inherits(parsed_dates, "Date") && any(!is.na(parsed_dates))) {
        return(parsed_dates)
      }
    }

    numeric_values <- suppressWarnings(as.numeric(values))
    if (all(!is.na(numeric_values))) {
      return(numeric_values)
    }

    values
  }

  x_breaks <- parse_x_break_values(x_breaks, is_date)

  make_x_n_breaks <- function(x_values, n, is_date) {
    if (is.null(n)) return(NULL)

    x_values <- x_values[!is.na(x_values)]
    if (length(x_values) == 0) return(NULL)

    if (is_date) {
      breaks <- pretty(x_values, n = n)
      return(as.Date(breaks))
    }

    if (!is.numeric(x_values) && !is.integer(x_values)) {
      values <- unique(as.character(x_values))
      indexes <- unique(round(seq(1, length(values), length.out = min(n, length(values)))))
      return(values[indexes])
    }

    pretty(range(x_values, na.rm = TRUE), n = n)
  }

  x_n_break_values <- if (is.null(x_breaks)) {
    make_x_n_breaks(df[[x_name]], x_n_breaks, is_date)
  } else {
    NULL
  }
  
  if (is_date && x_freq == "auto") {
    x_freq <- infer_date_frequency(df[[x_name]])
  }
  
  # =========================
  # SORTING
  # =========================

  parse_order <- function(order) {
    if (is.null(order)) return(NULL)
    if (length(order) == 0) return(NULL)

    if (is.character(order) && length(order) == 1) {
      order <- unlist(strsplit(order, ",", fixed = TRUE))
    }

    order <- trimws(as.character(order))
    order[nzchar(order)]
  }

  apply_manual_order <- function(order, groups) {
    order <- parse_order(order)
    if (is.null(order) || length(order) == 0) return(groups)

    matched <- order[order %in% groups]
    c(matched, setdiff(groups, matched))
  }
  
  manual_legend_order <- parse_order(legend_order)
  if (!is.null(manual_legend_order)) {
    group_vals <- apply_manual_order(manual_legend_order, group_vals)
  }

  col_order_values  <- group_vals
  line_order_values <- group_vals
  
  if (has_col && sort_col != "none") {
    col_totals <- df |>
      dplyr::group_by(.data[[group_name]]) |>
      dplyr::summarise(
        val = sum(.data[[y_col_name]], na.rm = TRUE),
        .groups = "drop"
      )
    
    col_order_values <- col_totals |>
      dplyr::arrange(if (sort_col == "asc") val else -val) |>
      dplyr::pull(.data[[group_name]])
  }
  
  if (has_line && sort_line != "none") {
    line_vals <- df |>
      dplyr::group_by(.data[[group_name]]) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
    
    line_order_values <- line_vals |>
      dplyr::arrange(
        if (sort_line == "asc") .data[[y_line_name]] else -.data[[y_line_name]]
      ) |>
      dplyr::pull(.data[[group_name]])
  }

  if (!is.null(manual_legend_order)) {
    col_order_values <- apply_manual_order(manual_legend_order, col_order_values)
    line_order_values <- apply_manual_order(manual_legend_order, line_order_values)
  }

  col_order_values <- apply_manual_order(col_order, col_order_values)
  line_order_values <- apply_manual_order(line_order, line_order_values)
  
  legend_order_values <- if (!is.null(manual_legend_order)) {
    apply_manual_order(manual_legend_order, unique(c(col_order_values, line_order_values)))
  } else {
    unique(c(col_order_values, line_order_values))
  }

  is_blank_label <- function(value) {
    is.null(value) || length(value) == 0 || all(is.na(value)) ||
      (is.character(value) && !nzchar(trimws(value[[1]])))
  }

  label_or_blank <- function(value) {
    if (is_blank_label(value)) "" else as.character(value[[1]])
  }

  col_label_visible <- label_or_blank(col_label)
  line_label_visible <- label_or_blank(line_label)
  col_key_label <- ".column"
  line_key_label <- ".line"
  
  # =========================
  # KEYS
  # =========================
  
  if (has_col) {
    df$col_key <- interaction(df[[group_name]], col_key_label, sep = ".")
    df$col_key <- factor(
      df$col_key,
      levels = paste(col_order_values, col_key_label, sep = ".")
    )
  }
  
  if (has_line) {
    df$line_key <- interaction(df[[group_name]], line_key_label, sep = ".")
    df$line_key <- factor(
      df$line_key,
      levels = paste(line_order_values, line_key_label, sep = ".")
    )
  }
  
  # =========================
  # LABELS
  # =========================
  
  make_labels <- function(groups, type_label) {
    type_label <- label_or_blank(type_label)
    if (!has_group) return(rep(type_label, length(groups)))
    single_measure <- xor(has_col, has_line)

    if (is.null(label_lookup)) {
      if (single_measure) return(groups)
      if (!nzchar(type_label)) return(groups)
      return(paste(groups, tolower(type_label)))
    }
    
    mapped <- label_lookup[groups]
    mapped[is.na(mapped)] <- groups[is.na(mapped)]
    
    if (single_measure) return(unname(mapped))
    if (!nzchar(type_label)) return(unname(mapped))

    paste(mapped, tolower(type_label))
  }
  
  # =========================
  # PALETTES
  # =========================
  
  align_palette <- function(pal, groups) {
    if (is.null(pal)) return(NULL)
    
    aligned <- pal[match(groups, names(pal))]
    missing <- is.na(aligned)
    
    if (any(missing)) {
      aligned[missing] <- scales::hue_pal()(sum(missing))
    }
    
    stats::setNames(aligned, groups)
  }
  
  base_cols <- if (is.null(palette)) {
    stats::setNames(scales::hue_pal()(length(group_vals)), group_vals)
  } else {
    align_palette(palette, group_vals)
  }
  
  palette_fill <- align_palette(palette_fill, group_vals)
  palette_line <- align_palette(palette_line, group_vals)
  
  if (has_col) {
    fill_palette <- stats::setNames(
      if (!is.null(palette_fill)) palette_fill else scales::alpha(base_cols, 0.7),
      paste(group_vals, col_key_label, sep = ".")
    )
    
    fill_keys   <- paste(legend_order_values, col_key_label, sep = ".")
    fill_labels <- make_labels(legend_order_values, col_label_visible)
  }
  
  if (has_line) {
    colour_palette <- stats::setNames(
      if (!is.null(palette_line)) palette_line else base_cols,
      paste(group_vals, line_key_label, sep = ".")
    )
    
    colour_keys   <- paste(legend_order_values, line_key_label, sep = ".")
    colour_labels <- make_labels(legend_order_values, line_label_visible)
  }
  
  # =========================
  # AXES
  # =========================
  
  if (has_line) {
    axis_line <- get_nice_breaks(
      max(df[[y_line_name]], na.rm = TRUE),
      min_breaks = if (line_is_primary) primary_min_breaks else secondary_min_breaks,
      max_breaks = if (line_is_primary) primary_max_breaks else secondary_max_breaks
    )
    
    if (is.null(y_line_accuracy)) {
      y_line_accuracy <- axis_line$accuracy
    }
  }
  
  if (has_col) {
    stacked <- df |>
      dplyr::group_by(.data[[x_name]]) |>
      dplyr::summarise(
        total = if (col_position == "stacked") {
          sum(.data[[y_col_name]], na.rm = TRUE)
        } else {
          max(.data[[y_col_name]], na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    axis_col <- get_nice_breaks(
      max(stacked$total, na.rm = TRUE),
      min_breaks = if (col_is_primary) primary_min_breaks else secondary_min_breaks,
      max_breaks = if (col_is_primary) primary_max_breaks else secondary_max_breaks
    )
    
    if (is.null(y_col_accuracy)) {
      y_col_accuracy <- axis_col$accuracy
    }
  }
  
  scale_factor <- if (has_line && has_col) {
    if (line_is_primary) {
      axis_line$rounded_max / axis_col$rounded_max
    } else {
      axis_col$rounded_max / axis_line$rounded_max
    }
  } else {
    1
  }

  line_plot_scale <- if (has_line && has_col && col_is_primary) scale_factor else 1
  col_plot_scale <- if (has_line && has_col && line_is_primary) scale_factor else 1
  
  ymax <- if (line_is_primary && has_line) {
    axis_line$rounded_max
  } else {
    axis_col$rounded_max
  }
  upper_limit <- if (forecast) ymax * forecast_max_mult else ymax
  
  max_plot <- max(
    if (has_line) {
      max(df[[y_line_name]] * line_plot_scale, na.rm = TRUE)
    } else {
      0
    },
    if (has_col) {
      max(stacked$total * col_plot_scale, na.rm = TRUE)
    } else {
      0
    }
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_name]]))
  
  # =========================
  # FORECAST SHADE
  # =========================
  
  if (forecast && !is.null(forecast_start) && !is.null(forecast_end)) {
    
    gradient <- matrix(
      grDevices::rgb(0, 0, 0, alpha = seq(0.25, 0, length.out = 100)),
      ncol = 1
    )
    
    label_y_base <- upper_limit * 0.98
    clearance <- upper_limit * 0.05
    min_allowed <- max_plot + clearance
    label_y <- max(label_y_base, min_allowed)
    
    if (is_date) {
      forecast_start_raw <- to_date_arg(forecast_start, "start")
      forecast_end_raw   <- to_date_arg(forecast_end, "end")
      
      xmin <- snap_period_start(
        d = forecast_start_raw,
        x_freq = x_freq,
        period_type = period_type,
        financial_start_month = financial_start_month
      )
      
      xmax <- snap_period_end(
        d = forecast_end_raw,
        x_freq = x_freq,
        period_type = period_type,
        financial_start_month = financial_start_month
      )
      
      mid_x <- xmin + (xmax - xmin) / 2
      
    } else {
      xmin <- forecast_start - 0.4
      xmax <- forecast_end + 0.4
      mid_x <- (forecast_start + forecast_end) / 2
    }
    
    p <- p +
      ggplot2::annotation_raster(
        gradient,
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = upper_limit
      ) +
      ggplot2::annotate(
        "text",
        x = mid_x,
        y = label_y,
        label = forecast_label,
        family = family,
        size = forecast_label_fontsize / ggplot2::.pt,
        vjust = 1
      )
  }
  
  # =========================
  # GEOMS
  # =========================
  
  width <- if (is_date) {
    as.numeric(diff(range(df[[x_name]], na.rm = TRUE))) /
      length(unique(df[[x_name]])) * 0.6
  } else {
    0.4
  }
  
  if (has_col) {
    p <- p +
      ggplot2::geom_col(
        ggplot2::aes(
          y = .data[[y_col_name]] * col_plot_scale,
          fill = col_key
        ),
        width = width,
        position = if (col_position == "dodge") {
          ggplot2::position_dodge(width = width)
        } else {
          "stack"
        }
      )
  }
  
  if (has_line) {
    df <- df |>
      dplyr::arrange(factor(.data[[group_name]], levels = line_order_values))
    
    p <- p +
      ggplot2::geom_line(
        data = df,
        ggplot2::aes(
          y = .data[[y_line_name]] * line_plot_scale,
          colour = line_key,
          group = .data[[group_name]]
        ),
        linewidth = 0.9
      )
  }
  
  # =========================
  # SCALES
  # =========================

  if (is.null(y_line_accuracy)) {
    y_line_accuracy <- 1
  }

  if (is.null(y_col_accuracy)) {
    y_col_accuracy <- 1
  }
  
  line_labeller <- scales::label_number(
    accuracy = y_line_accuracy * y_line_scale,
    scale = y_line_scale,
    big.mark = ","
  )
  
  col_labeller <- scales::label_number(
    accuracy = y_col_accuracy * y_col_scale,
    scale = y_col_scale,
    big.mark = ","
  )
  
  if (has_col) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = fill_palette,
        breaks = fill_keys,
        labels = fill_labels
      )
  }
  
  if (has_line) {
    p <- p +
      ggplot2::scale_colour_manual(
        values = colour_palette,
        breaks = colour_keys,
        labels = colour_labels
      )
  }
  
  if (has_line && has_col && line_is_primary) {
    p <- p +
      ggplot2::scale_y_continuous(
        name = y_line_label,
        limits = c(0, upper_limit),
        breaks = axis_line$breaks,
        labels = line_labeller,
        expand = c(0, 0),
        sec.axis = ggplot2::sec_axis(
          ~ . / scale_factor,
          name = y_col_label,
          breaks = axis_col$breaks,
          labels = col_labeller
        )
      )
  } else if (has_line && has_col && col_is_primary) {
    p <- p +
      ggplot2::scale_y_continuous(
        name = y_col_label,
        limits = c(0, upper_limit),
        breaks = axis_col$breaks,
        labels = col_labeller,
        expand = c(0, 0),
        sec.axis = ggplot2::sec_axis(
          ~ . / scale_factor,
          name = y_line_label,
          breaks = axis_line$breaks,
          labels = line_labeller
        )
      )
  } else if (has_line) {
    p <- p +
      ggplot2::scale_y_continuous(
        name = y_line_label,
        limits = c(0, upper_limit),
        breaks = axis_line$breaks,
        labels = line_labeller,
        expand = c(0, 0)
      )
  } else {
    p <- p +
      ggplot2::scale_y_continuous(
        name = y_col_label,
        limits = c(0, upper_limit),
        breaks = axis_col$breaks,
        labels = col_labeller,
        expand = c(0, 0)
      )
  }
  
  # =========================
  # X SCALE
  # =========================
  
  expand_x <- if (forecast) 0.25 else 0
  
  if (is_date) {
    
    if (x_freq == "quarterly") {
      
      start_date <- min(df[[x_name]], na.rm = TRUE)
      
      first_break <- snap_period_end(
        d = start_date,
        x_freq = "quarterly",
        period_type = period_type,
        financial_start_month = financial_start_month
      )
      
      breaks_vec <- seq(
        first_break,
        max(df[[x_name]], na.rm = TRUE),
        by = "3 months"
      )
      
      if (!is.null(x_breaks) && is.character(x_breaks)) {
        p <- p +
          ggplot2::scale_x_date(
            name = x_label,
            date_breaks = x_breaks,
            date_labels = "%b %Y",
            expand = c(0, expand_x)
          )
      } else {
        p <- p +
          ggplot2::scale_x_date(
            name = x_label,
            breaks = if (!is.null(x_breaks)) {
              x_breaks
            } else if (!is.null(x_n_break_values)) {
              x_n_break_values
            } else {
              breaks_vec
            },
            date_labels = "%b %Y",
            expand = c(0, expand_x)
          )
      }
      
    } else {
      
      date_labels <- switch(
        x_freq,
        "yearly"  = if (period_type == "financial") "FY%Y" else "%Y",
        "monthly" = "%b %Y"
      )
      
      if (!is.null(x_breaks) && !is.character(x_breaks)) {
        p <- p +
          ggplot2::scale_x_date(
            name = x_label,
            breaks = x_breaks,
            date_labels = date_labels,
            expand = c(0, expand_x)
          )
      } else if (!is.null(x_n_break_values)) {
        p <- p +
          ggplot2::scale_x_date(
            name = x_label,
            breaks = x_n_break_values,
            date_labels = date_labels,
            expand = c(0, expand_x)
          )
      } else {
        date_breaks_val <- if (!is.null(x_breaks)) {
          x_breaks
        } else {
          switch(
            x_freq,
            "yearly"  = "1 year",
            "monthly" = "1 month"
          )
        }

        p <- p +
          ggplot2::scale_x_date(
            name = x_label,
            date_breaks = date_breaks_val,
            date_labels = date_labels,
            expand = c(0, expand_x)
          )
      }
    }
    
  } else if (!is_discrete_x) {
    p <- p +
      ggplot2::scale_x_continuous(
        name = x_label,
        breaks = if (!is.null(x_breaks)) {
          x_breaks
        } else if (!is.null(x_n_break_values)) {
          x_n_break_values
        } else {
          sort(unique(df[[x_name]]))
        },
        expand = c(0, expand_x)
      )
  } else {
    discrete_breaks <- if (!is.null(x_breaks)) {
      as.character(x_breaks)
    } else if (!is.null(x_n_break_values)) {
      as.character(x_n_break_values)
    } else {
      ggplot2::waiver()
    }

    p <- p +
      ggplot2::scale_x_discrete(
        name = x_label,
        breaks = discrete_breaks,
        expand = c(0, expand_x)
      )
  }
  
  # =========================
  # THEME
  # =========================
  
  p <- p +
    theme_sopi(family = family, base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "#dad9d9"),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.title.y.right = ggplot2::element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        margin = ggplot2::margin(l = 10)
      ),
      axis.text.y.right = ggplot2::element_text(hjust = 0),
      panel.border = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.key.width  = grid::unit(4, "mm"),
      legend.key.height = grid::unit(4, "mm"),
      legend.position = "right",
      legend.justification = "top",
      legend.box.just = "left",
      legend.box = "vertical",
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.margin = ggplot2::margin(t = 0, b = 0)
    )
  
  # =========================
  # CUSTOM Y AXIS
  # =========================
  
  p <- p +
    ggplot2::annotate(
      "segment",
      x = -Inf,
      xend = -Inf,
      y = 0,
      yend = ymax,
      linewidth = 0.4,
      colour = "#dad9d9"
    )
  
  if (has_line && has_col) {
    p <- p +
      ggplot2::annotate(
        "segment",
        x = Inf,
        xend = Inf,
        y = 0,
        yend = ymax,
        linewidth = 0.4,
        colour = "#dad9d9"
      )
  }
  
  p <- p +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      colour = ggplot2::guide_legend(order = 1)
    )
  
  return(p)
}
