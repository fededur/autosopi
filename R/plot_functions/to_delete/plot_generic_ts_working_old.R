plot_generic_ts <- function(
    data,
    x,
    x_freq = c("auto", "yearly", "quarterly", "monthly"),
    y_line = NULL,
    y_col  = NULL,
    group = NULL,
    y_line_label = "Export revenue (NZ$)",
    y_col_label  = "Export volume (Tonnes)",
    x_label  = NULL,
    x_breaks = NULL,  
    y_line_accuracy = NULL,
    y_col_accuracy  = NULL,
    primary_min_breaks = 3,
    primary_max_breaks = 6,
    secondary_min_breaks = 3,
    secondary_max_breaks = 6,
    line_label = "Revenue",
    col_label  = "Volume",
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
    sort_col = c("none", "asc", "desc"),
    sort_line = c("none", "asc", "desc")
    
) {
  
  sort_col  <- match.arg(sort_col)
  
  sort_line <- match.arg(sort_line)
  
  x_freq    <- match.arg(x_freq)
  
  col_position <- match.arg(col_position)
  
  df <- data
  
  # =========================
  # NAMES
  # =========================
  
  x_name <- if (is.character(x)) x else deparse(substitute(x))
  
  y_line_name <- if (is.null(y_line)) NULL else if (is.character(y_line)) y_line else deparse(substitute(y_line))
  
  y_col_name  <- if (is.null(y_col))  NULL else if (is.character(y_col))  y_col  else deparse(substitute(y_col))
  
  group_name  <- if (is.null(group)) NULL else if (is.character(group)) group else deparse(substitute(group))

  has_group <- !is.null(group_name)

  if (!has_group) {
    group_name <- ".generic_ts_group"
    df[[group_name]] <- "All"
  }

  df[[x_name]] <- parse_flexible_date(df[[x_name]])
  
  has_line <- !is.null(y_line_name)
  
  has_col  <- !is.null(y_col_name)
  
  if (!has_line && !has_col) stop("At least one of y_line or y_col must be provided")
  
  label_lookup <- labels
  
  group_vals <- unique(as.character(df[[group_name]]))
  
  is_date <- inherits(df[[x_name]], c("Date", "POSIXct", "POSIXlt"))

  if (is_date && x_freq == "auto") {
    x_freq <- infer_date_frequency(df[[x_name]])
  }
  
  # =========================
  # SORTING
  # =========================
  
  col_order  <- group_vals
  
  line_order <- group_vals 
  
  if (has_col && sort_col != "none") {
    col_totals <- df |>
      dplyr::group_by(.data[[group_name]]) |>
      dplyr::summarise(val = sum(.data[[y_col_name]], na.rm = TRUE), .groups = "drop")
    
    col_order <- col_totals |>
      dplyr::arrange(if (sort_col == "asc") val else -val) |>
      dplyr::pull(.data[[group_name]])
  }
  
  if (has_line && sort_line != "none") {
    line_vals <- df |>
      dplyr::group_by(.data[[group_name]]) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()   
    
    line_order <- line_vals |>
      dplyr::arrange(if (sort_line == "asc") .data[[y_line_name]] else -.data[[y_line_name]]) |>
      dplyr::pull(.data[[group_name]])
  }
  
  legend_order <- unique(c(col_order, line_order))
  
  # =========================
  # KEYS
  # =========================
  
  if (has_col) {
    df$col_key <- interaction(df[[group_name]], col_label, sep = ".")
    df$col_key <- factor(df$col_key, levels = paste(col_order, col_label, sep = "."))
  }
  
  if (has_line) {
    df$line_key <- interaction(df[[group_name]], line_label, sep = ".")
    df$line_key <- factor(df$line_key, levels = paste(line_order, line_label, sep = "."))
  }
  
  # =========================
  # LABELS
  # =========================
  
  make_labels <- function(groups, type_label) {
    if (!has_group) return(rep(type_label, length(groups)))
    if (is.null(label_lookup)) return(paste(groups, tolower(type_label)))
    mapped <- label_lookup[groups]
    mapped[is.na(mapped)] <- groups[is.na(mapped)]
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
    setNames(scales::hue_pal()(length(group_vals)), group_vals)
  } else {
    align_palette(palette, group_vals)
  }
  
  palette_fill <- align_palette(palette_fill, group_vals)
  
  palette_line <- align_palette(palette_line, group_vals)
  
  if (has_col) {
    fill_palette <- setNames(
      if (!is.null(palette_fill)) palette_fill else scales::alpha(base_cols, 0.7),
      paste(group_vals, col_label, sep = ".")
    )
    
    fill_keys   <- paste(legend_order, col_label, sep = ".")
    fill_labels <- make_labels(legend_order, col_label)
  }
  
  if (has_line) {
    colour_palette <- setNames(
      if (!is.null(palette_line)) palette_line else base_cols,
      paste(group_vals, line_label, sep = ".")
    )
    
    colour_keys   <- paste(legend_order, line_label, sep = ".")
    colour_labels <- make_labels(legend_order, line_label)
    
  }
  
  # =========================
  # AXES
  # ========================= 
  
  if (has_line) {
    axis_line <- get_nice_breaks(
      max(df[[y_line_name]], na.rm = TRUE),
      min_breaks = primary_min_breaks,
      max_breaks = primary_max_breaks
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
      max(stacked$total),
      min_breaks = if (has_line) secondary_min_breaks else primary_min_breaks,
      max_breaks = if (has_line) secondary_max_breaks else primary_max_breaks
    )
    
    if (is.null(y_col_accuracy)) {
      y_col_accuracy <- axis_col$accuracy
    }
  }
  
  
  scale_factor <- if (has_line && has_col) axis_line$rounded_max / axis_col$rounded_max else 1
  
  ymax <- if (has_line) axis_line$rounded_max else axis_col$rounded_max
  
  upper_limit <- if (forecast) ymax * forecast_max_mult else ymax
  
  max_plot <- max(
    if (has_line) max(df[[y_line_name]], na.rm = TRUE) else 0,
    if (has_col) max(stacked$total * scale_factor, na.rm = TRUE) else 0
  )
  
  p <- ggplot(df, aes(x = .data[[x_name]]))
  
  # =========================
  # FORECAST
  # =========================
  forecast_start_raw <- forecast_start
  forecast_end_raw   <- forecast_end
  
  if (is_date && !is.null(forecast_start_raw)) {   
    
    # convert numeric → Date
    if (is.numeric(forecast_start_raw)) {
      forecast_start_raw <- as.Date(paste0(forecast_start_raw, "-01-01"))
    }
    
    if (is.numeric(forecast_end_raw)) {
      forecast_end_raw <- as.Date(paste0(forecast_end_raw, "-12-31"))
    }
    
    # create snapped versions (DO NOT overwrite raw)
    forecast_start_snap <- forecast_start_raw
    forecast_end_snap   <- forecast_end_raw   
    
    if (x_freq == "yearly") {
      
      forecast_start_snap <- as.Date(format(forecast_start_raw, "%Y-01-01"))
      forecast_end_snap   <- as.Date(format(forecast_end_raw, "%Y-12-31"))
      
    } else if (x_freq == "quarterly") {
      q_start <- function(d) {
        m <- as.integer(format(d, "%m"))
        q <- ((m - 1) %/% 3) * 3 + 1
        as.Date(sprintf("%s-%02d-01", format(d, "%Y"), q))
      }
      
      q_end <- function(d) {
        m <- as.integer(format(d, "%m"))
        q <- ((m - 1) %/% 3 + 1) * 3
        last <- as.Date(sprintf("%s-%02d-01", format(d, "%Y"), q)) + 31
        as.Date(format(last, "%Y-%m-01")) - 1
      }
      forecast_start_snap <- q_start(forecast_start_raw)
      forecast_end_snap   <- q_end(forecast_end_raw)
      
    } else if (x_freq == "monthly") {
      forecast_start_snap <- as.Date(format(forecast_start_raw, "%Y-%m-01"))    
      next_month <- as.Date(format(forecast_end_raw, "%Y-%m-01")) + 31
      forecast_end_snap <- as.Date(format(next_month, "%Y-%m-01")) - 1
    }
  }
  
  if (forecast && !is.null(forecast_start) && !is.null(forecast_end)) {   
    
    gradient <- matrix(
      rgb(0, 0, 0, alpha = seq(0.25, 0, length.out = 100)),
      ncol = 1
    )
    
    forecast_label_rel <- 0.98  # always 85% up the shaded region   
    
    label_y_base <- upper_limit * forecast_label_rel   
    
    clearance <- (upper_limit - 0) * 0.05   # 5% of full height
    
    min_allowed <- max_plot + clearance  
    
    label_y <- max(label_y_base, min_allowed)
    
    # =========================
    # FORECAST RANGE
    # =========================  
    
    if (is_date) {   
      
      # convert numeric → Date if needed
      forecast_start_raw <- if (is.numeric(forecast_start)) {
        as.Date(paste0(forecast_start, "-01-01"))
      } else {
        forecast_start
      }
      
      forecast_end_raw <- if (is.numeric(forecast_end)) {
        as.Date(paste0(forecast_end, "-12-31"))
      } else {
        forecast_end
      }
      
      # expand to full period (THIS fixes the width issue)
      if (x_freq == "yearly") {
        xmin <- as.Date(format(forecast_start_raw, "%Y-01-01"))
        xmax <- as.Date(format(forecast_end_raw,   "%Y-12-31"))
      } else if (x_freq == "quarterly") {
        xmin <- as.Date(cut(forecast_start_raw, "quarter"))
        xmax <- as.Date(cut(forecast_end_raw, "quarter")) + 92
        xmax <- as.Date(format(xmax, "%Y-%m-01")) - 1
      } else if (x_freq == "monthly") {
        xmin <- as.Date(format(forecast_start_raw, "%Y-%m-01"))
        xmax <- as.Date(format(forecast_end_raw, "%Y-%m-01")) + 31
        xmax <- as.Date(format(xmax, "%Y-%m-01")) - 1
      }
    } else {  
      xmin <- forecast_start - 0.4
      xmax <- forecast_end + 0.4
    }   
    
    mid_x <- if (is_date) {
      forecast_start_raw + (forecast_end_raw - forecast_start_raw) / 2
    } else {
      (forecast_start + forecast_end) / 2
    } 
    
    p <- p +
      annotation_raster(
        gradient,
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = upper_limit
      ) +
      annotate("text",
               x = mid_x,
               y = label_y,
               label = forecast_label,
               family = family,
               size = forecast_label_fontsize / ggplot2::.pt,
               vjust = 1)
  }
  
  # =========================
  # GEOMS
  # =========================
  
  width = if (is_date) {
    diff(range(df[[x_name]])) / length(unique(df[[x_name]])) * 0.6
  } else {
    0.4
  }
  
  if (has_col) {
    
    p <- p +
      geom_col(
        aes(y = .data[[y_col_name]] * scale_factor, fill = col_key),
        width = width,
        position = if (col_position == "dodge") {
          position_dodge(width = width)
        } else {
          "stack"
        }
      )
  }
  
  if (has_line) {
    df <- df |>
      dplyr::arrange(factor(.data[[group_name]], levels = line_order))
    
    p <- p +
      geom_line(
        data = df,
        aes(
          y = .data[[y_line_name]],
          colour = line_key,
          group = .data[[group_name]]
        ),
        linewidth = 0.9
      )
  }
  
  # =========================
  # SCALES
  # =========================
  line_labeller <- scales::label_number(
    accuracy = y_line_accuracy,
    big.mark = ","
  ) 
  
  col_labeller <- scales::label_number(
    accuracy = y_col_accuracy,
    big.mark = ","
  )
  
  if (has_col) {
    p <- p + scale_fill_manual(values = fill_palette, breaks = fill_keys, labels = fill_labels)
  }
  
  if (has_line) {
    p <- p + scale_colour_manual(values = colour_palette, breaks = colour_keys, labels = colour_labels)
  }
  
  if (has_line && has_col) {
    p <- p +
      scale_y_continuous(
        name = y_line_label,
        limits = c(0, upper_limit),
        breaks = axis_line$breaks,
        labels = line_labeller,
        expand = c(0, 0),
        sec.axis = sec_axis(~ . / scale_factor,
                            name = y_col_label,
                            breaks = axis_col$breaks,
                            labels = col_labeller)
      )
  } else if (has_line) {
    p <- p +
      scale_y_continuous(
        name = y_line_label,
        limits = c(0, upper_limit),
        breaks = axis_line$breaks,
        labels = line_labeller,
        expand = c(0, 0)
      )
  } else {
    p <- p +
      scale_y_continuous(
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
      
      first_q_end <- as.Date(cut(start_date, "quarter")) + 92
      first_q_end <- as.Date(format(first_q_end, "%Y-%m-01")) - 1     
      breaks_vec <- seq(first_q_end, max(df[[x_name]], na.rm = TRUE), by = "3 months")
      
      p <- p +
        scale_x_date(
          name = x_label,
          breaks = if (!is.null(x_breaks)) x_breaks else breaks_vec,
          date_labels = "%b %Y",
          expand = c(0, expand_x)
        )   
    } else {   
      date_labels <- switch(
        x_freq,
        "yearly"  = "%Y",
        "monthly" = "%b %Y"
      )    
      
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
        scale_x_date(
          name = x_label,
          date_breaks = date_breaks_val,
          date_labels = date_labels,
          expand = c(0, expand_x)
        )
    } 
  } else { 
    p <- p +
      scale_x_continuous(
        name = x_label,
        breaks = if (!is.null(x_breaks)) x_breaks else sort(unique(df[[x_name]])),
        expand = c(0, expand_x)
      )
  }
  
  # =========================
  # THEME
  # =========================
  p <- p +
    theme_sopi(family = family) +
    theme(
      axis.line = element_blank(),
      axis.line.x = element_line(colour = "#dad9d9"), 
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(l = 10)
      ),
      axis.text.y.right = element_text(hjust = 0),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.key.width  = unit(4, "mm"),
      legend.key.height = unit(4, "mm"),
      legend.position = "right",
      legend.justification = "top",
      legend.box.just = "left",
      legend.box = "vertical",
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
      legend.margin = margin(t = 0, b = 0),
      legend.box.margin = margin(t = 0, b = 0) 
    )
  
  # =========================
  # CUSTOM Y AXIS
  # =========================
  p <- p +
    annotate("segment", x=-Inf, xend=-Inf, y=0, yend=ymax,
             linewidth=0.4, colour="#dad9d9") 
  if (has_line && has_col) {
    p <- p +
      annotate("segment", x=Inf, xend=Inf, y=0, yend=ymax,
               linewidth=0.4, colour="#dad9d9")
  }
  
  p <- p +
    guides(
      fill = guide_legend(order = 1),
      colour = guide_legend(order = 1)
    )
  return(p)
}

#plot_sopi <- generic_ts_plot

