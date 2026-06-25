plot_generic_col <- function(
    data,
    group = "sopi_forecast_group",
    measure = "measure",
    value = "value",
    n_breaks = 5,
    y_lab = "Percentage change",
    y_limits = NULL,
    y_breaks = NULL,
    y_accuracy = 1,
    col_width = 0.5,
    col_dist = 0.8,
    group_order = NULL,
    palette = NULL,
    palette_fill = NULL,
    fill_palette = NULL,
    fill_order = NULL,
    fill_labels = NULL,
    legend = TRUE,
    family = "DIN",
    fontsize = 10
) {
  
  group <- rlang::ensym(group)
  measure <- rlang::ensym(measure)
  value <- rlang::ensym(value)
  group_name <- rlang::as_string(group)
  measure_name <- rlang::as_string(measure)
  value_name <- rlang::as_string(value)
  
  # =========================
  # DATA
  # =========================

  if (!is.null(group_order)) {
    data <- data %>%
      mutate(
        !!group_name := factor(
          .data[[group_name]],
          levels = group_order
        )
      )
  }

  measure_values <- unique(as.character(data[[measure_name]]))
  measure_values <- measure_values[!is.na(measure_values) & nzchar(measure_values)]

  if (is.null(fill_order)) {
    fill_order <- measure_values
  } else {
    fill_order <- as.character(fill_order)
    fill_order <- fill_order[nzchar(fill_order)]
  }

  if (is.null(fill_labels)) {
    fill_labels <- stats::setNames(fill_order, fill_order)
  } else {
    fill_labels <- complete_labels(fill_order, fill_labels)
  }

  first_non_empty <- function(...) {
    values <- list(...)
    for (item in values) {
      if (!is.null(item) && length(item) > 0) return(item)
    }
    NULL
  }

  fill_palette <- first_non_empty(fill_palette, palette_fill, palette)
  if (!is.null(fill_palette)) {
    fill_palette <- complete_palette(fill_order, fill_palette)
  }
  
  n_groups <- n_distinct(dplyr::pull(data, !!group))
  
  # =========================
  # AXIS
  # =========================
  
  max_val <- max(abs(data[[value_name]]), na.rm = TRUE)
  
  if (is.null(y_limits)) {
    
    n_breaks <- max(3, n_breaks)
    
    if (n_breaks %% 2 == 0) {
      n_breaks <- n_breaks + 1
    }
    
    axis_tmp <- get_nice_breaks(max_val, 2, 6)
    
    axis_max <- axis_tmp$rounded_max
    
    y_limits <- c(-axis_max, axis_max)
    
    y_breaks <- seq(
      -axis_max,
      axis_max,
      length.out = n_breaks
    )
    
    if (is.null(y_accuracy)) {
      y_accuracy <- axis_tmp$accuracy
    }
    
  } else if (is.null(y_breaks)) {
    
    y_breaks <- seq(
      y_limits[1],
      y_limits[2],
      length.out = n_breaks
    )
    
  }
  
  # =========================
  # PLOT
  # =========================
  
  p <- ggplot(
    data,
    aes(
      x = !!group,
      y = !!value,
      fill = !!measure
    )
  ) +
    geom_col(
      width = col_width,
      position = position_dodge(width = col_dist)
    ) +
    geom_hline(
      yintercept = 0,
      colour = "#dad9d9",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = seq(
        1.5,
        n_groups + 0.5,
        by = 1
      ),
      linetype = "dashed",
      colour = "#dad9d9"
    ) +
    {
      if (is.null(fill_palette)) {
        scale_fill_discrete(
          breaks = fill_order,
          labels = unname(fill_labels[fill_order]),
          drop = FALSE
        )
      } else {
        scale_fill_manual(
          values = fill_palette,
          breaks = fill_order,
          labels = unname(fill_labels[fill_order]),
          drop = FALSE
        )
      }
    } +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = scales::label_percent(
        accuracy = y_accuracy
      ),
      expand = c(0, 0)
    ) +
    labs(
      x = NULL,
      y = y_lab,
      fill = NULL
    ) +
    theme_sopi(
      family = family,
      base_size = fontsize
    ) +
    theme(
      axis.line.x = element_blank(),
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

  if (!legend) {
    p <- p + guides(fill = "none")
  }

  p
}
