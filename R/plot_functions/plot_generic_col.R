plot_generic_col <- function(
    data,
    group = sopi_forecast_group,
    n_breaks = 5,
    y_lab = "Percentage change",
    y_limits = NULL,
    y_breaks = NULL,
    y_accuracy = 1,
    col_width = 0.5,
    col_dist = 0.8,
    group_order = NULL,
    fill_palette = NULL,
    fill_order = NULL,
    family = "DIN",
    fontsize = 10
) {
  
  group <- rlang::ensym(group)
  
  # =========================
  # DATA
  # =========================

  if (!is.null(group_order)) {
    data <- data %>%
      mutate(
        sopi_forecast_group = factor(
          sopi_forecast_group,
          levels = group_order
        )
      )
  }
  
  n_groups <- n_distinct(dplyr::pull(data, !!group))
  
  # =========================
  # AXIS
  # =========================
  
  max_val <- max(abs(data$value), na.rm = TRUE)
  
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
  
  ggplot(
    data,
    aes(
      x = !!group,
      y = value,
      fill = measure
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
    scale_fill_manual(
      values = fill_palette,
      breaks = fill_order
    ) +
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
}
