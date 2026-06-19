net_contribution_plot <- function(
    data,
    group,
    y,
    driver,
    total,
    
    sort = c("desc", "asc", "none"),
    other_match = c("all other", "other", "rest"),
    
    labels = NULL,
    
    x_limits = NULL,
    x_breaks = NULL,
    n_breaks = 5,
    
    y_accuracy = 0.1,
    
    fill_values = c("Volumes" = "#d6effc", "Prices" = "#0080a1"),
    fill_labels = c("Volumes" = "Volumes", "Prices" = "Prices"),
    fill_order  = c("Volumes", "Prices"),
    col_width = 0.5,
    
    point_label  = "Net contribution",
    point_colour = "#7cc688",
    point_size   = 2.5,
    
    title = NULL,
    subtitle = NULL,
    x_label = "Contribution to export revenue growth",
    y_label = NULL,
    
    family = "DIN",
    fontsize = 10,
    legend = TRUE
) {
  
  sort <- match.arg(sort)
  df <- data
  
  # =========================
  # RLING CAPTURE
  # =========================
  
  group_sym  <- rlang::ensym(group)
  y_sym      <- rlang::ensym(y)
  driver_sym <- rlang::ensym(driver)
  total_sym  <- rlang::ensym(total)
  
  group_name  <- rlang::as_string(group_sym)
  y_name      <- rlang::as_string(y_sym)
  driver_name <- rlang::as_string(driver_sym)
  total_name  <- rlang::as_string(total_sym)
  
  # =========================
  # LABELS
  # =========================
  
  groups <- as.character(unique(df[[group_name]]))
  
  if (!is.null(labels)) {
    mapped <- labels[groups]
    mapped[is.na(mapped)] <- groups[is.na(mapped)]
    df[[group_name]] <- factor(df[[group_name]], levels = groups, labels = mapped)
  }
  
  # =========================
  # ORDERING
  # =========================
  
  order_df <- df |>
    dplyr::group_by(.data[[group_name]]) |>
    dplyr::summarise(total_val = unique(.data[[total_name]])[1], .groups = "drop")
  
  is_other <- grepl(
    paste(other_match, collapse = "|"),
    tolower(as.character(order_df[[group_name]]))
  )
  
  core  <- order_df[!is_other, ]
  other <- order_df[is_other, ]
  
  if (sort == "desc") {
    core <- core[order(-core$total_val), ]
  } else if (sort == "asc") {
    core <- core[order(core$total_val), ]
  }
  
  # correct for coord_flip
  final_levels <- c(other[[group_name]], core[[group_name]])
  
  df[[group_name]] <- factor(df[[group_name]], levels = final_levels)
  
  # =========================
  # AXIS
  # =========================
  
  max_val <- max(abs(c(df[[y_name]], df[[total_name]])), na.rm = TRUE)
  
  if (is.null(x_limits)) {
    n_total <- max(3, n_breaks)
    if (n_total %% 2 == 0) n_total <- n_total + 1
    
    axis_tmp <- get_nice_breaks(max_val, 2, 6)
    axis_max <- axis_tmp$rounded_max
    
    x_limits <- c(-axis_max, axis_max)
    x_breaks <- seq(-axis_max, axis_max, length.out = n_total)
    
    if (is.null(y_accuracy)) {
      y_accuracy <- axis_tmp$accuracy
    }
    
  } else {
    if (is.null(x_breaks)) {
      x_breaks <- seq(x_limits[1], x_limits[2], length.out = n_breaks)
    }
  }
  
  # =========================
  # LEGEND
  # =========================
  
  legend_items <- tibble::tibble(
    legend_item = factor(
      c(fill_order, point_label),
      levels = c(fill_order, point_label)
    ),
    x = df[[group_name]][1],
    y = 0
  )
  
  # =========================
  # PLOT
  # =========================
  
  p <- ggplot(df, aes(x = .data[[group_name]], y = .data[[y_name]])) +
    
    geom_col(
      aes(fill = .data[[driver_name]]),
      width = col_width,
      show.legend = FALSE
    ) +
    
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "#dad9d9") +
    
    geom_point(
      aes(y = .data[[total_name]]),
      shape = 21,
      size = point_size,
      fill = point_colour,
      colour = point_colour,
      show.legend = FALSE
    ) +
    
    geom_point(
      data = legend_items,
      aes(x = x, y = y, fill = legend_item),
      inherit.aes = FALSE,
      shape = 21,
      size = 3,
      colour = NA,
      show.legend = TRUE
    ) +
    
    coord_flip(clip = "off") +
    
    scale_y_continuous(
      limits = x_limits,
      breaks = x_breaks,
      labels = scales::label_number(
        accuracy = y_accuracy,
        scale = 100,
        suffix = "%"
      ),
      expand = c(0, 0)
    ) +
    
    scale_fill_manual(
      values = c(fill_values, setNames(point_colour, point_label)),
      labels = c(fill_labels, setNames(point_label, point_label)),
      breaks = c(fill_order, point_label),
      drop = FALSE
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = y_label,
      y = x_label,
      fill = NULL
    ) +
    
    theme_sopi(
      family = family,
      base_size = fontsize
    ) +
    
    theme(
      panel.border = element_blank(),
      legend.key = element_rect(fill = NA, colour = NA),
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
    ) +
    
    guides(
      fill = guide_legend(
        keyheight = unit(5, "mm"),
        override.aes = list(
          shape = c(rep(22, length(fill_order)), 21),
          size  = c(rep(4, length(fill_order)), 3),
          fill  = c(fill_values, point_colour),
          colour = NA
        )
      )
    )
  
  if (!legend) {
    p <- p + guides(fill = "none")
  }
  
  return(p)
}
