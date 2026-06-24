plot_net_contribution <- function(
    data,
    group = "category",
    y = "contribution",
    driver = "driver",
    total = "net_contribution",
    
    sort = c("desc", "asc", "none"),
    other_match = c("all other", "other", "rest"),
    
    labels = NULL,
    legend_order = NULL,
    
    x_limits = NULL,
    x_breaks = NULL,
    n_breaks = 5,
    
    y_accuracy = 0.1,
    
    palette = NULL,
    palette_fill = NULL,
    fill_values = NULL,
    fill_labels = NULL,
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

  df[[driver_name]] <- dplyr::recode(
    as.character(df[[driver_name]]),
    "Volume" = "Volumes",
    "volume" = "Volumes",
    "Quantity" = "Volumes",
    "quantity" = "Volumes",
    "Price" = "Prices",
    "price" = "Prices",
    .default = as.character(df[[driver_name]])
  )
  
  # =========================
  # LABELS
  # =========================
  
  groups <- as.character(unique(df[[group_name]]))
  
  if (!is.null(labels)) {
    mapped <- labels[groups]
    mapped[is.na(mapped)] <- groups[is.na(mapped)]
    df[[group_name]] <- factor(df[[group_name]], levels = groups, labels = mapped)
  }

  if (!is.null(legend_order)) {
    fill_order <- setdiff(as.character(legend_order), point_label)
  }

  fill_order <- as.character(fill_order)
  fill_order <- fill_order[nzchar(fill_order)]
  fill_order <- unique(fill_order)
  if (length(fill_order) == 0) {
    fill_order <- unique(as.character(df[[driver_name]]))
  }

  legend_keys <- c(fill_order, point_label)

  align_legend_palette <- function(pal, keys) {
    if (is.null(pal) || length(pal) == 0) return(NULL)

    pal <- as.character(pal)
    if (!is.null(names(pal)) && any(nzchar(names(pal)))) {
      named <- pal[!is.na(names(pal)) & nzchar(names(pal))]
      matched <- named[keys]
      missing <- is.na(matched) | !nzchar(matched)

      if (any(missing)) {
        unused <- unname(named[!names(named) %in% keys])
        unused <- unused[!is.na(unused) & nzchar(unused)]
        if (length(unused) > 0) {
          matched[missing] <- unused[seq_len(min(sum(missing), length(unused)))]
        }
      }

      if (all(!is.na(matched) & nzchar(matched))) {
        return(stats::setNames(matched, keys))
      }
    }

    values <- unname(pal)
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) return(NULL)

    if (length(values) < length(keys)) {
      values <- c(values, scales::hue_pal()(length(keys) - length(values)))
    }

    stats::setNames(values[seq_along(keys)], keys)
  }

  first_non_empty <- function(...) {
    values <- list(...)
    for (value in values) {
      if (!is.null(value) && length(value) > 0) return(value)
    }
    NULL
  }

  palette_source <- first_non_empty(fill_values, palette_fill, palette)
  legend_palette <- align_legend_palette(palette_source, legend_keys)

  if (is.null(legend_palette)) {
    fallback_colours <- c("#d6effc", "#0080a1", "#7cc688")
    if (length(fallback_colours) < length(legend_keys)) {
      fallback_colours <- c(
        fallback_colours,
        scales::hue_pal()(length(legend_keys) - length(fallback_colours))
      )
    }
    legend_palette <- stats::setNames(fallback_colours[seq_along(legend_keys)], legend_keys)
    legend_palette <- complete_palette(legend_keys, legend_palette)
  }

  fill_values <- legend_palette[fill_order]

  mapped_point_colour <- legend_palette[[point_label]]
  if (!is.null(mapped_point_colour) && length(mapped_point_colour) > 0 && !is.na(mapped_point_colour) && nzchar(mapped_point_colour)) {
    point_colour <- unname(mapped_point_colour)
  }

  if (is.null(fill_labels)) {
    default_fill_labels <- c(
      "Volumes" = "Volume contribution",
      "Prices" = "Price contribution"
    )
    default_fill_labels[[point_label]] <- point_label
    fill_labels <- complete_labels(legend_keys, default_fill_labels)
  } else {
    fill_labels <- as.character(fill_labels)

    if (!is.null(names(fill_labels)) && any(nzchar(names(fill_labels)))) {
      names(fill_labels) <- dplyr::recode(
        names(fill_labels),
        "Volume" = "Volumes",
        "volume" = "Volumes",
        "Quantity" = "Volumes",
        "quantity" = "Volumes",
        "Price" = "Prices",
        "price" = "Prices",
        .default = names(fill_labels)
      )
    }

    fill_labels <- complete_labels(legend_keys, fill_labels)
  }
  legend_labels <- stats::setNames(unname(fill_labels[legend_keys]), legend_keys)
  
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
      legend_keys,
      levels = legend_keys
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
      labels = legend_labels,
      breaks = legend_keys,
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
