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
    fill_values = c("Volume contribution" = "#d6effc", "Price contribution" = "#0080a1"),
    fill_labels = c(
      "Volumes" = "Volume contribution",
      "Prices" = "Price contribution",
      "Volume contribution" = "Volume contribution",
      "Price contribution" = "Price contribution"
    ),
    fill_label = NULL,
    fill_order = c("Volume contribution", "Price contribution"),
    col_width = 0.5,

    point_label = "Net contribution",
    point_colour = "#7cc688",
    point_size = 2.5,

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

  group_sym <- rlang::ensym(group)
  y_sym <- rlang::ensym(y)
  driver_sym <- rlang::ensym(driver)
  total_sym <- rlang::ensym(total)

  group_name <- rlang::as_string(group_sym)
  y_name <- rlang::as_string(y_sym)
  driver_name <- rlang::as_string(driver_sym)
  total_name <- rlang::as_string(total_sym)

  df[[driver_name]] <- dplyr::recode(
    as.character(df[[driver_name]]),
    "Volumes" = "Volume contribution",
    "Volume" = "Volume contribution",
    "volume" = "Volume contribution",
    "Quantity" = "Volume contribution",
    "quantity" = "Volume contribution",
    "Prices" = "Price contribution",
    "Price" = "Price contribution",
    "price" = "Price contribution",
    .default = as.character(df[[driver_name]])
  )

  if (!is.null(fill_label)) {
    fill_labels <- fill_label
  }

  normalise_driver_names <- function(x) {
    dplyr::recode(
      as.character(x),
      "Volumes" = "Volume contribution",
      "Volume" = "Volume contribution",
      "volume" = "Volume contribution",
      "Quantity" = "Volume contribution",
      "quantity" = "Volume contribution",
      "Prices" = "Price contribution",
      "Price" = "Price contribution",
      "price" = "Price contribution",
      .default = as.character(x)
    )
  }

  fill_order <- normalise_driver_names(fill_order)
  fill_order <- fill_order[nzchar(fill_order)]
  fill_order <- unique(fill_order)

  if (!is.null(legend_order)) {
    legend_order <- normalise_driver_names(legend_order)
    legend_order <- legend_order[nzchar(legend_order)]
    fill_order <- setdiff(legend_order, point_label)
  }

  if (length(fill_order) == 0) {
    fill_order <- unique(as.character(df[[driver_name]]))
  }

  normalise_named_vector <- function(x) {
    if (is.null(x) || length(x) == 0) return(x)
    x <- as.character(x)
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      names(x) <- normalise_driver_names(names(x))
    }
    x
  }

  fill_labels <- normalise_named_vector(fill_labels)
  fill_labels <- complete_labels(fill_order, fill_labels)

  fill_display_labels <- stats::setNames(unname(fill_labels[fill_order]), fill_order)
  legend_keys <- c(fill_order, point_label)
  legend_display_labels <- c(unname(fill_display_labels), point_label)

  first_non_empty <- function(...) {
    values <- list(...)
    for (value in values) {
      if (!is.null(value) && length(value) > 0) return(value)
    }
    NULL
  }

  align_combined_palette <- function(pal, raw_keys, display_labels, fallback) {
    if (is.null(pal) || length(pal) == 0) {
      return(stats::setNames(fallback[seq_along(raw_keys)], raw_keys))
    }

    pal <- as.character(pal)
    pal <- pal[!is.na(pal) & nzchar(pal)]
    if (length(pal) == 0) {
      return(stats::setNames(fallback[seq_along(raw_keys)], raw_keys))
    }

    out <- stats::setNames(rep(NA_character_, length(raw_keys)), raw_keys)

    if (!is.null(names(pal)) && any(nzchar(names(pal)))) {
      named <- pal[!is.na(names(pal)) & nzchar(names(pal))]
      names(named) <- normalise_driver_names(names(named))

      matched_raw <- intersect(raw_keys, names(named))
      out[matched_raw] <- named[matched_raw]

      display_lookup <- stats::setNames(raw_keys, display_labels)
      display_names <- names(pal)[!is.na(names(pal)) & nzchar(names(pal))]
      display_matched <- intersect(display_names, names(display_lookup))
      if (length(display_matched) > 0) {
        raw_from_display <- display_lookup[display_matched]
        out[raw_from_display] <- unname(pal[display_matched])
      }

      pool <- unname(named[!names(named) %in% raw_keys])
    } else {
      pool <- unname(pal)
    }

    missing <- is.na(out) | !nzchar(out)
    pool <- pool[!is.na(pool) & nzchar(pool)]

    if (any(missing)) {
      if (length(pool) < sum(missing)) {
        pool <- c(pool, scales::hue_pal()(sum(missing) - length(pool)))
      }
      out[missing] <- pool[seq_len(sum(missing))]
    }

    out
  }

  palette_source <- first_non_empty(palette_fill, palette, fill_values)
  fallback_colours <- c("#d6effc", "#0080a1", "#7cc688")
  if (length(fallback_colours) < length(legend_keys)) {
    fallback_colours <- c(
      fallback_colours,
      scales::hue_pal()(length(legend_keys) - length(fallback_colours))
    )
  }

  legend_values <- align_combined_palette(
    pal = palette_source,
    raw_keys = legend_keys,
    display_labels = legend_display_labels,
    fallback = fallback_colours
  )

  fill_values <- legend_values[fill_order]
  point_colour <- unname(legend_values[[point_label]])
  legend_display_keys <- legend_display_labels
  legend_display_values <- stats::setNames(unname(legend_values), legend_display_keys)
  scale_values <- c(
    stats::setNames(unname(fill_values), fill_order),
    legend_display_values
  )

  groups <- as.character(unique(df[[group_name]]))

  if (!is.null(labels)) {
    mapped <- labels[groups]
    mapped[is.na(mapped)] <- groups[is.na(mapped)]
    df[[group_name]] <- factor(df[[group_name]], levels = groups, labels = mapped)
  }

  order_df <- df |>
    dplyr::group_by(.data[[group_name]]) |>
    dplyr::summarise(total_val = unique(.data[[total_name]])[1], .groups = "drop")

  is_other <- grepl(
    paste(other_match, collapse = "|"),
    tolower(as.character(order_df[[group_name]]))
  )

  core <- order_df[!is_other, ]
  other <- order_df[is_other, ]

  if (sort == "desc") {
    core <- core[order(-core$total_val), ]
  } else if (sort == "asc") {
    core <- core[order(core$total_val), ]
  }

  final_levels <- c(other[[group_name]], core[[group_name]])
  df[[group_name]] <- factor(df[[group_name]], levels = final_levels)

  stacked_extent <- df |>
    dplyr::group_by(.data[[group_name]]) |>
    dplyr::summarise(
      positive_stack = sum(dplyr::if_else(.data[[y_name]] > 0, .data[[y_name]], 0), na.rm = TRUE),
      negative_stack = sum(dplyr::if_else(.data[[y_name]] < 0, .data[[y_name]], 0), na.rm = TRUE),
      .groups = "drop"
    )

  max_val <- max(
    abs(c(
      df[[y_name]],
      df[[total_name]],
      stacked_extent$positive_stack,
      stacked_extent$negative_stack
    )),
    na.rm = TRUE
  )

  if (!is.finite(max_val)) {
    stop("plot_net_contribution() could not calculate an axis range from the data.", call. = FALSE)
  }

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
    x_limits <- range(c(x_limits, -max_val, max_val), finite = TRUE)
    if (is.null(x_breaks)) {
      x_breaks <- seq(x_limits[1], x_limits[2], length.out = n_breaks)
    }
  }

  legend_items <- tibble::tibble(
    legend_item = factor(legend_display_keys, levels = legend_display_keys),
    x = df[[group_name]][1],
    y = 0
  )

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

    coord_flip(ylim = x_limits, clip = "off") +

    scale_y_continuous(
      breaks = x_breaks,
      labels = scales::label_number(
        accuracy = y_accuracy,
        scale = 100,
        suffix = "%"
      ),
      expand = c(0, 0)
    ) +

    scale_fill_manual(
      values = scale_values,
      breaks = legend_display_keys,
      labels = legend_display_keys,
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
      legend.key.width = unit(4, "mm"),
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
          size = c(rep(4, length(fill_order)), 3),
          fill = unname(legend_display_values),
          colour = NA
        )
      )
    )

  if (!legend) {
    p <- p + guides(fill = "none")
  }

  return(p)
}
