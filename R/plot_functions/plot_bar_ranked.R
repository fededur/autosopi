plot_bar_ranked <- function(
    data,
    x,
    y,
    group = NULL,
    n = 10,
    title = NULL,
    subtitle = NULL,
    x_label = NULL,
    y_label = NULL,
    palette = NULL,
    family = "DIN",
    base_size = 10.5,
    sort_desc = TRUE,
    ...
) {
  df <- data |>
    dplyr::mutate(
      .x = as.character(.data[[x]]),
      .y = .data[[y]]
    ) |>
    dplyr::arrange(if (sort_desc) dplyr::desc(.data$.y) else .data$.y) |>
    dplyr::slice_head(n = n)

  df$.x <- factor(df$.x, levels = rev(df$.x))

  fill_mapping <- if (!is.null(group)) ggplot2::aes(fill = .data[[group]]) else NULL

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$.y)) +
    ggplot2::geom_col(fill = if (is.null(group)) "#0072CE" else NULL, mapping = fill_mapping) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = c(0, 0.05)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme_sopi(family = family, base_size = base_size) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#eeeeee"),
      axis.line.y = ggplot2::element_blank()
    )

  if (!is.null(group) && !is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  }

  p
}
