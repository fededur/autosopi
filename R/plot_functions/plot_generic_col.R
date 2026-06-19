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
    fill_palette = c(
      Revenue = "#1b9e77",
      Quantity = "#d95f02",
      Price = "#7570b3"
    ),
    fill_labels = c(
      revenue_yoy_change_pct = "Revenue",
      quantity_yoy_change_pct = "Quantity",
      price_yoy_change_pct = "Price"
    ),
    fill_order = c(
      "Revenue",
      "Quantity",
      "Price"
    ),
    family = "DIN",
    fontsize = 10
    
) {
  
  group <- rlang::ensym(group)
  
  # =========================
  # DATA
  # =========================
  
  plot_data <- data %>%
    arrange(!!group, year) %>%
    group_by(!!group) %>%
    mutate(
      price = revenue / quantity,
      revenue_yoy_change_pct  = revenue / lag(revenue)  - 1,
      quantity_yoy_change_pct = quantity / lag(quantity) - 1,
      price_yoy_change_pct    = price / lag(price) - 1
    ) %>%
    ungroup() %>%
    pivot_longer(
      cols = ends_with("_yoy_change_pct"),
      names_to = "measure",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      measure = recode(measure, !!!fill_labels),
      measure = factor(measure, levels = fill_order)
    ) %>%
    filter(year == max(year))
  
  
  if (!is.null(group_order)) {
    plot_data <- plot_data %>%
      mutate(
        sopi_forecast_group = factor(
          sopi_forecast_group,
          levels = group_order
        )
      )
  }
  
  n_groups <- n_distinct(dplyr::pull(plot_data, !!group))
  
  # =========================
  # AXIS
  # =========================
  
  max_val <- max(abs(plot_data$value), na.rm = TRUE)
  
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
    plot_data,
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

# p2 <- plot_generic_col(t %>% filter(year<2027,
#                                     sopi_forecast_group %in% c("Mutton","Wool","Lamb","Beef and Veal","Venison")) ,
#                        fill_palette = build_color_palette(c("Revenue","Quantity","Price"),meat_colours[1:3]),
#                  fill_order = c(
#                    "Revenue","Price",
#                    "Quantity"
#                    ),
#                  col_width = 0.5,
#                  col_dist = 0.6,
#                  y_limits = c(-0.2,0.4),
#                  group_order = c("Beef and Veal","Lamb", "Mutton","Venison","Wool"),
#                  family = "DIN",
#                  fontsize = 10
#                  
#                  )
# 
# ggsave(
#   filename = "plot_generic_col.svg",
#   plot = p2,
#   width = 180,
#   height = 100,
#   units = "mm"
# )
# 
# 
# meat_colours <- get_palette(level = "forecast_group", sector = "Meat and Wool", metadata_table = sector_metadata)
# 
# metadata_path <- "J:/NEFD/SOPI Graphs/metadata/sopi_metadata.xlsx"
# sector_metadata <- read_metadata(metadata_path)
# 
# 
# 
# 
# 
# build_color_palette(c("Revenue","Quantity","Price"),meat_colours[1:3])
