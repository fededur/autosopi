plot_slaughter_trends <- function(
    data,
    y_line = "cumulative_value",
    month_year = NULL,
    y_lab = "Total slaughter (million head)",
    y_min_breaks = 4,
    y_max_breaks = 5,
    y_line_accuracy = 1,
    colour_palette = NULL,
    
    family = "DIN"
) {
  
  # -------------------------
  # MONTHS
  # -------------------------
  month_levels <- c(
    "Jul","Aug","Sep","Oct","Nov","Dec",
    "Jan","Feb","Mar","Apr","May","Jun"
  )
  
  # -------------------------
  # OPTIONAL FILTER
  # -------------------------
  if (!is.null(month_year)) {
    parsed_date <- parse_date_time(month_year, orders = "b Y")
    input_month <- month(parsed_date)
    input_year  <- year(parsed_date)
    
    fy_end_year <- ifelse(input_month >= 7, input_year + 1, input_year)
    cutoff_index <- (input_month - 7) %% 12 + 1
    
    data <- data %>%
      filter(
        season_end_year < fy_end_year |
          (season_end_year == fy_end_year & month_index <= cutoff_index)
      )
  }
  
  # -------------------------
  # YEARS
  # -------------------------
  current_end <- max(data$season_end_year, na.rm = TRUE)
  five_years  <- (current_end - 5):(current_end - 1)
  two_years   <- (current_end - 1):current_end
  
  slaughter_complete <- data %>%
    filter(n_months == 12, season_end_year %in% five_years)
  
  # -------------------------
  # 5-YEAR STATS
  # -------------------------
  slaughter_5yr <- slaughter_complete %>%
    group_by(month_index) %>%
    summarise(
      average_slaughter = mean(cumulative_value, na.rm = TRUE),
      min_slaughter     = min(cumulative_value, na.rm = TRUE),
      max_slaughter     = max(cumulative_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      average_label = paste0(min(five_years), "-", substr(max(five_years), 3, 4), " average"),
      range_label   = paste0(min(five_years), "-", substr(max(five_years), 3, 4), " range")
    )
  
  slaughter_2yr <- data %>%
    filter(season_end_year %in% two_years) %>%
    arrange(season_end_year, month_index)
  
  # -------------------------
  # LEGEND KEYS (CRITICAL FIX)
  # -------------------------
  legend_keys <- unique(c(
    slaughter_5yr$average_label,
    slaughter_5yr$range_label,
    slaughter_2yr$season
  ))
  
  # -------------------------
  # AXIS
  # -------------------------
  axis_line <- get_nice_breaks(
    max(data[[y_line]], na.rm = TRUE),
    min_breaks = y_min_breaks,
    max_breaks = y_max_breaks
  )
  
  if (is.null(y_line_accuracy)) {
    y_line_accuracy <- axis_line$accuracy
  }
  
  upper_limit <- axis_line$rounded_max
  
  line_labeller <- label_number(
    accuracy = y_line_accuracy,
    big.mark = ","
  )
  
  # -------------------------
  # PALETTE
  # -------------------------
  if (is.null(colour_palette)) {
    palette <- setNames(
      scales::hue_pal()(length(legend_keys)),
      legend_keys
    )
  } else {
    palette <- build_color_palette(
      legend_keys,
      c("#3D3D3D", "#dbdcde", colour_palette[1:2])
    )
  }
  
  # -------------------------
  # LEGEND TYPES (SAFE)
  # -------------------------
  legend_types <- dplyr::case_when(
    legend_keys %in% slaughter_5yr$average_label ~ "average",
    legend_keys %in% slaughter_5yr$range_label   ~ "range",
    TRUE                                        ~ "season"
  )
  
  override_linetype <- dplyr::case_when(
    legend_types == "average" ~ "dashed",
    legend_types == "season"  ~ "solid",
    legend_types == "range"   ~ "blank"
  )
  
  override_shape <- ifelse(legend_types == "range", 15, NA)
  
  override_fill <- ifelse(
    legend_types == "range",
    palette[legend_keys],
    NA
  )
  
  # -------------------------
  # PLOT
  # -------------------------
  ggplot(slaughter_5yr, aes(x = month_index)) +
    
    geom_ribbon(
      aes(
        ymin = min_slaughter,
        ymax = max_slaughter,
        fill = factor(range_label, levels = legend_keys)
      ),
      alpha = 0.25
    ) +
    
    geom_line(
      aes(
        y = average_slaughter,
        colour = factor(average_label, levels = legend_keys),
        linetype = factor(average_label, levels = legend_keys)
      ),
      linewidth = 0.6
    ) +
    
    geom_line(
      data = slaughter_2yr,
      aes(
        y = cumulative_value,
        colour = factor(season, levels = legend_keys),
        linetype = factor(season, levels = legend_keys)
      ),
      linewidth = 0.6
    ) +
    
    geom_point(
      data = data.frame(
        x = NA,
        y = NA,
        key = factor(legend_keys, levels = legend_keys)
      ),
      aes(x = x, y = y, colour = key, fill = key, shape = key),
      inherit.aes = FALSE,
      size = 3,
      stroke = 0,
      show.legend = TRUE
    ) +
  
    # -------------------------
  # SCALES (ALIGNED PERFECTLY)
  # -------------------------
  scale_colour_manual(
    name = NULL,
    values = setNames(palette[legend_keys], legend_keys),
    limits = legend_keys
  ) +
    
    scale_fill_manual(
      name = NULL,
      values = setNames(palette[legend_keys], legend_keys),
      limits = legend_keys
    ) +
    
    scale_linetype_manual(
      name = NULL,
      values = setNames(override_linetype, legend_keys),
      limits = legend_keys
    ) +
    
    
    scale_shape_manual(
      values = setNames(
        ifelse(legend_types == "range", 15, NA),
        legend_keys
      ),
      limits = legend_keys,
      name = NULL
    ) +
  
    scale_x_continuous(
      limits = c(1, 12),
      breaks = 1:12,
      labels = month_levels,
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      limits = c(0, upper_limit),
      breaks = axis_line$breaks,
      labels = line_labeller,
      expand = c(0, 0)
    ) +
    
    # -------------------------
  # LEGEND (NO ERRORS NOW)
  # -------------------------
  guides(
    colour = guide_legend(override.aes = list(
      linetype = override_linetype
    )),
    fill = "none",
    shape = guide_legend(override.aes = list(
      linetype = 0
    )),
    linetype = "none"
  ) +
    
    labs(
      y = y_lab,
      x = NULL
    ) +
    
    theme_sopi(
      family = family,
      base_size = 10,
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
}
