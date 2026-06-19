
plot_slaughter_trends <- function(
    data,
    y_line = "cummulative_value",
    month_year = NULL,
    y_lab = "Total slaughter (million head)",
    y_min_breaks = 4,
    y_max_breaks = 5,
    y_line_accuracy = NULL,
    colour_palette = NULL
) {
  
  # =========================
  # TIDY DATA
  # =========================
  
  month_levels <- c(
    "Jul","Aug","Sep","Oct","Nov","Dec",
    "Jan","Feb","Mar","Apr","May","Jun"
  )
  
  if (!is.null(month_year)) {
    
    parsed_date <- parse_date_time(month_year, orders = "b Y")
    input_month <- month(parsed_date)
    input_year <- year(parsed_date)
    
    # Determine financial year end
    fy_end_year <- ifelse(input_month >= 7, input_year + 1, input_year)
    
    # Calculate month_index: July = 1, ..., June = 12
    month_index <- (input_month - 7) %% 12 + 1
    
    data <- data %>%
      filter(season_end_year < fy_end_year |
               (season_end_year == fy_end_year & month_index <= !!month_index))
    
  }
  
  # Extract year and month
  current_end <- max(data$season_end_year, na.rm = TRUE)
  
  five_years <- (current_end - 5):(current_end - 1)
  
  slaughter_complete <- filter(data, n_months == 12)
  slaughter_complete <- filter(data, season_end_year %in% five_years)
  
  min_label <- filter(slaughter_complete, season_end_year == min(season_end_year)) %>%
    pull(season) %>%
    unique()
  
  min_label <- substr(min_label, 1, 4)
  
  # 5-Year Ribbon Stats
  slaughter_5yr <- slaughter_complete %>%
    filter(season_end_year %in% five_years) %>%
    group_by(month, month_index) %>%
    summarise(
      average_slaughter = mean(cummulative_value, na.rm = TRUE),
      min_slaughter     = min(cummulative_value,  na.rm = TRUE),
      max_slaughter     = max(cummulative_value,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      average_label = paste0(min(five_years),"-",substr(max(five_years), 3, 4), " average"),
      range_label = paste0(min(five_years),"-",substr(max(five_years), 3, 4), " range")
    ) %>%
    arrange(month_index)
  
  # Last 2 Seasons
  two_years <- (current_end - 1):current_end
  slaughter_2yr <- data %>%
    filter(season_end_year %in% two_years) %>%
    arrange(season_end_year, month_index)
  
  # Legend Setup
  group_vals <- c(
    unique(slaughter_5yr$average_label),
    unique(slaughter_5yr$range_label),
    unique(slaughter_2yr$season)
  )
  
  legend_points <- data.frame(
    month_index = 1,
    y = 0,
    legend_key = factor(group_vals, levels = group_vals)
  )
  
  # =========================
  # AXES
  # =========================
  
  axis_line <- get_nice_breaks(
    max(data[[y_line]], na.rm = TRUE),
    min_breaks = y_min_breaks,
    max_breaks = y_max_breaks
  )
  
  if (is.null(y_line_accuracy)) {
    y_line_accuracy <- axis_line$accuracy
  }
  
  upper_limit <- axis_line$rounded_max
  
  line_labeller <- scales::label_number(
    accuracy = y_line_accuracy,
    big.mark = ","
  ) 
  
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
    palette <-
      build_color_palette(
        group_vals, c("#3D3D3D",
                      "#dbdcde",
                      colour_palette[1:2])
        )
    
    align_palette(palette, group_vals)
  }

  # =========================
  # GEOMS
  # =========================
  
  ggplot(slaughter_5yr, aes(x = month_index)) +
    geom_ribbon(
      aes(ymin = min_slaughter, ymax = max_slaughter, fill = factor(range_label, levels = group_vals)),
      alpha = 0.25,
      colour = NA,
      show.legend = FALSE
    ) +
    
    geom_line(
      aes(
        y = average_slaughter,
        colour = factor(average_label, levels = group_vals)
        ),
      linewidth = 0.5,
      linetype = "dashed",
      show.legend = FALSE
    ) +
    
    geom_line(
      data = slaughter_2yr,
      aes(
        y = cummulative_value,
        colour = factor(season, levels = group_vals)
        ),
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    
    geom_point(
      data = legend_points,
      aes(x = month_index, y = y, colour = legend_key),
      inherit.aes = FALSE,
      shape = 16,
      size = 3,
      alpha = 0,
      show.legend = TRUE
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
    
    labs(
      y = y_lab,
      x = NULL
    ) +
    
    scale_colour_manual(
      name = NULL,
      values = palette, 
      breaks = group_vals,
      labels = group_vals 
    ) +
    
    scale_fill_manual(
      name = NULL,
      values = palette,
      breaks = group_vals,
      labels = group_vals,
      guide = "none"
    ) +
    
    theme_sopi(
      family = "DIN",
      base_size = 11,
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text.y = element_text(margin = margin(r = 6)),
      axis.text.x = element_text(margin = margin(t = 4)),
      legend.box.margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "mm"),
      legend.key = element_rect(fill = NA, colour = NA),
      legend.key.height = unit(16, "pt"),
      legend.spacing.x = unit(3, "pt"),
      legend.key.size = unit(4, "mm")
    ) +
    
    guides(
      colour = guide_legend(
        title = NULL,
        override.aes = list(shape = 16, size = 3, alpha = 1)
      )
    )
}
