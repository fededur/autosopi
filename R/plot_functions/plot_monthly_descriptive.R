sopi_monthly_descriptive_legend_keys <- function(
    data,
    date_var = "date",
    y = "value",
    month_year = NULL) {
  y_name <- if (is.character(y)) y else rlang::as_name(rlang::enquo(y))

  data <- data %>%
    dplyr::mutate(
      date = parse_flexible_date(.data[[date_var]]),
      month = lubridate::month(.data$date),
      month_lab = lubridate::month(.data$date, abbr = TRUE, label = TRUE),
      month_lab = sub("Sept", "Sep", .data$month_lab),
      year = lubridate::year(.data$date)
    ) %>%
    dplyr::mutate(
      season_end_year = ifelse(.data$month >= 7, .data$year + 1, .data$year),
      season = paste0(
        .data$season_end_year - 1,
        "/",
        substr(.data$season_end_year, 3, 4)
      )
    ) %>%
    dplyr::group_by(.data$season, .data$season_end_year, .data$month_lab) %>%
    dplyr::summarise(
      value = sum(.data[[y_name]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$season) %>%
    dplyr::mutate(n_months = dplyr::n()) %>%
    dplyr::ungroup()

  if (!is.null(month_year)) {
    parsed_date <- lubridate::parse_date_time(month_year, orders = "b Y")
    input_month <- lubridate::month(parsed_date)
    input_year <- lubridate::year(parsed_date)

    fy_end_year <- ifelse(input_month >= 7, input_year + 1, input_year)

    data <- data %>%
      dplyr::filter(.data$season_end_year <= fy_end_year)
  }

  current_end <- max(data$season_end_year, na.rm = TRUE)
  five_years <- (current_end - 5):(current_end - 1)
  two_years <- (current_end - 1):current_end

  complete_seasons <- data %>%
    dplyr::filter(.data$n_months == 12, .data$season_end_year %in% five_years)

  seasons_label <- paste0(
    min(five_years) - 1,
    "/",
    substr(min(five_years), 3, 4),
    "-",
    substr(max(five_years), 3, 4)
  )

  average_label <- paste0(seasons_label, " average")
  range_label <- paste0(seasons_label, " range")

  two_seasons <- data %>%
    dplyr::filter(.data$season_end_year %in% two_years)

  unique(c(
    average_label,
    range_label,
    two_seasons$season
  ))
}

plot_monthly_descriptive <- function(
    data,
    date_var = "date",
    y = "value",
    cumulative = FALSE,
    month_year = NULL,
    y_lab = "Total slaughter (head)",
    y_min_breaks = 4,
    y_max_breaks = 5,
    y_accuracy = 1,
    y_scale = 1,
    colour_palette = NULL,
    family = "DIN",
    base_size = 10.5
) {
  
  y_quo <- rlang::enquo(y)
  
  if (rlang::quo_is_missing(y_quo) && !is.character(y)) {
    stop("`y` must be supplied.")
  }
  
  # Convert to column name
  y_name <- if (is.character(y)) y else rlang::as_name(y_quo)

  # -------------------------
  # MONTHS
  # -------------------------
  month_levels <- c(
    "Jul","Aug","Sep","Oct","Nov","Dec",
    "Jan","Feb","Mar","Apr","May","Jun"
  )
  
  # -------------------------
  # PREPARE DATA
  # -------------------------
  data <- data %>%
    mutate(
      date = parse_flexible_date(.data[[date_var]]),
      month = lubridate::month(date),
      month_lab = lubridate::month(date, abbr = TRUE, label = TRUE),
      month_lab = sub("Sept", "Sep", month_lab),
      year  = lubridate::year(date)
    )  %>%
    mutate(
      season_end_year = ifelse(month >= 7, year + 1, year),
      season = paste0(
        (season_end_year - 1),"/", substr(season_end_year, 3, 4)
      )
    ) %>%
    mutate(
      month = factor(month_lab, levels = month_levels, ordered = TRUE),
      month_index = as.integer(month)
    ) %>%
    group_by(season, season_end_year, month, month_lab, month_index) %>%
    summarise(
      value = sum(.data[[y_name]], na.rm = TRUE),
      .groups = "drop"
    )%>%
    group_by(season) %>%
    mutate(n_months = n()) %>%
    ungroup() %>%
    arrange(season_end_year,month_index) 
  
  if (cumulative) {
    
    data <- data %>%
    group_by(season) %>%
    mutate(value = cumsum(value)) %>%
    ungroup()
    
  }
  
  # Pull values for calculations
  y_vals <- dplyr::pull(data, .data[[y_name]])
  
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
  
  complete_seasons <- data %>%
    filter(n_months == 12, season_end_year %in% five_years)
  
  seasons_label <- paste0(
    min(five_years) - 1,
    "/",
    substr(min(five_years), 3, 4),
    "-",
    substr(max(five_years), 3, 4)
  )
  
  average_label <- paste0(
    seasons_label,
    " average"
  )
  
  range_label <- paste0(
    seasons_label,
    " range")

  # -------------------------
  # 5-YEAR STATS
  # -------------------------
  five_seasons <- complete_seasons %>%
    group_by(month_index) %>%
    summarise(
      average_value = mean(.data[[y_name]], na.rm = TRUE),
      min_value     = min(.data[[y_name]], na.rm = TRUE),
      max_value     = max(.data[[y_name]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      average_label = average_label,
      range_label = range_label
    )
  
  two_seasons <- data %>%
    filter(season_end_year %in% two_years) %>%
    arrange(season_end_year, month_index)
  
  # -------------------------
  # LEGEND KEYS
  # -------------------------
  legend_keys <- unique(c(
    five_seasons$average_label,
    five_seasons$range_label,
    two_seasons$season
  ))
  
  # -------------------------
  # AXIS
  # -------------------------
  axis_line <- get_nice_breaks(
    max(y_vals, na.rm = TRUE),
    min_breaks = y_min_breaks,
    max_breaks = y_max_breaks
  )
  
  if (is.null(y_accuracy)) {
    y_accuracy <- axis_line$accuracy
  }
  
  upper_limit <- axis_line$rounded_max
  
  line_labeller <- label_number(
    accuracy = y_accuracy,
    scale = y_scale,
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
  } else if (!is.null(names(colour_palette)) && any(nzchar(names(colour_palette)))) {
    default_palette <- stats::setNames(
      c("#3D3D3D", "#dbdcde", scales::hue_pal()(max(length(legend_keys) - 2, 0))),
      legend_keys
    )
    named_palette <- colour_palette[!is.na(names(colour_palette)) & nzchar(names(colour_palette))]
    matched_names <- intersect(names(named_palette), legend_keys)
    default_palette[matched_names] <- named_palette[matched_names]
    palette <- default_palette
  } else {
    
    n_pal <- length(legend_keys) - 2
    
    palette <- build_color_palette(
      legend_keys,
      c("#3D3D3D", "#dbdcde",
        colour_palette[1:n_pal])
    )
  }
  
  # -------------------------
  # LEGEND TYPES
  # -------------------------
  legend_types <- dplyr::case_when(
    legend_keys %in% five_seasons$average_label ~ "average",
    legend_keys %in% five_seasons$range_label   ~ "range",
    TRUE                                        ~ "season"
  )
  
  override_linetype <- dplyr::case_when(
    legend_types == "average" ~ 3,
    legend_types == "season"  ~ 1,
    legend_types == "range"   ~ 0
  )
  
  # -------------------------
  # PLOT
  # -------------------------
  ggplot(five_seasons, aes(x = month_index)) +
    
    geom_ribbon(
      aes(
        ymin = min_value,
        ymax = max_value,
        fill = factor(range_label, levels = legend_keys)
      ),
      alpha = 0.25
    ) +
    
    geom_line(
      aes(
        y = average_value,
        colour = factor(average_label, levels = legend_keys),
        linetype = factor(average_label, levels = legend_keys)
      ),
      linewidth = 0.6
    ) +
    
    geom_line(
      data = two_seasons,
      aes(
        y = .data[[y_name]],
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
    
    guides(
      colour = guide_legend(
        override.aes = list(linetype = override_linetype)
      ),
      fill = "none",
      shape = guide_legend(
        override.aes = list(linetype = 0)
      ),
      linetype = "none"
    ) +
    
    labs(
      y = y_lab,
      x = NULL
    ) +
    
    theme_sopi(
      family = family,
      base_size = base_size,
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
