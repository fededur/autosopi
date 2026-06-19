milk_production_plot <- function(
    data,
    filter_year_month = NULL,
    y_lab = 'Milksolids production (million kgMS)'
) {
  
  milk_prod_data <- data
  
  month_order <- c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                   "Dec", "Jan", "Feb", "Mar", "Apr", "May")
  
  if (!is.null(filter_year_month)) {
    filter_parts <- strsplit(filter_year_month, " ")[[1]]
    filter_year <- as.numeric(filter_parts[1])
    filter_month <- filter_parts[2]
    
    actual_check <- milk_prod_data %>%
      dplyr::filter(season_end_year == filter_year, month == filter_month) %>%
      dplyr::summarise(has_actual = any(!is.na(ms_million_actual))) %>%
      dplyr::pull(has_actual)
    
    if (!actual_check) {
      stop(paste0("No actual data found for cutoff date '", filter_year_month, "'."))
    }
    
    milk_prod_data <- milk_prod_data %>%
      dplyr::mutate(
        month_index = match(month, month_order),
        cutoff_index = match(filter_month, month_order),
        data_type = dplyr::case_when(
          season_end_year < filter_year ~ "actual",
          season_end_year == filter_year & month_index <= cutoff_index ~ "actual",
          season_end_year %in% c(filter_year, filter_year - 1) ~ "forecast",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(data_type))
  } else {
    milk_prod_data <- milk_prod_data %>%
      dplyr::mutate(data_type = dplyr::if_else(!is.na(ms_million_actual), "actual", "forecast"))
  }
  
  milk_prod_data <- milk_prod_data %>%
    dplyr::group_by(season_end_year, data_type) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      fill_label = dplyr::case_when(
        data_type == "actual" & n < 12 ~ paste0(season, " season to date"),
        data_type == "forecast" & n <= 12 ~ paste0(season, " season forecast"),
        TRUE ~ paste0(season, " season")
      ),
      ms_million_value = dplyr::if_else(!is.na(ms_million_actual), ms_million_actual, ms_million_forecast)
    )
  
  reference_year <- if (!is.null(filter_year_month)) {
    as.numeric(strsplit(filter_year_month, " ")[[1]][1])
  } else {
    max(milk_prod_data$season_end_year)
  }
  
  complete_seasons <- milk_prod_data %>%
    dplyr::filter(data_type == "actual") %>%
    dplyr::group_by(season_end_year) %>%
    dplyr::summarise(n_months = dplyr::n_distinct(month[!is.na(ms_million_actual)]), .groups = "drop") %>%
    dplyr::filter(n_months == 12, season_end_year <= reference_year) %>%
    dplyr::arrange(desc(season_end_year)) %>%
    dplyr::slice(1:5) %>%
    dplyr::pull(season_end_year)
  
  milk_prod_average <- milk_prod_data %>%
    dplyr::filter(season_end_year %in% complete_seasons, data_type == "actual") %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      ms_million_average = mean(ms_million_actual, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      month = factor(month, levels = month_order),
      fill_label = "Five-year average"
    )
  
  milk_prod_2yrs <- milk_prod_data %>%
    dplyr::filter(season_end_year %in% c(reference_year, reference_year - 1)) %>%
    dplyr::mutate(month = factor(month, levels = month_order))
  
  max_val <- max(
    c(milk_prod_2yrs$ms_million_value,
      milk_prod_average$ms_million_average),
    na.rm = TRUE
  )
  
  axis <- get_nice_breaks(max_val)
  
  fill_levels <- c(
    "Five-year average",
    milk_prod_2yrs %>%
      dplyr::arrange(season_end_year, data_type) %>%
      dplyr::pull(fill_label) %>%
      unique()
  )
  
  legend_items_line <- tibble::tibble(
    legend_item = factor(rep("Five-year average", 2), levels = fill_levels),
    x = levels(milk_prod_2yrs$month)[1:2],
    y = c(0, 0)
  )
  
  ggplot() +
    
    geom_col(
      data = milk_prod_2yrs,
      aes(
        x = month,
        y = ms_million_value,
        fill = factor(fill_label, levels = fill_levels)
      ),
      position = 'dodge',
      width = 0.5
    ) +
    
    # geom_line(
    #   data = milk_prod_average,
    #   aes(x = month, y = ms_million_average, group = 1),
    #   linetype = "dotted", 
    #   lty="11",
    #   colour = "#6d6e70",
    #   linewidth = 0.8
    # ) +
    
    geom_line(
      data = milk_prod_average,
      aes(x = month, y = ms_million_average, group = 1),
      linetype = "dotted",
      colour = "#6d6e70",
      linewidth = 0.8
    ) +
  
    
    geom_line(
      data = legend_items_line,
      aes(x = x, y = y, colour = legend_item, group = 1),
      inherit.aes = FALSE,
      linetype = "dotted",
      linewidth = 0.8,
      show.legend = TRUE
    ) +
    
    scale_colour_manual(
      values = c("Five-year average" = "#6d6e70"),
      breaks = "Five-year average",
      name = NULL
    ) +
    
    scale_fill_manual(
      values = build_color_palette(
        fill_levels,
        c("#dbdcde", "#ffeb95", "#fdb813", "#bf8828")
      )
    ) +
    
    scale_y_continuous(
      labels = scales::label_comma(scale = 1 / 1e6),
      breaks = axis$breaks,
      limits = c(0, axis$rounded_max),
      expand = c(0, NA)
    ) +
    
    scale_x_discrete(expand = c(0, 0)) +
    
    labs(x = NULL, y = y_lab, fill = NULL) +
    
    theme_sopi(
      family = "DIN",
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text.y = element_text(margin = margin(r = 6)),
      axis.text.x = element_text(margin = margin(t = 4))
    ) +
    
    theme(
      legend.title = element_blank(),
      legend.key.width  = unit(4, "mm"),
      legend.key.height = unit(4, "mm"),
      legend.position = "right",
      legend.justification = "top",
      legend.box.just = "left",
      legend.box = "vertical",
      plot.margin = margin(5, 5, 5, 5),
      legend.margin = margin(t = 0, b = 0, l = 0 , r = 0),
      legend.box.margin = margin(t = 0, b = 0, l = 0 , r = 0),
      legend.spacing.y = unit(0, "mm"),
      legend.box.spacing = unit(0, "mm")
    ) +
    
    guides(
      fill = guide_legend(
        order = 1,
        
        title = NULL,
        keyheight = unit(4, "mm"),
        keywidth  = unit(4, "mm"),

        override.aes = list(
          shape = 22,
          size = 4,
          colour = NA
        )
      ),
      colour = guide_legend(
        order = 1,
        title = NULL,
        keyheight = unit(4, "mm"),
        keywidth  = unit(4, "mm"),
        override.aes = list(
          linetype = "dotted",
          linewidth = 0.8,
          shape = NA
        )
      )
    )
}
