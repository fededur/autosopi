transform_yoychange_data <- function(
    data,
    group,
    time_var,
    revenue = NULL,
    quantity = NULL,
    latest = TRUE,
    fill_labels = c(
      revenue_yoy_change_pct = "Revenue",
      quantity_yoy_change_pct = "Quantity",
      price_yoy_change_pct = "Price"
    ),
    fill_order = c("Revenue","Quantity","Price")
) {
  
  time_var <- as.character(time_var)
  group <- as.character(group)
  
  detect_column <- function(fields, patterns, label) {
    matches <- fields[grepl(patterns, fields, ignore.case = TRUE)]
    if (length(matches) == 0) {
      stop("Could not identify a ", label, " column. Please include a matching column or pass the argument explicitly.", call. = FALSE)
    }
    matches[[1]]
  }
  
  fields <- names(data)
  if (is.null(revenue) || !nzchar(trimws(as.character(revenue)))) {
    revenue <- detect_column(fields, "^(export[ _.-]*)?(revenue|value)$|export[ _.-]*(revenue|value)", "revenue")
  }
  if (is.null(quantity) || !nzchar(trimws(as.character(quantity)))) {
    quantity <- detect_column(fields, "^(export[ _.-]*)?(quantity|volume)$|export[ _.-]*(quantity|volume)", "quantity or volume")
  }
  

  revenue <- as.character(revenue)
  
  quantity <- as.character(quantity)
  
  keep_cols <- unique(c(
    group,
    time_var,
    "revenue_yoy_change_pct",
    "quantity_yoy_change_pct",
    "price_yoy_change_pct"
  ))
   
  missing_group <- setdiff(group, names(data))
  if (length(missing_group) > 0) {
    stop("Group column(s) not found in data: ", paste(missing_group, collapse = ", "), call. = FALSE)
  }

  if (!time_var %in% names(data)) {
    stop("Time column not found in data: ", time_var, call. = FALSE)
  }

  n_periods_total <- dplyr::n_distinct(data[[time_var]], na.rm = TRUE)
  if (n_periods_total < 2) {
    stop(
      "transform_yoychange_data() needs at least two time periods to calculate year-on-year change. ",
      "The selected time column '", time_var, "' has only ", n_periods_total, " period",
      ifelse(n_periods_total == 1, "", "s"),
      ". Check filters or load more than one year/period.",
      call. = FALSE
    )
  }

  group_counts <- data %>%
    group_by(across(all_of(group))) %>%
    summarise(n_periods = dplyr::n_distinct(.data[[time_var]], na.rm = TRUE), .groups = "drop") %>%
    mutate(.group_label = do.call(paste, c(across(all_of(group)), sep = " / ")))

  single_period_groups <- group_counts %>%
    filter(.data$n_periods < 2)

  if (nrow(single_period_groups) > 0) {
    stop(
      "transform_yoychange_data() needs at least two time periods within each group. ",
      "These groups have fewer than two periods after filters/previous transforms: ",
      paste(
        paste(single_period_groups$.group_label, single_period_groups$n_periods, sep = "="),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  dt <- data %>%
    arrange(
      across(all_of(group)),
      .data[[time_var]]
    ) %>%
    group_by(across(all_of(group))) %>%
    mutate(
      price = .data[[revenue]] / .data[[quantity]],
      
      revenue_yoy_change_pct =
        .data[[revenue]] / lag(.data[[revenue]]) - 1,
      
      quantity_yoy_change_pct =
        .data[[quantity]] / lag(.data[[quantity]]) - 1,
      
      price_yoy_change_pct =
        price / lag(price) - 1
    ) %>%
    ungroup() %>%
    select(all_of(keep_cols)) %>%
    pivot_longer(
      cols = ends_with("_yoy_change_pct"),
      names_to = "measure",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      measure = recode(measure, !!!fill_labels),
      measure = factor(measure, levels = fill_order)
    )

  if (nrow(dt) == 0) {
    stop(
      "transform_yoychange_data() returned no rows because no non-missing year-on-year changes could be calculated. ",
      "Check that each group has at least two time periods after previous transform steps. ",
      "Period counts: ",
      paste(
        paste(group_counts$.group_label, group_counts$n_periods, sep = "="),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (
    isTRUE(latest) &&
    !is.null(time_var) &&
    time_var %in% names(dt)
  ) {
    
    dt <- dt %>%
      group_by(across(all_of(group))) %>%
      filter(
        .data[[time_var]] ==
          max(.data[[time_var]], na.rm = TRUE)
      ) %>%
      ungroup()
    
  }
  
  return(dt)
}
