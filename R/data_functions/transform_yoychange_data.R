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
    fill_order = c("Price", "Quantity", "Revenue")
) {
  
  time_var <- as.character(time_var)
  
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
