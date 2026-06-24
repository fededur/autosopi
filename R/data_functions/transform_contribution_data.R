transform_contribution_data <- function(
    data,
    group,
    time_var,
    revenue = NULL,
    quantity = NULL,
    fill_order = c("Volume contribution", "Price contribution")
) {
  group <- as.character(group)
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

  dt <- data %>%
    arrange(.data[[group]], .data[[time_var]]) %>%
    group_by(.data[[group]]) %>%
    mutate(
      price = .data[[revenue]] / .data[[quantity]],
      revenue_lag = lag(.data[[revenue]]),
      quantity_lag = lag(.data[[quantity]]),
      price_lag = lag(price),
      d_revenue = .data[[revenue]] - revenue_lag,
      d_price = price - price_lag,
      d_quantity = .data[[quantity]] - quantity_lag,
      price_effect = d_price * quantity_lag,
      quantity_effect = d_quantity * price_lag,
      interaction = d_price * d_quantity,
      price_contribution = price_effect + 0.5 * interaction,
      quantity_contribution = quantity_effect + 0.5 * interaction
    ) %>%
    ungroup()

  total_revenue_lag <- dt %>%
    filter(.data[[time_var]] == max(.data[[time_var]]), !is.na(revenue_lag)) %>%
    pull(revenue_lag) %>%
    sum(., na.rm = TRUE)

  dt %>%
    mutate(
      price_pp = price_contribution / total_revenue_lag,
      quantity_pp = quantity_contribution / total_revenue_lag,
      total_pp = price_pp + quantity_pp
    ) %>%
    filter(
      .data[[time_var]] == max(.data[[time_var]]),
      !is.na(revenue_lag)
    ) %>%
    transmute(
      category = .data[[group]],
      net_contribution = total_pp,
      price_pp,
      quantity_pp
    ) %>%
    pivot_longer(
      cols = c(price_pp, quantity_pp),
      names_to = "driver",
      values_to = "contribution"
    ) %>%
    mutate(
      driver = recode(
        driver,
        price_pp = "Price contribution",
        quantity_pp = "Volume contribution"
      )
    ) %>%
    mutate(
      driver = factor(driver, levels = fill_order),
      category = fct_reorder(category, net_contribution, .desc = TRUE),
      category = if ("All other" %in% levels(category)) {
        fct_relevel(category, "All other", after = Inf)
      } else {
        category
      },
      category = fct_rev(category)
    ) %>%
    select(category, driver, contribution, net_contribution)
}
