transform_contribution_data <- function(
    data,
    group,
    time_var,
    fill_order = c("Volumes", "Prices")
) {
  group <- as.character(group)
  time_var <- as.character(time_var)

  dt <- data %>%
    arrange(.data[[group]], .data[[time_var]]) %>%
    group_by(.data[[group]]) %>%
    mutate(
      price = revenue / quantity,
      revenue_lag = lag(revenue),
      quantity_lag = lag(quantity),
      price_lag = lag(price),
      d_revenue = revenue - revenue_lag,
      d_price = price - price_lag,
      d_quantity = quantity - quantity_lag,
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
    select(
      category = all_of(group),
      total_pp,
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
        price_pp = "Prices",
        quantity_pp = "Volumes"
      )
    ) %>%
    mutate(
      driver = factor(driver, levels = fill_order),
      category = fct_reorder(category, total_pp, .desc = TRUE),
      category = fct_relevel(category, "All other", after = Inf),
      category = fct_rev(category)
    )
}
