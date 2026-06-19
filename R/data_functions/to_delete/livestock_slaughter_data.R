slaughter_numbers_data <- function(animal_category = "Lambs") {
  
  month_levels <- c(
    "Jul","Aug","Sep","Oct","Nov","Dec",
    "Jan","Feb","Mar","Apr","May","Jun"
  )
  
  category_string <- paste(animal_category, collapse = "\",\"")
  
  lss_data <- getPwrBI(
    dataset_id = "50ec5c54-c039-44f9-a6e3-f0cecb9149a2",
    # mpi_tenant_id = get_app_token("mpi_tenant_id"),
    # local_r_code_app_id = get_app_token("local_r_code_app_id"),
    mpi_tenant_id = "c30d47c4-6369-4cf2-9dd6-79a0e0aa416d",
    local_r_code_app_id = "b9d05957-46e4-4886-a860-9d057ab55f89",
    
    columns_list = list(
      "Animal Category" = "Animal Classification",
      "Financial Year" = "Date",
      "Financial Month" = "Date"
    ),
    measures_list = list(
      measure = "'Numbers and Weights'[Total Slaughter Numbers (including condemned numbers)]"
    ),
    filters_list = list(
      "Animal Category" = paste0(
        "'Animal Classification'[Animal Category] in {\"",
        category_string,
        "\"}"
      )
    )
  ) %>%
    select(-`Animal Category`)
  
  slaughter_data <- lss_data %>%
    rename(season = `Financial Year`,
           month = `Financial Month`,
           value = measure) %>%
    mutate(
      season_end_year = as.integer(str_extract(season, "\\d{4}")) + 1,
      month = factor(month, levels = month_levels, ordered = TRUE),
      month_index = as.integer(month)
    ) %>%
    group_by(season) %>%
    mutate(n_months = n()) %>%
    ungroup() %>%
    arrange(season_end_year,month_index) %>%
    group_by(season) %>%
    mutate(cumulative_value = cumsum(value)) %>%
    ungroup()
  
  return(slaughter_data)
}
