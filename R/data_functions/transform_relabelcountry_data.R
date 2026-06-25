transform_relabelcountry_data <- function(
    data,
    country_var = "Country",
    time_var = NULL
    ) {
  
  country_var <- as.character(country_var)
  
  time_var <- if (is.null(time_var)) NULL else as.character(time_var)
  
  eu_countries <- getPwrBI(
    dataset_id = "36a78684-827e-4296-8983-1e78343fe6f0",
    mpi_tenant_id = get_app_token("mpi_tenant_id"),
    local_r_code_app_id = get_app_token("local_r_code_app_id"),
    dax_query = "
      EVALUATE
      SUMMARIZECOLUMNS(
          'Country'[Country],
          'Economic Region'[Economic Region],
          FILTER(
              'Economic Region',
              'Economic Region'[Economic Region] in {\"European Union\"}
          )
      )
    "
    )$Country

  # Identify numeric columns to aggregate
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  
  character_cols <- names(data)[vapply(data, is.character, logical(1))]
  
  if (!is.null(time_var)) {
    numeric_cols <- setdiff(numeric_cols, time_var)
  }
  
  # Grouping variables
  grouping_vars <- c(
    if (!is.null(time_var)) time_var,
    country_var,
    setdiff(character_cols, country_var)
  )

  # Aggregate
  dt <- data %>%
    mutate(
      !!country_var := case_when(
        .data[[country_var]] %in% eu_countries ~ "EU",
        .data[[country_var]] == "United Kingdom" ~ "UK",
        .data[[country_var]] == "Papua New Guinea" ~ "PNG",
        .data[[country_var]] == "United Arab Emirates" ~ "UAE", 
        TRUE ~ .data[[country_var]]
      )
    ) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      across(
        all_of(numeric_cols),
        \(x) sum(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  return(dt)
}
