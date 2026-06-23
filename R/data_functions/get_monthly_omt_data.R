get_monthly_omt_data <- function(
    sector = NULL,
    forecast_group = NULL
    ) {
  
  
  
  category_string <- paste(sector, collapse = "\",\"")
  
  
  # columns_list = list(
  #   "Animal Category" = "Animal Classification",
  #   "Levy Return Date" = "Date"
  # )
  # 
  data <- getPwrBI(
    dataset_id = "36a78684-827e-4296-8983-1e78343fe6f0",
    
    mpi_tenant_id = get_app_token("mpi_tenant_id"),
    
    local_r_code_app_id = get_app_token("local_r_code_app_id"),
    
    columns_list = list(
      "Primary Industry Sector" = "NZHSC",
      "SOPI Forecast Group" = "NZHSC",
      "Mar Year End Year" = "Time"
    ),
    
    measures_list = list(
      revenue = "'Export Measures'[Export Free On Board ($NZ)]"
    ),
    
    filters_list = list(
      "Primary Industry Sector" = paste0(
        "'NZHSC'[Primary Industry Sector] in {\"",
        category_string,
        "\"}"
      )
    )
  ) %>%
    rename(date = `Mar Year End Year`)
  
  return(data)
}
