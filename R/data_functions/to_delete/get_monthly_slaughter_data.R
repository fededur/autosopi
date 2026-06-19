get_monthly_slaughter_data <- function(animal_category = "Lambs") {
  
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
      "Levy Return Date" = "Date"
    ),
    measures_list = list(
      value = "'Numbers and Weights'[Total Slaughter Numbers (including condemned numbers)]"
    ),
    filters_list = list(
      "Animal Category" = paste0(
        "'Animal Classification'[Animal Category] in {\"",
        category_string,
        "\"}"
      )
    )
  ) %>%
    rename(date = `Levy Return Date`)%>%
    select(-`Animal Category`)

  return(lss_data)
}
