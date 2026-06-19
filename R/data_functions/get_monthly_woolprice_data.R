get_monthly_wool_data <- function(wtg_category = c("Less than 24.5 microns","24.5 to 31.4 microns","More than 31.4 microns")) {
  
  category_string <- paste(wtg_category, collapse = "\",\"")
  
  wtg_data <- getPwrBI(
    dataset_id = "36a78684-827e-4296-8983-1e78343fe6f0",
    
    mpi_tenant_id = get_app_token("mpi_tenant_id"),
    
    local_r_code_app_id = get_app_token("local_r_code_app_id"),
    
    columns_list = list(
      "World Trade Group" = "NZHSC",
      "Month Start Date" = "Time"
    ),
    
    measures_list = list(
      "value" =
        "VAR FOB = SUM('Export'[ExportFOB])
         VAR QNY = SUM('Export'[ExportQuantity])
         
         RETURN
          IF(
           ISBLANK(QNY) || QNY == 0,
           BLANK(),
           FOB/QNY)
      "
    ),
    
    filters_list = list(
      "World Trade Group" = paste0(
        "'NZHSC'[World Trade Group] in {\"",
        category_string,
        "\"}"
      )
    )
  ) %>%
    rename(
      date = `Month Start Date`,
      group= `World Trade Group`
    ) %>%
    arrange(date,group)
  
  return(wtg_data)
}
