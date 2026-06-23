#' Calculate period
#'
#' Calculate period using lubridate function
#'
#' @param x a date.
#' @param f a string indicating a lubridate function.
#' @import dplyr lubridate tsibble
#' @importFrom magrittr %>%
#'
#' @return a period based on lubridate function
#' @export
timeFun <- function(x, f = "yearmonth") {
  
  period_functions <- list(
    "yearmonth" = tsibble::yearmonth,
    "yearquarter" = tsibble::yearquarter,
    "junfinancialyear" = function(date) { fiscal_year(tsibble::yearquarter(date, fiscal_start = 7)) },
    "marfinancialyear" = function(date) { fiscal_year(tsibble::yearquarter(date, fiscal_start = 4)) },
    "sepfinancialyear" = function(date) { fiscal_year(tsibble::yearquarter(date, fiscal_start = 10)) },
    "calendaryear" = lubridate::year
  )
  
  if (!f %in% names(period_functions)) {
    stop("Invalid period argument supplied to 'redefPeriod'\n
           choose one of: 'yearmonth', 'yearquarter', 'junfinancialyear', 'marfinancialyear', 'sepfinancialyear' or 'calendaryear'")
  }
  
  pFun <- period_functions[[f]]
  
  res <- pFun(x)
  
  return(res)
}

get_omt_data <- function(
    dataset_id = "36a78684-827e-4296-8983-1e78343fe6f0",
    columns_list = NULL,
    filters_list = NULL,
    measures_list =
      list(
        "Export Free On Board ($NZ)" = "'Export Measures'[Export Free On Board ($NZ)]",
        "Export Quantity" = "'Export Measures'[Export Quantity]",
        "SOPI Export Price $NZ/KG" = TRUE
        ),
    f = "yearmonth"
    ) {
  
  period_key <- c(period = names(which(unlist(columns_list, use.names=TRUE) == "Time")))
  
  group_keys <- names(which(unlist(columns_list, use.names=TRUE) != "Time"))
  
  measure_keys <- names(measures_list)

  price_var <- measure_keys[which(stringr::str_detect(measure_keys, "Price"))]
  
  quantity_var <- measure_keys[which(stringr::str_detect(measure_keys, "Quantity"))]
  
  revenue_var <- measure_keys[which(stringr::str_detect(measure_keys, "Free On Board"))]
  
  rw <- getPwrBI(
    mpi_tenant_id = get_app_token("mpi_tenant_id"),
    local_r_code_app_id = get_app_token("local_r_code_app_id"),
    dataset_id = dataset_id,
    columns_list = columns_list,
    filters_list = filters_list,
    measures_list = measures_list
  )
  
  # ts <- rw %>%
  #   rename(any_of(period_key)) %>%
  #   mutate(period = tsibble::yearmonth(period)) %>%
  # 
  #   mutate(., period = timeFun(period, f = f)) %>%
  #   group_by(across(all_of("period")),across(all_of(group_keys))) %>%
  #   summarise(.,
  #             !!revenue_var := sum(!!sym(revenue_var), na.rm=TRUE),
  #             !!quantity_var := sum(!!sym(quantity_var), na.rm=TRUE),
  #             !!price_var := if_else(!!sym(quantity_var) == 0, NA_real_, sum(!!sym(revenue_var), na.rm=TRUE)/!!sym(quantity_var)),
  #             .groups = "drop"
  #             )
  # 
  # return(ts)
  rw
}
