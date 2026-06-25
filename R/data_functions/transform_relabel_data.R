transform_relabel_data <- function(
    data,
    category,
    sector = NULL,
    metadata_level = c("forecast_group", "sector"),
    project_root = NULL,
    metadata_file = "sopi_metadata.xlsx",
    metadata_sheet = "metadata",
    key_col = NULL,
    label_col = NULL,
    group_vars = NULL,
    time_var = NULL,
    revenue = NULL,
    quantity = NULL,
    price = NULL,
    keep_unmatched = TRUE
) {
  category <- as.character(category)
  metadata_level <- match.arg(metadata_level)

  if (!category %in% names(data)) {
    stop("Category column not found in data: ", category, call. = FALSE)
  }

  metadata_path <- if (!is.null(project_root) && nzchar(trimws(as.character(project_root)))) {
    file.path(project_root, "metadata", metadata_file)
  } else {
    file.path("metadata", metadata_file)
  }

  if (!file.exists(metadata_path)) {
    stop("Metadata workbook not found: ", metadata_path, call. = FALSE)
  }

  if (!metadata_sheet %in% readxl::excel_sheets(metadata_path)) {
    stop("Metadata sheet not found in workbook: ", metadata_sheet, call. = FALSE)
  }

  metadata <- readxl::read_xlsx(metadata_path, sheet = metadata_sheet) |>
    clean_metadata_names()

  if (is.null(key_col)) {
    key_col <- if (identical(metadata_level, "sector")) "sector_key" else "forecast_group_key"
  }

  if (is.null(label_col)) {
    label_col <- if (identical(metadata_level, "sector")) "sector_label" else "forecast_group_label"
  }

  key_col <- as.character(key_col)
  label_col <- as.character(label_col)

  missing_metadata_cols <- setdiff(c(key_col, label_col), names(metadata))
  if (length(missing_metadata_cols) > 0) {
    stop(
      "Metadata workbook is missing required columns: ",
      paste(missing_metadata_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(sector) && "sector_key" %in% names(metadata)) {
    metadata <- metadata |>
      dplyr::filter(.data$sector_key %in% sector)
  }

  label_map <- metadata |>
    dplyr::filter(
      !is.na(.data[[key_col]]),
      nzchar(.data[[key_col]]),
      !is.na(.data[[label_col]]),
      nzchar(.data[[label_col]])
    ) |>
    dplyr::distinct(.data[[key_col]], .keep_all = TRUE) |>
    dplyr::transmute(key = as.character(.data[[key_col]]), label = as.character(.data[[label_col]]))

  label_lookup <- stats::setNames(label_map$label, label_map$key)

  relabelled <- unname(label_lookup[as.character(data[[category]])])
  if (isTRUE(keep_unmatched)) {
    relabelled[is.na(relabelled) | !nzchar(relabelled)] <- as.character(data[[category]])[
      is.na(relabelled) | !nzchar(relabelled)
    ]
  }

  data[[category]] <- relabelled

  detect_first_column <- function(fields, patterns) {
    matches <- fields[grepl(patterns, fields, ignore.case = TRUE)]
    matches <- matches[!is.na(matches) & nzchar(matches)]
    if (length(matches) == 0) NULL else matches[[1]]
  }

  fields <- names(data)

  if (is.null(revenue) || !nzchar(trimws(as.character(revenue)))) {
    revenue <- detect_first_column(fields, "^(export[ _.-]*)?(revenue|value)$|export[ _.-]*(revenue|value)")
  }

  if (is.null(quantity) || !nzchar(trimws(as.character(quantity)))) {
    quantity <- detect_first_column(fields, "^(export[ _.-]*)?(quantity|volume)$|export[ _.-]*(quantity|volume)")
  }

  if (is.null(price) || !nzchar(trimws(as.character(price)))) {
    price <- fields[grepl("price", fields, ignore.case = TRUE)]
  }

  time_var <- if (is.null(time_var)) character() else as.character(time_var)
  group_vars <- if (is.null(group_vars)) character() else as.character(group_vars)

  character_group_vars <- names(data)[
    vapply(data, function(x) is.character(x) || is.factor(x) || inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))
  ]
  time_like_numeric_vars <- names(data)[
    vapply(data, is.numeric, logical(1)) &
      grepl("year|month|quarter|period|season|date", names(data), ignore.case = TRUE)
  ]

  grouping_vars <- unique(c(
    time_var,
    group_vars,
    category,
    setdiff(character_group_vars, category),
    time_like_numeric_vars
  ))
  grouping_vars <- grouping_vars[grouping_vars %in% names(data)]

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  price_cols <- as.character(price)
  price_cols <- price_cols[price_cols %in% names(data)]

  numeric_sum_cols <- setdiff(numeric_cols, unique(c(grouping_vars, price_cols)))

  out <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numeric_sum_cols),
        \(x) sum(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  if (
    length(price_cols) > 0 &&
      !is.null(revenue) &&
      !is.null(quantity) &&
      revenue %in% names(out) &&
      quantity %in% names(out)
  ) {
    for (price_col in price_cols) {
      out[[price_col]] <- dplyr::if_else(
        is.na(out[[quantity]]) | out[[quantity]] == 0,
        NA_real_,
        out[[revenue]] / out[[quantity]]
      )
    }
  }

  out
}
