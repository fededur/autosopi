required_packages <- c(
  "dplyr",
  "ggplot2",
  "readxl",
  "rlang",
  "scales",
  "svglite",
  "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running the chart framework.",
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))
