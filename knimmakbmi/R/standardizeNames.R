#' Standardize variable names in a tibble
#'
#' This function uses `dplyr::rename_with` and `janitor::make_clean_names` to clean and standardize the
#' column names of a tibble. It then converts these names to "small_camel" case using `snakecase::to_small_camel_case`.
#'
#' @param data A tibble whose column names are to be standardized.
#' @return A tibble with standardized column names in "small_camel" case.
#' @importFrom dplyr rename_with
#' @importFrom janitor make_clean_names
#' @examples
#' library(tibble)
#' data = tibble(`some data` = 1:5, `!!more data!!` = 5:1)
#' standardized_data = standardizeNames(data)
#' @export
standardizeNames = function(data) {
  if (!inherits(data, "tbl_df")) {
    stop("input must be a tibble")
  }

  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("janitor", quietly = TRUE) ||
      !requireNamespace("snakecase", quietly = TRUE)) {
    stop("Please ensure that dplyr, janitor, and snakecase packages are installed.")
  }

  dplyr::rename_with(data, .fn = ~ snakecase::to_any_case(janitor::make_clean_names(.), case = "small_camel"))
}
