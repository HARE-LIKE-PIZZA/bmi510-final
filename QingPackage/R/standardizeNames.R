#' Standardize Names of Data Frame Columns
#'
#' This function standardizes the names of a data frame or tibble by converting
#' them into small camel case format after replacing any non-alphanumeric characters with underscores.
#'
#' @param data A data frame or tibble.
#' @return A tibble with standardized column names.
#' @export
#' @examples
#' data <- tibble(
#'   "First Name" = c("John", "Jane"),
#'   "Last_Name" = c("Doe", "Smith"),
#'   "Age." = c(25, 30)
#' )
#' standardized_data <- standardizeNames(data)
#' print(standardized_data)
standardizeNames <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a tibble or data frame")
  }
  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }
  names(data) <- make.names(names(data))
  to_small_camel <- function(x) {
    x <- tolower(x)
    x <- gsub("\\.", "_", x)
    x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
    return(x)
  }

  names(data) <- to_small_camel(names(data))
  return(data)
}
