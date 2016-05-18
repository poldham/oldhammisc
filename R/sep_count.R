#' Count Separators for Separate_Gather
#'
#' Count the number of separators in a column as the basis for the calculation of the number of columns to separate a column into using separate() in tidyr. Provides input to separate_gather
#' @param data A data frame
#' @param col column containing separators to count
#' @param sep separator
#' @export
#' @importFrom stringr str_count
#' @return numeric count of separators
#' @details Calculates the maximum number of separators in a column (typically ";"). When dividing values by a separator note that the final entry will not be marked by a separator. To address this the function uses +1. Caution should be exercised when using "," as a separator when using person or organisation names containing this punctuation to separate first and second names etc.
#' @examples
sep_count <- function(data, col = "", sep = "[^[:alnum:]]+") {
  sepcount <- stringr::str_count(data[[col]], pattern = sep)
  n <- as.integer(max(sepcount)+1)
  print(n)
}