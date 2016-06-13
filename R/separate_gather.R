#' Separate a concatenated column on a separator and gather into a new column
#'
#' Combines the common task of separating multiple values in a single column into individual columns based on a separator and then gathering them back in again.
#' @details The function uses separate_ and gather_ from tidyr. A function sep_count counts up the number of separators and +1 to calculate the number of columns to be used by separate. These are then gathered back in. The default for na.rm is set to TRUE.
#' @param data = A data frame to be processed.
#' @param col A concatenated character column to separate using sep (quoted)
#' @param sep The separator e.g. `;`
#' @param new_col Name of the character column to gather the data into (quoted).
#' @param na.rm Logical. Should NA values be removed. Default = TRUE
#' @importFrom tidyr separate
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @export
#' @return A dataframe with separated observations in new rows and a named value column.
#'
#' @examples \dontrun{separate_gather(data, "authors", sep = ";", new_col = "auth_split", na.rm = TRUE)}
separate_gather <- function(data, col = "", sep = "[^[:alnum:]]+", new_col = "", na.rm = TRUE){
  sepcount <- stringr::str_count(data[[col]], pattern = sep)
  n <- as.integer(max(sepcount)+1)
  tidyr::separate_(data, col, into = 1:n, sep) %>%
    tidyr::gather_(., key = "key", new_col, 1:n, na.rm = na.rm)
}
# key is "key" as a requirement of gather but throws a Note on checks
# length of example throws a Note on checks and would be truncated in pdf manual
# Note that with the programmatic versions of tidyr separate and gather used here the character vector needs to be specified or it won't work.



