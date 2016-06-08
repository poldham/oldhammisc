#' Separate a concatenated column on a separator and gather into a new column
#'
#' Combines the common task of separating multiple values in a single column into individual columns based on a separator and then gathering them back in again.
#' @details The function uses separate_ and gather_ from tidyr. A function sep_count counts up the number of separators and +1 to calculate the number of columns to be used by separate. These are then gathered back in. The default for na.rm is set to TRUE.
#' @param data x = A data frame
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
#' @examples separate_gather(data = data, col = "authors", sep = ";", new_col = "authors_split", na.rm = TRUE)
separate_gather <- function(data = x, col = "", sep = "[^[:alnum:]]+", new_col = "", na.rm = TRUE){
  sepcount <- stringr::str_count(data[[col]], pattern = sep)
  n <- as.integer(max(sepcount)+1)
  tidyr::separate_(data, col, into = 1:n, sep) %>%
    tidyr::gather_(., key = "key", new_col, 1:n, na.rm = na.rm)
}

# Note that with the programmatic versions of separate and gather the character vector needs to be specified or it doesn't work. `from` is the same as `into` and could be combined into 1 param. integrate sep_count.



