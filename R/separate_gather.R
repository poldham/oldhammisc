#' Separate column values then gather them back in with tidyr
#'
#' Combines the common task of separating multiple values in a single column into individual columns based on a separator and then gathering them back in again.
#'
#' @param data x = A data frame
#' @param col A character column to separate
#' @param into The number of columns to separate into
#' @param sep The separator
#' @param key used by gather(). Only specify if required.
#' @param new_col Name of the character column to gather the data into.
#' @param from numeric position of columns to gather into new-col
#' @param na.rm Logical. Should NA values be removed. Default = TRUE
#' @importFrom tidyr separate
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @return A dataframe
#'
#' @examples
separate_gather <- function(data = x, col = "", into = NULL, sep = "[^[:alnum:]]+", key = NULL, new_col = "", from = NULL, na.rm = TRUE){
  tidyr::separate_(data, col, into, sep, fill = "right") %>%
    tidyr::gather_(., key, new_col, from, na.rm = na.rm)
}

# Note that with the programmatic versions of separate and gather the character vector needs to be specified or it doesn't work. `from` is the same as `into` and could be combined into 1 param. integrate sep_count.



