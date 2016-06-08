#' @title Select field in dataframe containing ISI and/or DOI, separate, gather and
#' trim
#'
#' @description For working with scientific literature containing concatenated
#'   author, country, organisation or other fields. The data.frame must contain
#'   isi or doi numbers or something similar for the function to work.
#'
#' @param data A data frame containing the target concatenated field and isi or
#'   doi
#' @param x A concatenated character column (authors, organisations etc)
#' @param sep a separator (normally `;`)
#' @details Prints the number of separated columns for information part way through the
#'   function.
#' @return a data frame containing the target (whitespace trimmed), isi and doi
#'   fields
#' @export
#' @examples process_isi(df, x = "authors", sep = ";") \not run
process_isi <- function(data, x = "", sep = ""){
  data <- dplyr::select_(data, x, "isi", "doi") # must be quoted for select_
  n <- sep_count(data, x, sep)
  separate_gather(data, x, 1:n, sep, key = "key", x, 1:n, na.rm = TRUE) %>%
    dplyr::select(-key) # does this need to be select_
  data$x <- data$x %>% # not clear that this is working, try select, trim, paste0
    stringr::str_trim(side = "both")
}