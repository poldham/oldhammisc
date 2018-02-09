#' @title reformat Thomson Innovation (Derwent Innovation) patent numbers
#' @description reformats Thomson Innovation (Clarivate Analtics Derwent Innovation) patent numbers for calls to the OPS service by removing extra zeros and correcting for the insertion of years on PCT numbers. See details.
#' @param x A data.frame containing patent numbers in Thomson Innovation/Derwent Innovation Format
#' @details Thomson Innovation patent numbers involve the insertion of extra zeros to make US patent numbers nchar 15 and will not be found by OPS. In the case of Patent Cooperation Treaty (WO) documents, the year is inserted for records prior to the 1st of January 2004. Thus the form WO98xxx becomes WO1998xxx and will not be found in a call to OPS. In a small number of cases (so far) the Thomson Innovation number deleted a leading zero from EP numbers e.g. EP0xxx becomes EPxx and will not be found. At present the function does not address this issue and results from a call to EP numbers with OPS should be checked against the input numbers.
#' @return A tibble with formatted numbers
#' @export
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tidyr unite
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @examples \dontrun{ops_thomson(inpadoc_first_family_members, col = "inpadoc_first_family_number")}
ops_thomson_redraft <- function(x, col){
  # extract countrycode to new column and nchar for filter. trime whitespace to ensure that substring works correctly
  x[[col]] <- trimws(x[[col]], which = "both")
  x <- x %>% dplyr::mutate(., cc = substring(x[[col]], 1,2)) %>%
    dplyr::mutate(., nchar = nchar(x[[col]]))
  
  # US Numbers
  # Remove extra 0s after the year in US numbers
  us <- x %>% dplyr::filter(., cc == "US", nchar == 15) %>%
    dplyr::mutate(., ccyr = substring(.data[[col]], 1,6)) %>%
    dplyr::mutate(., num = substring(.data[[col]], 8,15)) %>%
    tidyr::unite(., formatted, c(ccyr, num), sep = "") %>%
    select_(col, "formatted")
  
  # Patent Cooperation Treaty (WO)
  # Until 1.1.2004: CCyynnnnnW
  # After 1.1.2004: CCccyynnnnnnW
  wo <- x %>% dplyr::filter(., cc == "WO") %>%
    dplyr::mutate(., ccyr = substring(.data[[col]], 1,6)) %>%
    dplyr::mutate(., num = substring(.data[[col]], 7,14))
  wo$ccyr <- wo$ccyr %>% stringr::str_replace_all(., "19", "") %>%
    stringr::str_replace_all(., "2000", "00") %>%
    stringr::str_replace_all(., "2001", "01") %>%
    stringr::str_replace_all(., "2002", "02") %>%
    stringr::str_replace_all(., "2003", "03")
  wo <- wo %>% tidyr::unite(., formatted, c(ccyr, num), sep = "") %>%
    select_(col, "formatted")
  
  # EP (European Patent Organisation)
  # In what is normally a small number of cases the Thomson approach drops a leading zero from EP numbers. Only a limited number of documents appear to be affected by this and so it is not addressed here. Test cases to be developed in future.
  
  # Combine dfs adding complete formatted column
  
  out <- bind_rows(us, wo)
  # In cases where there are multiple entries in the input col join by id does not work correctly. So this case needs to be dealt with
  # test <- any(duplicated(x[[col]]))
  # if (any == TRUE) {
  #   # do something # rep_along? rep? # seq_along
  # } else {
  x <- x %>% select_(col)
  x <- left_join(x, out, by = col)
  x$formatted[is.na(x$formatted)] <- 0
  t <- x %>% filter(., formatted == 0)
  t$formatted <- t[[col]]
  x <- bind_rows(out, t)
  # }
  x
}
#inpadoc_first_family_members %>% mutate(cc = substring(inpadoc_first_family_members$inpadoc_first_family_number, 1,2))
