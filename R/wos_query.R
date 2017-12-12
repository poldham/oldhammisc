#' Search Web of Science
#' @description This function allows for a search of Web of Science on single search terms using the topic or the address field and one or more Web of Science Subject Categories.
#' @param topic A topic such as a country name
#' @param address A country name
#' @param wos_category A web of science category or vector or categories selected from data/wos_categories
#'
#' @return data.frame
#' @export
#'
#' @examples
wos_query <- function(topic = NULL, address = NULL, wos_category = NULL){
  obr <- "("
  cbr <- ")"
  topic <- paste0(obr, "TS=", obr, topic, cbr)
  address <- paste0("AD=", obr, address, cbr)
  tmp <- paste0(topic, " OR ", address, cbr)
  tmp1 <- paste0(wos_category, collapse = " OR ")
  wos_category <- paste(obr, tmp, cbr)
  out <- paste0(tmp, " AND ", "WC=", obr, tmp1, cbr)
}
