#' @title Retrieve links from a google search
#' @description Use a url created with google_url() to retrieve a list of links
#'   for the research results. This function needs combining into one and to
#'   handle parsing of the urls returned.
#' @param url A search url created with google_url()
#'
#' @return A list of urls requiring further parsing (at the moment)
#' @importFrom XML htmlTreeParse
#' @importFrom XML getNodeSet
#' @importFrom XML xmlAttrs
#' @importFrom RCurl getURL
#' @details The URLS returned need to be parsed at the beginning after `=`` and
#'   then before `&sa`.
#' @export
#' @details from this SO question.
#'   \url{http://stackoverflow.com/questions/32889136/how-to-get-google-search-results}
#' @examples get_google_links(url) \not run
get_google_links <- function(url) {
  doc <- getURL(url, httpheader = c("User-Agent" = "R
    (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
    (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}

#the urls that are returned from this need to be parsed at the beginning after `=`` and then before `&sa``