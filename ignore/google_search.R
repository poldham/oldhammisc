#' Search Google and Retrieve results
#'
#' @param query
#' @param domain
#' @param quotes
#' @details  from SO http://stackoverflow.com/questions/32889136/how-to-get-google-search-results
#' @return
#' @export
#'
#' @examples
getGoogleURL <- function(search.term, domain = '.co.uk', quotes=TRUE)
{
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='')
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',
    search.term, sep='')
}
getGoogleLinks <- function(google.url) {
  doc <- getURL(google.url, httpheader = c("User-Agent" = "R
    (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
    (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}