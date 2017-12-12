#' @title Create a Google Search Url
#' @description enter a search term to generate a url to search google. Use with goog
#' @param search_term A search query
#' @param domain default is .co.uk. Change to your local service as needed
#' @param quotes whether the search should be quoted or not (default is TRU)
#' @details from this SO question.
#'   \url{http://stackoverflow.com/questions/32889136/how-to-get-google-search-results}
#'
#' @return a URL
#' @export
#'
#' @examples \dontrun{google_url(pizza)}
#' @examples google_url("pizza")
google_url <- function(search_term, domain = '.co.uk', quotes=TRUE)
{
  search_term <- gsub(' ', '%20', search_term)
  if (quotes) search_term <- paste('%22', search_term, '%22', sep = '')
  get_google_url <- paste('http://www.google', domain, '/search?q=', search_term, sep = '')
}