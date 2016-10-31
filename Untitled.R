#' @title Count results for query using rcrossref
#' @description A simple wrapper for rcrossref::cr_works() that extracts the total number of results from the meta data for a query. Useful for dividing up the calls within the 1000 limit using the offset argument in cr_works. e.g Call 1. cr_works(query, limit = 1000), Call 2. cr_works(query, limit = 1000, offset = 1001) where Call 2 retrievs the next 1000 results and so on.  
#' @param query 
#'
#' @return numeric
#' @export
#' @importFrom dplyr %>% 
#' @importFrom rcrossref cr_works
#' @examples \dontrun{pizza <- crossref_count(query = "pizza")}
crossref_count <- function(query) {
  cr_works(query)$meta$total_results %>% 
    as.numeric()
  # show the value printed and the number of queries required by dividing by 1000. Note the 1001 issue dealt with elsewhere. 
}