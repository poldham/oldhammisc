#' @title Retrieve count of results for a query with PLOS
#' @description Use this function to work out how many results a query of PLOS using rplos will return. Useful for deciding on data to download.
#' @param query A search term or vector of search terms. For multiple terms use double quotes (see examples).
#' @return prints the maximum value of results
#' @export
#' @importFrom plyr ldply
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @examples \dontrun{drones <- plos_records(drones)}
#' @examples \dontrun{synbio <- c('"synthetic biology"', '"synthetic genomics"', '"synthetic genome"', '"synthetic genomes"')
#' synbio <- plos_records(synbio)}
plos_records <- function(query) {
  lapply(query, function(x) rplos::searchplos(x, limit = 0)) %>%
    plyr::ldply("[[", 1) %>% # access meta
    dplyr::select(numFound) %>% # select numFound
    dplyr::filter(numFound == max(numFound)) %>% # filter max value
    print()
}