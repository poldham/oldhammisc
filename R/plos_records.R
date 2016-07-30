#' @title Calculate the number of records for a search of PLOS
#' @description Given a search term or vector of search terms this function returns the maximum number of records across the term. Use this to know where to set the limit in functions such as searchplos or to select a small set for review. 
#' @param q 
#'
#' @return numeric
#' @export
#' @importFrom plyr ldply
#' @importFrom magrittr %>% 
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @examples \dontrun{s <- c('"synthetic biology"', '"synthetic genomics"', '"synthetic genome"', '"synthetic genomes"')} 
#' @examples \dontrun{records <- plos_records(s)}
plos_records <- function(q) {
  library(plyr) #for ldply
  library(dplyr) #for pipes, select and filter
  lapply(q, function(x) searchplos(x, limit = 0)) %>%
    ldply("[[", 1) %>% #get meta from the lists
    select(numFound) %>% #select numFound column of meta
    filter(numFound == max(numFound)) %>% #filter on max numFound
    print() #print max value of numFound
}
