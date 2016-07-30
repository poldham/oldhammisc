#' @title Retrieve total records from rplos
#' @description This function takes a search query and calculates the total
#'   number of results recorded in plos (see plos_records). It then retrieves
#'   the results with the id, author, publication_date, title and abstract
#'   fields in a data table. A column id field identifies the search terms in
#'   the query. Note that it is generally a good idea to run plos_records first
#'   to view the number of records that will be returned.
#' @param q a single term 
#' @details The function is presently limited to the datafields specified
#'   internally and should be updated to allow for the arguments as in
#'   rplos::searchplos.
#' @return a data frame
#' @export
#' @importFrom plyr ldply
#' @importFrom magrittr %>% 
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rplos searchplos
#' @examples \dontrun{s <- c('"synthetic biology"', '"synthetic genomics"', '"synthetic genome"', '"synthetic genomes"')}
#' \dontrun{res <- plos_multi(s)}
plos_multi <- function(q) {
  library(plyr) #for ldply
  library(dplyr) #for pipes, select and filter
  library(rplos)
  res <- lapply(q, function(x) searchplos(x, limit = 0)) %>%
    ldply("[[", 1) %>% #get meta from the lists
    select(numFound) %>% #select numFound column of meta
    filter(numFound == max(numFound)) %>% #filter on max numFound
    print() #print max value of numFound
  df <- lapply(q, function(x) searchplos(x, fl = c('id', 'author', 'publication_date', 'title', 'abstract'), limit = res)) %>%
    setNames(s) %>% # use the search terms in the columns. 
    ldply("[[", 2)
}
