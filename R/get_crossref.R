#' @title get_cross ref dois
#' @description A simple wrapper for rcrossref cr_works that returns a data.frame.
#' @param dois A vector of dois
#'
#' @return A data frame
#' @export
#' @details See the ropensci rcrossref package \url{https://github.com/ropensci/rcrossref}
#' @examples get_crossref(dois) \not run
get_crossref <- function(dois){
  data <- cr_works(dois = dois)
  data <- data$data # returns a data frame dropping other results
}
