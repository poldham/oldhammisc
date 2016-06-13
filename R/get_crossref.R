#' @title get_cross ref dois
#' @description A simple wrapper for rcrossref \code{cr_works} that returns a data.frame dropping other data. Note that this wrapper does not include the additional arguments available for \code{cr_works}.
#' @param dois A vector of dois
#' @return A data frame
#' @export
#' @importFrom rcrossref cr_works
#' @references \url{https://github.com/ropensci/rcrossref}
#' @examples \dontrun{get_crossref(dois)}
get_crossref <- function(dois){
  data <- cr_works(dois = dois)
  data <- data$data # returns a data frame dropping other results
}
