#' @title Extract data.frames from orcid_doi searches
#'
#' @description This wraps the orcid_doi() function from rorcid and parses the
#'   results to return a data frame. Note that it drops results that are not
#'   found.
#' @param dois A vector of dois for use in orcid_doi.
#' @param start (integer) Result number to start on. Keep in mind that pages
#'   start at 0. (rorcid documentation)
#' @param rows (integer) Numer of results to return.
#' @param fuzzy fuzzy	(logical) Use fuzzy matching on input DOIs. Defaults to
#'   FALSE. If FALSE, we stick "digital-object-ids" before the DOI so that the
#'   search sent to ORCID is for that exact DOI. If TRUE, we use some regex to
#'   find the DOI.
#' @return A data frame
#' @details A convenience wrapper for the rorcid_doi function from rorcid. See
#'   the rorcid package \url{https://github.com/ropensci/rorcid} (from which the
#'   documentation is taken). The advantage of the function is simply that it
#'   returns a data.frame.
#' @export
#' @importFrom rorcid orcid_doi
#' @importFrom magrittr %>%
#' @examples \dontrun{orcid_df(dois)}
orcid_df <- function(dois = NULL, start = NULL, rows = NULL, fuzzy = FALSE){
  data <- rorcid::orcid_doi(dois=dois, start = start, rows = rows, fuzzy=FALSE)
  lapply(data, "[[", 2) %>%
    Filter(is.data.frame, .) %>%
    dplyr::bind_rows()
}

