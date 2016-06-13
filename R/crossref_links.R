#' @title Extract crossref link data into a data.frame
#' @description  Takes the return from rcrossref \code{cr_works} and extracts the data in the link field and joins it to the source data.frame.
#' @param col the list column of a data.frame containing cross ref link
#' @param source The source data.frame
#' @return A data.frame
#' @details A DOI (quoted) column must be present in the source data.frame.
#' @export
#' @importFrom plyr ldply
#' @references \url{https://github.com/ropensci/rcrossref}
#' @examples \dontrun{links(col, source)}
links <- function(col, source){
  data <- plyr::ldply(synbio_crossref$link, .id = "DOI") %>%
    rename(intended_application = intended.application, content_version = content.version, content_type = content.type, link_URL = URL)
  data <- dplyr::left_join(data, source, by = as.character("DOI"))

}
