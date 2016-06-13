#' @title Rename crossref fields
#' @description Start of a function to rename fields generated from rcrossref \code{cr_works} with underscores. Does not yet address complete field names.
#' @param x A data.frame with columns to rename
#'
#' @return A data.frame
#' @export
#' @importFrom dplyr rename
#' @references \url{https://github.com/ropensci/rcrossref}
#' @examples \dontrun{crossref_rename}
crossref_rename <- function(x){
  dplyr::rename(x, alternative_id = alternative.id,
  container_title = container.title,
  reference_count = reference.count,
  license_content_version = license_content.version,
  license_delay_in_days = license_delay.in.days,
  update_policy = update.policy)
}