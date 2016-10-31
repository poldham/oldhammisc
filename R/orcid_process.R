#' @title Process rorcid results into a data frame
#' @description Turn raw results from orcid into a data frame
#' @details Note dependency on oldhammisc::orcid_rename
#' @param x A list of results returned from rorcid orcid_doi(). 
#' @return a data.frame
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>% 
#' @importFrom tidyr unite
#' @examples \dontRun
orcid_process <- function(x){
  names(x) <- lapply(x, attr, "doi")
  data$.id <- names(x)
  lapply(x, "[[", 2) %>% 
    Filter(is.data.frame, .) %>% 
    dplyr::bind_rows() %>% 
    oldhammisc::orcid_rename() %>% 
    tidyr::unite(., orcid_name, personal_details_family_name_value,  personal_details_given_names_value, sep = ", ", remove = FALSE)
}
