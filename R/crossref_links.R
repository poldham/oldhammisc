#' @title link
#' @description  
#' @param data 
#' @param col 
#' @param sep 
#' @param convert 
#'
#' @return
#' @export
#'
#' @examples
links <- function(col, source){
  data <- plyr::ldply(synbio_crossref$link, .id = "DOI") %>% 
    rename(intended_application = intended.application, content_version = content.version, content_type = content.type, link_URL = URL)
  data <- dplyr::full_join(data, source, by = as.character("DOI"))
  
}
