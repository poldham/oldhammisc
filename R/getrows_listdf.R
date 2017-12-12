#' @title Get n rows from a list of data.frames
#'
#' @param x A list of data.frames such as one created using `split(.,
#'   asean_categories [, 1])`
#' @param rows The number of rows to return per data.frame (e.g. 1:5)
#'
#' @return a list of data frames with the corresponding answer
#' @export
#' @examples
getrows_listdf <- function(x, rows = NULL){
  x <- purrr::map(x, `[`, rows,)
  #or lapply(ascat, "[", 1:5,)
  x
}

