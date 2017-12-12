#' @title Detect all duplicates in a column or vector
#'
#' @param x a data frame, column or vector
#'
#' @return data.frame
#' @export
#'
#' @examples
detect_duplicates <- function(x){
    duplicated <- duplicated(x) | duplicated(x, fromLast = TRUE)
}
