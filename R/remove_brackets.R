#' @title Remove Text in Brackets
#' @description Uses string R to replace text in brackets
#' @param x A vector or column in a data frame
#'
#' @return x A vector or data frame column
#' @export
#' @importFrom stringr str_replace
#' @examples \dontrun{stringr::str_replace(doubtful$scientificname, " \\(.*\\)", "")}
remove_brackets <- function(x){
  x <- stringr::str_replace(x, " \\(.*\\)", "")
}

