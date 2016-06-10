#' @title Convert data.frame columns to character
#' @description Useful where columns in a data.frame include factors or lists that you don't want to deal with at the time (e.g. exploring an API return). Helpful if you want to use readr::write_csv() but get blocked from doing so by the existence of lists in the data.frame.
#' @param data A data.frame with factor or list columns.
#' @details From an SO answer by Hadley Wickham \url{http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters}
#' @return a data.frame
#' @export
#' @examples \dontrun{character_df(data)}
character_df <- function(data){
  data[] <- lapply(data, as.character)
}
