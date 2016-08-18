#' @title Add a new column to a data frame and fill with a value.
#' @description Use to add an identifying column to a data frame. For example,
#'   add a column called species and fill the column with the name of the
#'   species.
#' @param data A data frame
#' @param col The column name to add (character, quoted)
#' @param value The value to fill the column with, quoted for character vector.
#'
#' @return a data frame
#' @export
#'
#' @examples \dontrun{df <- addcol(data, "species", "myrtus communis") }
addcol <- function(data, col, value){
  data[col] <- rep(value, 1,nrow(data))
  return(data)
}
