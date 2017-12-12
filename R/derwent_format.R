#' @title Derwent Innovation Text file name matcher
#' @description Downloads of text files in Derwent Innovation take a long form including the date and extension txt (e.g. EP1224299A220020724.txt). There is no consistent separator between the publication number and file names are not a consistent length. This prevents joining with table format downloads. This function reformats the numbers to solve this problem.
#' @param x A data frame
#' @param col A column containing long form publication numbers
#' @return data.frame
#' @details Derwent Innovation downloads of patents texts in txt format use patent numbers in long form. These numbers are not a uniform length. However, the YYYYMMDD.txt chunk of the string is a uniform 12 characters. The function calculates the length of each string and deducts the last 12 characters to arrive at the publication number for matching with table data.
#' @export
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace_all
#' @examples \dontrun{df <- derwent_format(derwent_text_format, "doc_id")}
derwent_format <- function(x, col){
  count <- nchar(x[[col]]) - 12
  x$publication_number <- stringr::str_sub(x[[col]], 1, count)
  x$publication_number <- stringr::str_replace_all(x$publication_number, "_", "")
  x
}
