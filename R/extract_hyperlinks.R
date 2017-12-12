#' @title Extract All Hyperlinks
#' @description This function wraps a hrbrmstr  \href{url_pattern matcher}{https://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column} for identifying urls in texts.
#' @param x An object containing hyperlinks
#'
#' @return data.frame
#' @export
#' @importFrom stringr str_extract_all
#' @examples
extract_hyperlinks <- function(x){
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  x <- stringr::str_extract_all(x, url_pattern)
  x
}
