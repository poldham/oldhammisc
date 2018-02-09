#' @title Read an Excel file from a URL
#' @description Read an online excel file where the URL contains the file extension or a zip file. The type argument attempts to address cases where the file extension is known but is not found in the url itself.
#' @param url A url containing an excel .xls or xlsx or .zip file extension
#' @param type .xls or .xlsx or other excel type (not tested)
#' @param ... Other arguments to readxl::read_excel
#'
#' @return data.frame
#' @export
#' @importFrom httr write_disk
#' @importFrom stringr str_detect
#' @examples \dontrun{ dfxls <- readxl_online("https://github.com/wipo-analytics/opensource-patent-analytics/blob/master/2_datasets/pizza_all_24294/patentscope_pizza_1940_2005_9659.xls?raw=true")
#'
#' dfxlsx <- readxl_online("https://github.com/wipo-analytics/opensource-patent-analytics/blob/master/2_datasets/ewaste/ewaste.xlsx?raw=true")
#'
#' dfzip <- readxl_online("https://github.com/poldham/opensource-patent-analytics/blob/master/2_datasets/taxonomy_final.zip?raw=true")
#' }
readxl_online <- function(url, type = NULL, ...){
  test <- stringr::str_detect(url, "[.]xlsx|[.]xls|[.]zip")
  if (test == FALSE) {
    print(message("Expecting file extension of type .xlsx, .xls or .zip in the URL. Check the URL or the data source for the correct file extension and use the type argument"))
  }
  # test for individual file extensions
  # for xls use look forward, xls not followed by x
  t1 <- stringr::str_detect(url, "[.]xlsx")
  t2 <- stringr::str_detect(url, "[.]xls(?!x)")
  tz <- stringr::str_detect(url, "[.]zip")
  if (t1 == TRUE) {
    type = ".xlsx"
  }
  if (t2 == TRUE) {
    type = ".xls"
  }
  if (tz == TRUE) {
   httr::GET(url, write_disk("tmp.zip", overwrite = TRUE))
   tmp <- unzip("tmp.zip")
  # On mac osx more than one file name is returned,
  # select first element. To be tested on windows.
  df <- readxl::read_excel(tmp[[1]])
  return(df)
  }
  if (!is.null(type)) {
    type = type
  }

  df <- httr::GET(url, write_disk(paste0("tmp", type), overwrite = TRUE))
  df <- readxl::read_excel(paste0("tmp", type))
}
