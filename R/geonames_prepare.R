#' @title Extract geonames export dump file names
#' @description Fetch the geonames daily dump page and parse it to a data frame.
#' @details The function fetches the geoname daily dump page consisting of a table with "Name", "Last Modified", "Size" and "Description". The Name field is parsed to create a file_name and file_type column. The files are a mix of individual country files (e.g. AD.zip) and other files (e.g. allCountries.zip, admin2Codes.txt etc.). To facilitate country matches the file name is parsed into iso2c for two letter country codes and other
#' @return data.frame
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom magrittr "%>%"
#' @examples {geonames_prepare()}
geonames_table <- function(...){

  # the dump page has irregular spacing between file names and date modified.

  dump <- xml2::read_html("http://download.geonames.org/export/dump/")

  dump_page <- rvest::html_nodes(dump, "pre:nth-child(4) , pre a") %>%
    rvest::html_text() %>%
    stringr::str_replace(., "Parent Directory", "") %>%
    stringr::str_replace(., "-", "") %>%
    stringr::str_replace_all(., " +", " ") %>%
    stringr::str_replace_all(., " ", ",")

    # note warnings on extra col and grepl
dump_page <- suppressWarnings(readr::read_csv(dump_page, col_names = TRUE)) %>%
    dplyr::select(., 1:5) %>%
    tidyr::unite(., last_modified, c("Last", "modified"), sep = " ")

  names(dump_page) <- tolower(names(dump_page))

  dump_page$file_name <- stringr::str_split_fixed(dump_page$name, "[.]", 2) %>% .[,1]
  dump_page$file_type <- stringr::str_split_fixed(dump_page$name, "[.]", 2) %>% .[,2]

  dump_page$is_iso <- stringr::str_detect(dump_page$file_name, "^[[:upper:]]")

  # add url

  baseurl <- "http://download.geonames.org/export/dump/"
  dump_page$url <- paste0(baseurl, dump_page$name)

  # split on filter then bind. iso2c to facilitate join with country code

  iso2c <- dump_page %>% filter(., is_iso == TRUE)
  iso2c$iso2c <- iso2c$file_name
  other <- dump_page %>% filter(., is_iso == FALSE)
  other$other <- other$file_name

  df <- bind_rows(iso2c, other)



}