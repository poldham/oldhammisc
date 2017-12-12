# Geonames function workings

# Aim: import geonames country files from the web into R as tables and allow selections by country or multiple countries

# ----obtain page text, parse zip and txt file names----
# there are a mixture of file names and a readme txt (with repetition of file name & extension) on the dump page. Separate into dump_files and dump_text and then into dump_names. Date of modification and file size not retrieved.

# prepare the geonames data
dump <- xml2::read_html("http://download.geonames.org/export/dump/")

dump_files <- rvest::html_nodes(dump, "pre:nth-child(4)") %>%
  rvest::html_text()

dump_readme <- rvest::html_nodes(dump, "pre:nth-child(5)") %>%
  rvest::html_text()

# country and other data files are in zip. Descriptors are in .txt. I may want to create a data frame from this instead with Name, Last Modified, Size, Description.

dump_zip <- stringr::str_extract_all(dump_files, "\\b[a-zA-Z0-9_-]+\\.zip") %>% unlist()
# this misses out shapes_simplified_low.json.zip

dump_txt <- stringr::str_extract_all(dump_files, "\\b[a-zA-Z0-9_-]+\\.txt") %>% unlist()
#misses modifications-2017-08-31.txt
#alternateNamesDeletes-2017-08-31.txt

dump_names <- tibble::tibble(file = c(dump_zip, dump_txt)) %>% separate(., file, c("iso", "type"), sep = "[.]", remove = FALSE)
# note that not quite complete

# ----Create countryinfo names table----

# The table is extracted from countryInfo.txt, column names to lower, other characters and spaces replaced with `_`.

countryinfo <- readr::read_delim("data/geonames_countrytable.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
names(countryinfo) <- tolower(names(countryinfo))
names(countryinfo) <- stringr::str_replace(names(countryinfo), "#", "")
names(countryinfo) <- stringr::str_replace(names(countryinfo), "-", "_")
names(countryinfo) <- stringr::str_replace_all(names(countryinfo), "[(]", "_")
names(countryinfo) <- stringr::str_replace_all(names(countryinfo), "[)]", "")
names(countryinfo) <- stringr::str_replace_all(names(countryinfo), " ", "_")

save(countryinfo, file = "data/countryinfo.rda")

#---- Create function----

import_geonames <- function(countrycode = NULL, countryname = NULL, path = NULL){
  load("data/countryinfo.rda")
  # To do. Add official name column from countrycode package

  # join
  country_data <- left_join(dump_names, countryinfo, by = "iso")

  # filter to one or more country and create the url as below

  # create tibble with urls
  baseurl <- "http://download.geonames.org/export/dump/"
  dump_url <- paste0(baseurl, dump_names)
  df <- tibble(dump_names, dump_url) %>%
    tidyr::separate(., dump_names, c("file_name", "file_type"), sep = "[.]", remove = FALSE)

}



import_geonames <- function(path){
  # tibble for txt for paste
  geofiles <- dir(path) %>%
    tibble() %>%
    rename_(., "file" = ".") # update

  geofiles$txt <-
    stringr::str_detect(geofiles$file, "[.]txt")

  geofiles <- geofiles %>%
    dplyr::filter(., txt == TRUE) %>%
    select_(., "file")

  # create country_code field
  geofiles$country_code <- stringr::str_replace_all(geofiles$file, ".txt", "")

  # add country_name from countrycode package

  countrydata <- countrycode::countrycode_data %>%
    select_(., "iso2c", "country.name.en") %>%
    rename_(., "country_code" = "iso2c")

  geofiles <- left_join(geofiles, countrydata, by = "country_code")
  geofiles <- geofiles %>% rename_(., "country_name" = "country.name.en")

  # generate path for read_delim
  geofiles$path <- paste0(path, "/", geofiles$file)
  #
  # # As a list of dfs maybe use set_names. Really want the names from the
 df <- purrr::map(geofiles$path, readr::read_delim, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, col_types = cols(
   X1 = col_integer(),
   X2 = col_character(),
   X3 = col_character(),
   X4 = col_character(),
   X5 = col_double(),
   X6 = col_double(),
   X7 = col_character(),
   X8 = col_character(),
   X9 = col_character(),
   X10 = col_character(),
   X11 = col_character(),
   X12 = col_character(),
   X13 = col_character(),
   X14 = col_character(),
   X15 = col_integer(),
   X16 = col_integer(),
   X17 = col_integer(),
   X18 = col_character(),
   X19 = col_date(format = "")
 )
   ) %>% bind_rows() %>%
   dplyr::rename_(., "geonameid" = "X1", "name" = "X2", "asciiname" = "X3", "alternatenames" = "X4", "latitude" = "X5", "longitude" = "X6", "feature_class" = "X7", "feature_code" = "X8", "country_code" = "X9", "cc2" = "X10", "admin1_code" = "X11", "admin2_code" = "X12", "admin3_code" = "X13", "admin4_code" = "X14", "population" = "X15", "elevation" = "X16", "dem" = "X17", "timezone" = "X18", "modification_date" = "X19")
 df <- df %>% left_join(., countrydata, by = "country_code")
}
