#' @title Read in a file downloaded from geonames and regularise the column names
#' @description Assumes a file has been downloaded from the geonames dump, imports with the correct column type and assigns the correct name.
#' @param path
#'
#' @return data.frame
#' @export
#'
#' @examples
geonames_single <- function(path){
df <-  readr::read_delim(path, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, col_types = cols(
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
  X15 = col_character(),
  X16 = col_integer(),
  X17 = col_integer(),
  X18 = col_character(),
  X19 = col_date(format = "")
)
) %>% bind_rows() %>%
  dplyr::rename_(., "geonameid" = "X1", "name" = "X2", "asciiname" = "X3", "alternatenames" = "X4", "latitude" = "X5", "longitude" = "X6", "feature_class" = "X7", "feature_code" = "X8", "country_code" = "X9", "cc2" = "X10", "admin1_code" = "X11", "admin2_code" = "X12", "admin3_code" = "X13", "admin4_code" = "X14", "population" = "X15", "elevation" = "X16", "dem" = "X17", "timezone" = "X18", "modification_date" = "X19")
#df <- df %>% left_join(., country_data, by = "country_code")
}