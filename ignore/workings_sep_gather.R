species_isi <- readxl::read_excel("/Users/pauloldham17inch/Desktop/GIZ\ Africa/abs-africa/parse_isi/from_tableau.xlsx", col_names = TRUE) # works

species_sep <- tidyr::separate(species_isi, Species, 2:150, sep = ";", fill = "right")
species_gather <- tidyr::gather(species_sep, key, species, 2:150, na.rm = TRUE) %>%
  dplyr::select(-key)

separate_gather <- function(data = x, col = NULL, into = NULL, sep = "[^[:alnum:]]+", key = NULL, newcol = NULL, from = NULL, na.rm = NULL){
  tidyr::separate_(data, col, into, sep, fill = "right") %>%
    tidyr::gather_(., key, newcol, from, na.rm = na.rm)
} # ok that works, note that with the programmatic versions the character vector needs to be specified or it doesn't work.

author_count <- function(data, col = "", sep = "[^[:alnum:]]+") {
  library(stringr)
  authcount <- str_count(data[[col]], pattern = sep)
  n <- as.integer(max(authcount)+1)
  print(n)
}

separate_gather(species_isi, "Species", 2:150, sep = ";", key = "key", newcol = "species", from = 2:150, na.rm = TRUE)