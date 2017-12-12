#' Tidy up species names
#' @description Read in a list of species and then tidy up by converting to lower case and then capitalizing the first letter before using to search with rgibif or other packages. Presently assumes a column called Species exists with capital S and needs updating to make it generic.
#' @param file A csv file containing species names in a column called Species
#'
#' @return a data frame
#' @export
#' @importFrom Hmisc capitalize
#' @importFrom dplyr %>%
#' @examples \dontrun{df <- tidy_species(file)}
tidy_species <- function(file, col) {
  species_list <- (read.csv(file, stringsAsFactors = FALSE)
  )
  species_list$col %>%
    tolower() %>%
    capitalize() %>%
    print()
}
