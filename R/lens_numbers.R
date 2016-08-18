#' @title Remove spaces and extra characters in patent numbers from the Lens.
#' @description Removes spaces after country codes and before kind codes along
#'   with "/" in patent numbers in data from the Lens. Useful for harmonising
#'   numbers for work with espacenet, Open Patent Services and other databases.
#' @param df A data.frame containing patent numbers
#' @param col the character column (quoted) containing the publication numbers
#' @details Note that the Lens is planning a facility that will allow sets of
#'   patent numbers to be uploaded. It can be assumed that the same format will
#'   be required as for data downloaded from the Lens. It will thefore often be
#'   prudent to create a new column with the edited number as in example 2
#'   below.
#' @return a data.frame
#' @export
#'
#' @examples \dontrun{df <- lens_numbers(df, "publication_number)}
#' @examples \dontrun{df$new_col <- lens_numbers(df, "publication_number")}
lens_numbers <- function(df, col){
  df[[col]] <- gsub(" ","", df[[col]])  # remove blank spaces
  df[[col]] <- gsub("/","", df[[col]]) # remove "/"
  return(df)
}
