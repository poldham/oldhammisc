#' @title Create data.frame with authors from crossref
#' @description This function builds on rcrossref \code{cr_works}. It takes a
#'   data.frame and extracts the `author` list column into a data.frame with one
#'   author per line and a new `full_name` column (family, given).
#' @param data x$author. The author list in a data.frame produced by
#'   \code{get_crossref}
#' @param source The source crossref data.frame name (unquoted) produced as
#'   output by \code{get_crossref}.
#' @param .id This must be the DOI field from the source crossref data.frame as
#'   x$DOI (unquoted).
#' @details A data.frame of results from crossref (see \code{get_crossref} )
#'   containing an author column in list format with author family and given
#'   names. This function extracts authors one per row per DOI. The DOI from the
#'   source table serves as the index. The extracted author data is joined with
#'   the data in the source table using dplyr \code{leftjoin} to create a new
#'   data.frame with an author name per row per DOI. A new `full_name` column is
#'   added as `family, given` using tidyr \code{unite}. The data can then be
#'   used for author name cleaning, analysis and visualization. To split other
#'   data columns for further processing use tidyr \code{separate_rows}. Warning
#'   At present this version only addresses crossref data where a DOI column is
#'   present and does not address other identifiers (ISBN, ISSN). A DOI column
#'   must be present for the function to work.
#' @return a data.frame
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom tidyr unite
#' @references \url{https://github.com/ropensci/rcrossref}
#' @examples \dontrun{crossref_authors(df$author, source = source_df, .id = source_df$DOI)}
crossref_authors <- function(data, source, .id = id){
  names(data) <- source$DOI
  data <- dplyr::bind_rows(Filter(is.data.frame, data), .id = "DOI")
  data <- dplyr::left_join(data, source, by = "DOI")
  data <- tidyr::unite(data, full_name, c(family, given), sep = ", ", remove = FALSE)
  }

