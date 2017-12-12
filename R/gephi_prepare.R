#' @title Prepare data for network visualization in gephi
#' @description This function imports a .csv file, selects the desired columns, trims white space and adds a weight field based on the number of rows. Use the output to create a nodes file (gephi_nodes()) and edges file (gephi_edges()) for import into gephi.
#' @param x a .csv format file containing the data to visualize
#' @param col The column you would like to network
#' @param count_col The column used as the basis for calculating shared records as edges (for example dois, isi numbers, publication_numbers, priority_numbers, family_numbers).
#'
#' @return A data.frame
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr select_
#' @importFrom stringr str_trim
#' @examples
gephi_prepare <- function(x, col, count_col){
  df <- readr::read_csv(x) %>%
    dplyr::select_(col, count_col) %>%
    na.omit()
    df %>% stringr::str_trim(side = "both")
    df$weight <- rep(1, nrow(df))
    df
}
