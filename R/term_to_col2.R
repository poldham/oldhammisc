#' Search a column to identify terms and add column
#' @description Takes a vector of search terms and searches the column of a data.frame. Returns named columns to the data frame with logical values.
#' @param df A data frame with the target column
#' @param col The column to be searched
#' @param expr A search term or vector of search terms
#'
#' @return
#' @export
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom tidyr separate
#' @importFrom stringr str_replace
#' @importFrom dplyr bind_cols
#' @examples \dontrun{a <- term_to_col2(sentences_country, "word", expr = c("ocean", "marine"))}
term_to_col2 <- function(df, col, expr){
  l <- length(expr)
  if (l > 1) {
    # could do this easily with ldply
    res <- map(df[[col]], str_detect, expr) %>%
      as.matrix() %>%
      as_tibble() %>%
      separate(V1, into = paste0(expr), sep = ",", remove = TRUE, convert = TRUE)
    # deal with the mess before joining
    m <- max(length(res))
    res[[1]] <- str_replace(res[[1]], "c[(]|[)]", "")
    res[[m]] <- str_replace(res[[m]], "c[(]|[)]", "")
    res <- bind_cols(df, res)

#     cl <- class(res)
#     if (cl == "party_df") {
#       # need to just run to res for part_df to understand this
#       res
#
#     } else {
#       res <- bind_rows(df, res)
#     }
} else {
  res <- str_detect(df[[col]], expr) %>%
    as_tibble(nm = expr)
  res <- bind_cols(df, res)
}
  if (is.null(expr)) {
    warning("expr requires a value")
  }
  res
}