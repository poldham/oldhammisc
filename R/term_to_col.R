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
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @examples \dontrun{a <- term_to_col(sentences_country, "word", expr = c("ocean", "marine")))}
term_to_col <- function(df, col, expr){
  l <- length(expr)
  if (l > 1) {
    res <- map(df[[col]], str_detect, expr) %>%
      data.frame() %>%
      rownames_to_column() %>%
      t() %>%
      as_tibble()
    names(res) <- paste0(expr)
    res = res[-1, ]
    res <- bind_cols(df, res)
    # try as.matrix() %>% as_tibble()
  } else {
    res <- str_detect(df[[col]], expr)
    res <- bind_cols(df, res)
  }
  if (is.null(expr)) {
    warning("expr requires a value")
  }
  res
}