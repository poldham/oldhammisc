#' @title Extract sections of Derwent patent full texts to columns
#' @description Given a data frame containing the full texts of patent texts, this function provides options to split the texts into number, title, abstract, description or claims. Options are also provided to just break out the title abstract or claims ("tac") or all fields.
#' @param x a data.frame
#' @param col A column in x
#' @param section either number, title, abstract, description, or claims or "tac" or "al;l"
#' @param remove Whether to remove the input column. Default is true.
#' @return A data.frame with new columns
#' @export
#'
#' @examples \dontrun{derwent_extract(five_texts, "tac")
#' derwent_extract(five_texts, "all")}
derwent_extract <- function(x, col, section, remove = TRUE){
  if (section == "number") {
    x$number <- gsub('^.*\npn\\s*|\\s*\ntie.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  if (section == "title"){
    x$title <- gsub('^.*\ntie\\s*|\\s*\nabe.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  if (section == "abstract"){
    x$abstract <- gsub('^.*\nabe\\s*|\\s*\\ndsc.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  if (section == "description"){
    x$description <- gsub('^.*\ndsc\\s*|\\s*\\ncle.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  if (section == "claims"){
    x$claims <- gsub('^.*\ncle\\s*|\\s*\\.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  if (section == "tac"){
    x$number <- gsub('^.*\npn\\s*|\\s*\ntie.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$title <- gsub('^.*\ntie\\s*|\\s*\nabe.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$abstract <- gsub('^.*\nabe\\s*|\\s*\\ndsc.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$claims <- gsub('^.*\ncle\\s*|\\s*\\.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  if (section == "all"){
    x$number <- gsub('^.*\npn\\s*|\\s*\ntie.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$title <- gsub('^.*\ntie\\s*|\\s*\nabe.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$abstract <- gsub('^.*\nabe\\s*|\\s*\\ndsc.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$description <- gsub('^.*\ndsc\\s*|\\s*\\ncle.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    x$claims <- gsub('^.*\ncle\\s*|\\s*\\.*$', '', x[[col]]) %>%
      stringr::str_replace(., "-", "")
    return(x)
  }
  x
  # this is not working at the moment
  # include the str_replace as the final operator rather than all of the repetition
  if (remove == TRUE){
    x[[col]] <- NULL
    x
  }
}
