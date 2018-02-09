#' @title Identify the earliest priority number in derwent innovation documents
#' @description An input data frame from derwent innovation containing a column with priority numbers and dates. The function identifies the earliest dates.
#' @param x a data.frame
#' @param id A column containing ids
#' @param col A list column containing the date field
#' @param date A date as ymd. Character fields will be converted to date
#' @importFrom dplyr arrange
#' @importFrom tidyr unnest
#' @importFrom dplyr %>%
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{resultssofar %>% unnest(epodoc_priority) %>% split(.$id)}
 earliest_priority <- function(x, id = NULL, col = NULL, date  = NULL){
   #id <- enquo(id)
   col <- dplyr::enquo(col)
   #date <- dplyr::enquo(date)
   #if (class(date) != "Date") {
   #x$date <- lubridate::ymd(x$date)
   #}
   # some kind of quoting problem here
   #col <- quo(col)
   #split_fun <- function(x){
   #   dplyr::arrange(x, date)
   # }

   # unnest the column
   x <- x %>% tidyr::unnest(., !!col)
   # convert character date field to date format. Note patent standard is YYYYMMDD

   x[[date]] <- lubridate::ymd(x[[date]])
   x <- x %>% split(.[[id]])
# note that date does not work here and has to be priority_date. Probably needs quoting
   x <- x %>% purrr::map(., arrange, date) %>%
   purrr::map(., `[`, 1, ) %>%
     dplyr::bind_rows()
   x
 }

# tests
# splitwo %>% map(., arrange, priority_date1) %>% map(., `[`, 1, ) %>% bind_rows()
# splitdf %>% map(., arrange, priority_date) %>% map(., `[`, 1, ) %>% bind_rows()
# The above tests work. The question now is marking the earliest with a column