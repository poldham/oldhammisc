#' @title convert publication number to derwent
#' @description Work in progress function to convert publication numbers from PATSTAT to Derwent Innovation format. This involves identifying EP publications with a zero at the start of a number and removing them. For US numbers those numbers that are 14 charcaters are converted to 15 characters through the addition of a padding zero after CCYYYY.
#' @param x a data.frame
#' @param col A column containing publication numbers to be transformed.
#' @details Be careful when using this function as it is not thoroughly tested. Also be careful that for existing mixed datasets conversion may duplicate an existing derwent innovation number.
#' @return a data frame containing the original publication numbers and a column in derwent format. Could be updated to tidyeval
#' @export
#'
#' @examples \dontrun{format_toderwent(problem_numbers)}
format_toderwent <- function(x, col){
  x[[col]] <- trimws(x[[col]], which = "both")
  x <- x %>% dplyr::mutate(., cc = substring(x[[col]], 1,2)) %>%
    dplyr::mutate(., nchar = nchar(x[[col]]))

  # extract everything that is not EP or US. Note that WO to be added in full function.
  not_epus <- x %>% dplyr::filter(., cc != "EP") %>%
    dplyr::filter(., cc != "US")
  not_epus  <- not_epus %>%
    dplyr::mutate(., derwent_format = publication_number)

  # Divide the df into two for the EP and the US cases, process, then bring together again.
#
#   #EP numbers. Address leading zeros in EP where either 0 or 00
#   # Watch out for cases where there are less than or more than 11 characters. Tests needed.
#  THE PROBLEM IS THAT THE SELECT IS TAKING ALL OF THE OTHER THINGS OUT. I JUST WANT TO DROP THE TEMP COLS I HAVE CREATED.

  ep <- x %>% dplyr::filter(., cc == "EP", nchar == 11) %>%
    dplyr::mutate(., num = substring(.data[[col]], 3,11))
   ep$num <- stringr::str_replace(ep$num, "^0", "") %>%
     stringr::str_replace(., "^0", "")
   ep <- ep %>%
     tidyr::unite(., derwent_format, c(cc, num), sep = "") %>%
     select(publication_number, derwent_format)
#
#    # US numbers. Add zeros in appropriate places to arrive at a uniform 15 characters.
#
   us <- x %>% dplyr::filter(., cc == "US", nchar < 15) %>%
     dplyr::mutate(., usccyr = substring(.data[[col]], 1,6)) %>%
     dplyr::mutate(., usnum = stringr::str_sub(.data[[col]], 7, -1)) %>% dplyr::mutate(., us_nchar = nchar(.$usnum)) # could cause issues
# #    now need to filter those that are nchar 8 and then add a zero to the front and then unite those
     us8 <- us %>% dplyr::filter(., us_nchar == 8) %>%
       dplyr::mutate(., reformat_us = paste0("0", usnum)) %>%
       tidyr::unite(., derwent_format, c(usccyr, reformat_us), sep = "") %>% select(publication_number, derwent_format)
#
#     # # less than 8 char left as is. The use of publication_number is a problem here
     usother <- us %>%
      filter(., us_nchar < 8) %>%
      mutate(., derwent_format = publication_number) %>%
      select(publication_number, derwent_format)
#      # 15 is the standard length. So this needs a test.
      us15 <- us %>%
        filter(., us_nchar == 15) %>%
        mutate(., derwent_format = publication_number) %>%
        select(publication_number, derwent_format)
#
us <- dplyr::bind_rows(us8, usother, us15)
#
x <- bind_rows(not_epus, ep, us)

}
