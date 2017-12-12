#' @title Match WOS author names to USPTO inventors
#' @description This draft function takes a data frame of raw unprocessed names from Web of Science and processes them to match with the USPTO raw inventors patent table as a basis for matching to cleaned inventor names.
#' @param x A vector containing raw author names.
#'
#' @return data frame
#' @export
#' @importFrom magrittr %$%
#' @examples
wos_inventor_match <- function(x){

  wos_plos_new %$% stringr::str_detect(new, ", ") -> wos_plos_new$comma

  # filter on FALSE replace first space with comma
  wos_plos_comma <- filter(wos_plos_new, comma == "FALSE")
  wos_plos_comma$new <- str_replace(wos_plos_comma$new, " ", ", ")

  # filter original for join
  wos_plos_new <- filter(wos_plos_new, comma == "TRUE")

  # join back together
  wos_plos_new <- bind_rows(wos_plos_new, wos_plos_comma)
  wos_plos_new$comma <- NULL

  # divide into name_first and name_last. Check the too many issue
  wos_plos_div <- separate(wos_plos_new, new, c("name_last", "name_first"), sep = ",", remove = FALSE)

  # # trim white space
  wos_plos_div$name_last <- str_trim(wos_plos_div$name_last, side = "both")
  wos_plos_div$name_first <- str_trim(wos_plos_div$name_first, side = "both")

  #calculate the number of spaces
  wos_plos_div$first_count <- str_count(wos_plos_div$name_first, " ")

  # filter entries with 0 seond

  wos_plos_div0 <- wos_plos_div %>% filter(., first_count == 0)

  # filter then separate with one space

  wos_plos_div1 <- wos_plos_div %>% filter(., first_count == 1)


  # filter then separate with two spaces

  wos_plos_div2 <- wos_plos_div %>% filter(., first_count == 2)

  # identify cases where the first name is only an initial
  # could do this by identifying cases where an initial is followed by a space or where two initials are CAPS. Could then be done on the original list.

  wos_plos_div0$initial_only <- map(wos_plos_div0$name_first, str_detect, "^[[:upper:]]$")

  wos_plos_div1$initial_only <- map(wos_plos_div1$name_first, str_detect, "^[[:upper:]]$")

  wos_plos_div2$initial_only <- map(wos_plos_div2$name_first, str_detect, "^[[:upper:]]$")

  # may be cases where there are three initials?

  # data where there are no initials

  wos_plos_div0_noi <- filter(wos_plos_div0, initial_only == "FALSE")
  wos_plos_div1_noi <- filter(wos_plos_div1, initial_only == "FALSE")
  wos_plos_div2_noi <- filter(wos_plos_div2, initial_only == "FALSE")

  # Bind no initials
  wos_plos_noi <- bind_rows(wos_plos_div0_noi, wos_plos_div1_noi, wos_plos_div2_noi)

  # Initials only
  wos_plos_div0_i <- filter(wos_plos_div0, initial_only == "TRUE")
  wos_plos_div1_i <- filter(wos_plos_div1, initial_only == "TRUE")
  wos_plos_div2_i <- filter(wos_plos_div2, initial_only == "TRUE")
  wos_plos_i <- bind_rows(wos_plos_div0_i, wos_plos_div1_i, wos_plos_div2_i)


  ## Need to deal with cases where there is a name and an initial also

}
