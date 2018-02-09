# these are the original workings that worked before the evaluation problem cropped up

#term_to_col <- function(df, col, term){
  my_fun <- function(df, col, term){
    df <- df %>% mutate(., name = str_detect(col, term))
    names(df)[names(df) == "name"] <- term
    df
  }
#}



my_funl <- function(df, col, expr){
  res <- vector(mode = "list", length = length(expr))
  for(i in 1:length(res)){
    col <- enquo(col)
    expr <- enquo(expr)
    #name_term <- paste0("name_", quo_name(expr))
    name_term <- paste0(quo_name(expr))
    df <- df %>% mutate(!!col, !!name_term := str_detect(!!col, !!expr))

  }
}


  #map_df(sentences_country, my_fun, "ocean")


# now I want to use that in a call to map. I assume I will need a vector or df of terms to send
  # is a way to do it to create a separate col and then bind it on to it
  ops_loop <- function(x, timer = 20) {
    ops_results <- vector(mode = "list", length = length(x))
    for(i in 1:length(ops_results)) {
      my_urls <- x[[i]]
      raw_results <- httr::GET(url = my_urls, httr::content_type("plain/text"), httr::accept("application/json"))
      content <- httr::content(raw_results)
      ops_results[[i]] <- content
      message("getting_data")
      Sys.sleep(time = timer)
    }
    return(ops_results)
  }


  res <- vector(mode = "list", length = length(terms))
  for (term in c("ocean", "marine", "hydrothermal")){
    sentences_country %>% mutate(word, name = str_detect(word, term))
    names(sentences_country)[names(sentences_country) == "name"] <- term
    sentences_country
  }

#  add_column(sentences_country, ocean = str_detect(sentences_country$word, "ocean"#)) %>% View()



# this sets up the function that I want to use in a call to map to create multiple columns. It takes a df, the column to be searches and the term.
#my_fun <- function(df, col, term){
#  df <- df %>% mutate(., name = str_detect(col, term))
#  names(df)[names(df) == "name"] <- term
#  df
#}

lapply(sentences_country, my_fun, col = "word", term = "ocean")
 #Weird class character, probably something to do with evaluation