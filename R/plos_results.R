plos_results <- function(query = "", limit = ""){
  df <- lapply(s, function(x) searchplos(x, fl = c('id', 'author', 'publication_date', 'title', 'abstract'), limit = as.numeric(limit)) %>%
      setNames(s) %>% 
      ldply("[[", 2)
}

  
