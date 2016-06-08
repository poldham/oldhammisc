get_google_url <- function(search.term, domain = '.co.uk', quotes=TRUE)
{
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='')
  get_google_url <- paste('http://www.google', domain, '/search?q=',
    search.term, sep='')
}