gghorizontal_bar <- function(df, x = NULL, y = NULL){
  ggplot(., aes(x = reorder(web_of_science_categories, n), y = n)) +
    geom_bar(stat = 'identity') +
    coord_flip()
}
