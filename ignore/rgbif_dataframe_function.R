#function version
##iterate over the list identifying and removing entries with "no data found" using logical test in temp. Then combines
##list of data.frames into single data frame.
##x = a raw list in rgbif containing a mix of data frames and character items with "no data found"
rgbif_dataframe <- function(x){
  library(plyr)
  t <- length(df_species) #get the length of the list for value of t
  x <- df_species[1:t] ##addresses rgbif object class object FIX THIS. try length(df_species for the number)
  x$tmp <- lapply(x, is.data.frame) #identify data frames and create tmp item in df
  x[x$tmp == FALSE] <- NULL #remove items in tmp where not a data frame (false)
  x$tmp <- NULL #remove tmp to leave a list of data.frames
  y <- rbind.fill(x) #combine data.frames noting that ldply(df) also works but creates .id column
  View(y) #View df prior to writing to file
} ## This function is no longer working properly


t %>% length(df_species)
x %>% df_species[1:t] %>%
  x$tmp <- lapply(x, is.data.frame) %>%
  x[x$tmp == FALSE] <- NULL %>%
  x$tmp <- NULL %>%
  rbind.fill(x) %>%
  View() #View df pr
