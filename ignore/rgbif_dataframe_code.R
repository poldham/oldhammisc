#identify rgbif results in list that are "no data found" using a logical test
#and remove them. Then combine data frames into one data frame using ldply
df <- df_species
df$type <- lapply(df, is.data.frame) ##add logical TRUE/FALSE type column
df[df$type == FALSE] <- NULL #remove FALSE
df$type <- NULL #remove type as not a data.frame and will cause an error
dfr <- rbind.fill(df) #or use ldply(df)
View(dfr)
limited <- select(dfr, name, kingdom, family, genus)
df_solution <- ldply(df)
class(df_solution)
View(df_solution)

#function version
##iterate over the list identifying and removing entries with "no data found" using logical test in temp. Then combines
##list of data.frames into single data frame.
##x = a raw list in rgbif containing a mix of data frames and character items with "no data found"
rgbif_dataframe <- function(x){
  library(plyr)
  x$tmp <- lapply(x, is.data.frame) 
  x[x$tmp == FALSE] <- NULL
  x$tmp <- NULL
  rbind.fill(x)
  View(x)
}
