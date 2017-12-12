library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
# 1. Read in file, select columns, trim whitespace, add weight count.
# Insert path to file inside quote marks at read_csv. It should look like something like "~/opensource-patent-analytics/2_datasets/synbio_patents/synbio_applicants.csv"
synbio <- read_csv("your file name here") %>%
  select(applicants_clean, publication_number) %>%
  na.omit() # ignore date warnings
synbio$applicants_clean <- str_trim(synbio$applicants_clean, side = "both")
synbio$Weight <- rep(1, nrow(synbio)) # add column Weight filled with number 1.
# 2. Melt the data frame and create a matrix of values above 0 removing the diagonal.
dat <- melt(synbio) # melt the df. SO solution
w <- dcast(dat, applicants_clean~publication_number)
x <- as.matrix(w[, -1])
x[is.na(x)] <- 0
x <- apply(x, 2,  function(x) as.numeric(x > 0))
v <- x %*% t(x) #transpose
diag(v) <- 0 # remove the diagonal
dimnames(v) <- list(w[,1], w[,1]) # takes the dim names from matrix w
# 3. Create an edges table
edges <-
  as.data.frame(v) %>%
  add_rownames(., "Source") %>%
  gather(Target, Weight, 2:363) %>%
  filter(Weight >= 1)
edges$Type <- "Undirected" # Add type otherwise will be Directed graph
# 4. create a nodes table with the number of records for the applicant
nodes <-
  count(synbio, applicants_clean, wt = Weight, sort = TRUE) %>%
  distinct(applicants_clean, n) %>% # review if needed
  rename(., "Records" = n)
# 5. write the files to disk
readr::write_csv(nodes, "synbio_nodes.csv")
readr::write_csv(edges, "synbio_edges.csv")
# note: code works but requires an update to address new tibble table message at as.data.frame format and review of other warning messages.
# rewrite as reusable function allowing different field choices from source data.
