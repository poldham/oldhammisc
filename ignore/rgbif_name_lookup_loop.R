# data to search for
library(rgbif)
africa_look <- readr::read_csv("/Users/colinbarnes/Desktop/open_source_master/rbgif_oldham/final_species_gbif_test.csv")
africa_look_parse <- taxize::gbif_parse(africa_look$species) # all names
africa_look_species <- dplyr::filter(africa_look_parse, type == "SCIENTIFIC") # scientific
africa_look_informal <- dplyr::filter(africa_look_parse, type == "INFORMAL") # not scientific names
africa_look_virus <- dplyr::filter(africa_look_parse, type == "VIRUS", type = "DOUBTFUL")

# store files for reference
readr::write_csv(africa_look_parse, "africa_look_parse_13052016.csv")
readr::write_csv(africa_look_species, "africa_look_species_13052016.csv")
readr::write_csv(africa_look_informal, "africa_look_informal_13052016.csv")
readr::write_csv(africa_look_virus_doubtful, "africa_look_virus_doubtful_13052016.csv")

# select a vector of scientific names
africa_lookup_species <- africa_look_species$scientificname

# container for results
lookup_results <- vector(mode = "list", length = length(africa_lookup_species)) # change value in length

# for loop part 1
for(i in 1:length(lookup_results)){
  get_results <- name_backbone(name = africa_lookup_species[[i]]) # $data
  lookup_results[[i]] <- get_results
  message("getting_data")
  Sys.sleep(time = 2)
}

# Note that for some reason the for loop did not complete and returned upto row 10,737.
# next step is to parse the table from 10,738 to 18,322

africa_lookup_species1 <- africa_lookup_species[10738:18322] # select remaining rows
# container for results
lookup_results1 <- vector(mode = "list", length = length(africa_lookup_species1)) # change value in length

# for loop part 2
for(i in 1:length(lookup_results1)){
  get_results <- name_backbone(name = africa_lookup_species1[[i]]) # $data
  lookup_results1[[i]] <- get_results
  message("getting_data")
  Sys.sleep(time = 2)
}

# for loop part 3
africa_lookup_informal <- africa_look_informal$genusorabove
lookup_results3 <- vector(mode = "list", length = length(africa_lookup_informal))
for(i in 1:length(lookup_results3)){
  get_results <- name_backbone(name = africa_lookup_informal[[i]], rank = "genus") # $data
  lookup_results3[[i]] <- get_results
  message("getting_data")
  Sys.sleep(time = 2)
}

#africa look part 4
africa_lookup_virus <- africa_look_virus$scientificname
lookup_results4 <- vector(mode = "list", length = length(africa_lookup_virus))
for(i in 1:length(lookup_results4)){
  get_results <- name_backbone(name = africa_lookup_virus[[i]], rank = "genus") # $data
  lookup_results4[[i]] <- get_results
  message("getting_data")
  Sys.sleep(time = 2)
}

# Process the results:

results <- plyr::ldply(lookup_results, .fun = as.data.frame) # does this solve my aggregate issue. It must do!
readr::write_csv(results, "africa_look_results_part1_13052016.csv")

results1 <- plyr::ldply(lookup_results1, .fun = as.data.frame)
readr::write_csv(results1, "africa_look_results_part2_13052016.csv")

results2 <- ldply(lookup_results3, .fun = as.data.frame)
readr::write_csv(results2, "africa_look_results_part3_13052016.csv")

results4 <- ldply(lookup_results4, .fun = as.data.frame)
readr::write_csv(results2, "africa_look_results_part4_13052016.csv")

results_complete <- bind_rows(results, results1, results2, results4)
readr::write_csv(results_complete, "africa_results_complete.csv")

# NEXT STEP _ IMPORT INTO VANTAGE POINT!!!!! AND THEN MATCH UP FROM THERE!
