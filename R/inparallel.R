#' @title Run code in parallel
#' @description Use \code{multidplyr} to set up a local cluster to run code in parallel. Packages automatically transferred are the tidyverse and tidytext
#' @param cores Number of cores to use in the cluster
#' @param df A data frame
#' @param packages A vector of one or more packages
#' @param value A vector of values
#' @return
#' @export
#' @importFrom multidplyr create_cluster
#' @importFrom dplyr bind_cols
#' @importFrom multidplyr partition
#' @importFrom multidplyr cluster_library
#' @importFrom multidplyr cluster_assign_value
#' @examples
inparallel <- function(cores = NULL, df, packages = "NULL", value = "NULL", expr = "NULL") {
  # get_default_cluster() will create one
  cluster <- multidplyr::create_cluster(cores = cores)
  group <- rep(1:cores, length.out = nrow(df)) %>%
    tibble::tibble("group" = .)
  df <- dplyr::bind_cols(group, df)
 groupeddf <- df %>% multidplyr::partition(group, cluster = cluster)
  # # assign libraries
  # # check length of input and then add row and pipe
 # its vectorised so the if may not be necessary
 if (length(packages) == 1) {
   groupeddf %>%
     multidplyr::cluster_library(packages)
   }
  if (length(packages) > 1) {
    groupeddf %>%
      multidplyr::cluster_library(packages)
  }
# assign value
  groupeddf %>%
   multidplyr::cluster_assign_value(value, as.name(value))
  # # assign expression
  #groupeddf %>%

}