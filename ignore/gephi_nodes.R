#' @title Create a nodes table for gephi
#'
#' @param x
#' @param col
#' @param count_col
#'
#' @return
#' @export
#'
#' @examples
gephi_nodes <- function(x, col = "", count_col = ""){
  reshape2::melt(x) # melt the df. SO solution
  w <- reshape2::dcast(x, col~count_col)
  x <- as.matrix(w[, -1])
  x[is.na(x)] <- 0
  x <- apply(x, 2,  function(x) as.numeric(x > 0))
  v <- x %*% t(x) #transpose
  diag(v) <- 0 # remove the diagonal
  dimnames(v) <- list(w[,1], w[,1])
  as.matrix(v)
  print(v) #output is presently an empty matrix so needs fixing
}

# dat <- melt(synbio) # melt the df. SO solution
# w <- dcast(dat, applicants_clean~publication_number)
# x <- as.matrix(w[, -1])
# x[is.na(x)] <- 0
# x <- apply(x, 2,  function(x) as.numeric(x > 0))
# v <- x %*% t(x) #transpose
# diag(v) <- 0 # remove the diagonal
# dimnames(v) <- list(w[,1], w[,1])
