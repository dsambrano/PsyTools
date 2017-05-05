#' @rdname sphericity
#' @name jcsphericity
#' @title
#' Evaluate Commpound Symmetry and Sphericity
#' @aliases
#' jcsphericity
#' @author
#' K. Preston and D. Sambrano
#' @description
#' Produces a covariance matrix to test for commpound symmetry
#'     as well as variances for sphericity.
#' @usage jcsphericity(dataset)
#' @param x A matrix or data.frame object.
#' @examples
#' week1 <- rnorm(10, 0, 1)
#' week2 <- rnorm(10, 0, 1)
#' week3 <- rnorm(10, 0, 1)
#' week4 <- rnorm(10, 0, 1)
#' week5 <- rnorm(10, 0, 1)
#' dataset <- data.frame(week1, week2, week3, week4, week5)
#' jcsphericity(dataset)
# Simple Slopes
jcsphericity <- function(x){
  com.sym <- cov(x)
  n <- length(x[1, ])
  t <-array()
  l <-array()
  k <-0
  for (i in 1:(n - 1)){
    for (j in (i + 1):n){
      k <- k + 1
      t[k] <- var(x[, i] - x[, j])
      l[k] <- paste(i, j, sep = ",")
    }
  }
  names(t)<-l
  return(list("Compound Symmetry Test" = com.sym, "Sphericity Test" = t))
}
