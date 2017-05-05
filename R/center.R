#' @rdname center
#' @name center
#' @title
#' Centering Data
#' @aliases
#' center
#' @author
#' K. Preston and D. Sambrano
#' @description
#' Produces the centered data (e.g., new mean = 0 )
#' @usage center(data)
#' @param data data A matrix, or dataset of class data.frame.
#' @param c.vec A vector of center values, defaults to mean of
#'      each variable.
#' @return
#' Returns a dataframe or matrix with all variables centered
#' @examples
#' dv <- rnorm(500, 47, 12)
#' iv1 <- rnorm(500, 0, 6)
#' iv2 <- rnorm(500, 100, 5)
#' iv3 <- rnorm(500, 12, 3)
#' iv4 <- rnorm(500, 12, 100)
#' dataset <- data.frame(dv, iv1, iv2, iv3, iv4)
#' center(dataset)
# Centering Data
center <- function(data, c.vec = colMeans(data)) {
  if (is.vector(data)) {warning("Data must be a matrix or data.frame")}
  mean.mat <- matrix(rep(c.vec, 20), nrow(data), ncol(data), byrow = TRUE)
  centered.data <- data - mean.mat
  return(centered.data)
}
