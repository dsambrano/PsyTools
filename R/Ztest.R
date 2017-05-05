#' @rdname Ztest
#' @name z.test
#' @title
#' Z-test
#' @aliases
#' z.test
#' @author
#' K. Preston and D. Sambrano
#' @description
#' This function produces the z statistic of a given input.
#' @usage z.test(X, mu, sigma, N)
#' @param X A vector of the data or the mean value of the data.
#' @param mu The population mean, default is 0.
#' @param sigma The population standard deviation, default is 1.
#' @param N Sample Size. Default length of X.
#' @return
#' Produces the standard error of a given input.
#' @examples
#' z.test(rnorm(10,3,1))
#'  #or
#' z.test(X = 50, mu = 45, sigma = 6, N = 25)
# Z test
z.test <- function(X, mu = 0, sigma = 1, N = length(X)){
  se <- sigma / sqrt(N)
  z <- (mean(X) - mu) / se
  pval <- pnorm(z, lower.tail = FALSE)
  print(paste("z = ", round(z, 3), ", p = ", round(pval, 3), sep = ""))
}
