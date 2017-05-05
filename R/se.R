#' @rdname se
#' @title
#' Standard Error
#' @aliases
#' se
#' @author
#' K. Preston and D. Sambrano
#' @description
#' This function produces the standard error of a given input.
#' @usage se(x, sigma = FALSE, N)
#' @param x A vector of data for which you wish to calculate the
#' standard error for.
#' @param sigma Optional argument used if you have population standard deviation
#' @param N Total sample sizes, required if you use sigma and do not include the data.
#' @return
#' Produces the standard error of a given input.
#' @examples
#' se(rnorm(10,3,1))
#'  #or
#' se(sigma = 6, N = 25)
# Standard Error
se <- function(x, sigma = FALSE, N = length(x)) {
  if (sigma == FALSE) {
    sd <- sd(x)
    se <- sd / sqrt(N)
    return(se)
  }
  if (!(sigma == FALSE)) {
    se <- sigma / sqrt(N)
    return(se)
  }
}
