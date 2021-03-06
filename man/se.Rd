% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/se.R
\name{se}
\alias{se}
\title{Standard Error}
\usage{
se(x, sigma = FALSE, N)
}
\arguments{
\item{x}{A vector of data for which you wish to calculate the
standard error for.}

\item{sigma}{Optional argument used if you have population standard deviation}

\item{N}{Total sample sizes, required if you use sigma and do not include the data.}
}
\value{
Produces the standard error of a given input.
}
\description{
This function produces the standard error of a given input.
}
\examples{
se(rnorm(10,3,1))
 #or
se(sigma = 6, N = 25)
}
\author{
K. Preston and D. Sambrano
}
