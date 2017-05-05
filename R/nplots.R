#' @title
#' Normality Plots
#' @aliases
#' nplots
#' @author
#' K. Preston and D. Sambrano
#' @description
#' Prints a 2 by 2 grid with normality plots. Plots include: histogram,
#'     qq-plot (with line), density plot, and box plot. Plots bar graphs
#'     for nominal data.
#' @usage nplots(data)
#' @param data A vector, matrix, or dataset of class data.frame.
#' @return
#' A 2 by 2 grid of normality plots for each variable in a dataset.
#' @examples
#' dv <- rnorm(500, 50, 10)
#' iv1 <- factor(rep(1:2, each = 250))
#' iv2 <- factor(rep(1:5, 100))
#' iv3 <- rnorm(500, 12, 3)
#' iv4 <- rnorm(500, 12, 100)
#' dataset <- data.frame(dv, iv1, iv2, iv3, iv4)
#' nplots(dataset)
#Normality check all plots on one screen
nplots <- function(data) {
  vars <- names(data)
  if (is.null(vars)) {
    vars <- "data"
    data <- matrix(dv, length(dv), 1)
  }
  old <- par()$mfrow
  on.exit(par(mfrow = old), add = TRUE)
  for(i in 1:length(vars)){
    if(is.factor(data[,i])) {
      par(mfrow = c(1,1))
      tab = table(data[, i])
      barplot(main = vars[i], tab, col = c(2:(length(tab)+1)))
    }
    else{
      par(mfrow=c(2,2))
      hist(data[, i], main = vars[i], xlab = "")
      qqnorm(data[, i], col = "red", main = vars[i])
      qqline(data[, i])
      plot(density(na.omit(data[, i])), ylab="Proportion", main = vars[i])
      polygon(density(na.omit(data[, i])), col = "cyan")
      boxplot(data[, i], main = vars[i])
    }
  }
}
