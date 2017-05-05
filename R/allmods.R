#' @rdname allmods
#' @name allmods
#' @title
#' All Possible Models
#' @aliases
#' allmods
#' @author
#' K. Preston and D. Sambrano
#' @description
#' This function produces the coeffiecients and the anova summary table
#'     for Hierarchical regression.
#' @usage allmods(dv="dv", ivs=c("iv1","iv2","iv3"),
#'         data=dataset, plot = FALSE)
#' @param dv A string of the variable name of you dependent variable.
#' @param ivs A list of strings of the names of you independent variables.
#' @param data A \code{data.frame} object.
#' @param plot A \code{logical} statement indicating whether you want the affiliated
#'    plots.
#' @return
#' A matrix with the R^2, adj R^2, PRESS, and Mallow Cp for all possible
#'     (non interaction) models given the variables.
#' @examples
#' dv <- rnorm(500, 47, 12)
#' iv1 <- rnorm(500, 0, 6)
#' iv2 <- rnorm(500, 100, 5)
#' iv3 <- rnorm(500, 12, 3)
#' iv4 <- rnorm(500, 12, 100)
#' dataset <- data.frame(dv, iv1, iv2, iv3, iv4)
#' allmods(dv ="dv", ivs=c("iv1", "iv2", "iv3"),
#'         data = dataset, plots = FALSE)
# All Possible Models
allmods <- function(dv, ivs = names(dataset)[which((!names(dataset) == "dv") == TRUE)], data, plots = TRUE) {
  old <- par()$mfrow
  on.exit(par(mfrow = old), add = TRUE)
  vars <- ivs
  ### Change this to reflect the name of the criterion variable
  y <- dv
  bin <- matrix(0, 2^length(vars), length(vars))
  for(k in 1:2^length(vars) - 1){
    x <- R.utils::intToBin(k)
    blah <- sprintf(paste("%0", length(vars), "i",sep = ""), as.integer(x))
    as.integer(x)
    bin[(k + 1), ] <- rev(substring(blah, 1:nchar(blah), 1:nchar(blah)))
  }
  bin <- noquote(bin)
  fullfmla <- stats::as.formula(paste("get(y) ~", paste(vars, collapse = "+")))
  fitfull <- lm(fullfmla, dataset)
  for(i in 1:2^length(vars)){
    if(i == 1){fit0 <- lm(get(y) ~ 1, dataset)}
    if(i > 1) {
      subvar <- subset(vars, bin[i, ] == 1)
      fmla <- as.formula(paste("get(y) ~", paste(subvar, collapse = "+")))
      assign(paste("fit", (i - 1), sep = ""), lm(fmla, dataset))
    }
  }
  ## Evaluating All Possible Models###
  Poss <-  matrix(0, 2^length(vars), 5)
  preds <- matrix(0, 2^length(vars), 1)
  for (i in 1:2^length(vars)){
    fit <- get(noquote(paste('fit', i - 1, sep = '')))
    fith <- anova(fit)
    Poss[i, 2] <- sum(fith[1:(nrow(fith) - 1), 2]) / sum(fith[1:nrow(fith), 2])
    Poss[i, 3] <- 1 - ((fith[nrow(fith), 2]) / sum(fith[1:nrow(fith), 2])) * (sum(fith[1:nrow(fith), 1]) / (fith[nrow(fith), 1]))
    Poss[i, 4] <- MPV::PRESS(fit)
    mallow <- wle::mle.cp(get(noquote(paste('fit', 2^length(vars) - 1, sep = ''))))
    mall <- subset(mallow$cp,mallow$cp[, 1] == 1)
    Poss[i, 1] <- sum(fith[1:nrow(fith), 1]) - (fith[nrow(fith), 1])
    if (Poss[i,1] == 0) {Poss[i, 2] <- 0}
    Poss[i, 5] <- mall[i, (length(vars) + 2)]
    for (q in 1:length(vars)){
      if(q == 1){preds[i, ] <- mall[i, q + 1]}
      else{
        preds[i, ] <- paste(preds[i, ], mall[i, q + 1], sep = '')
      }
    }
    colnames(Poss) <- c("#preds", "Rsq", "Adj Rsq", "PRESS", "Mallow Cp")
    rownames(Poss) <- preds
    Poss <- round(Poss, digits = 3)
  }
  ## Creates Diagnositc plots for all possible models
  if(plots == TRUE){
    par(mfrow = c(2, 2))
    plot(Poss[, 1], Poss[, 2], xlab = "Number of Predictors", ylab = "R-squared", pch = 19)
    plot(Poss[, 1], Poss[, 3], xlab = "Number of Predictors", ylab = "Adjusted R-squared", pch = 19)
    plot(Poss[, 1], Poss[, 4], xlab = "Number of Predictors", ylab = "PRESS", pch = 19)
    plot(Poss[, 1], Poss[, 5], xlab = "Number of Predictors", ylab = "Mallow's Cp", pch = 19)
    }
  return(Poss)
}
