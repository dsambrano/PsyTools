#' @rdname HierRegression
#' @name Hier.reg
#' @title
#' Hierarchical Regression
#' @aliases
#' Hier.reg
#' @author
#' K. Preston and D. Sambrano
#' @description
#' This function produces the coeffiecients and the anova summary table
#'     for Hierarchical regression.
#' @usage Hier.reg(models=c(fit1, fit2, fit3))
#' @param model Must be a concatinated series of the models to be compared
#'     (e.g., use \code{c()}.
#' @return
#' Two matrices displaying (1) b coeffients at each level of the model
#'     (2) anova summary table with \eqn{R^2},, adj \eqn{R^2},, delta \eqn{R^2},, delta F-ratios,
#'     degrees of freedom, and p-values for each step of the model.
#' @examples
#' dv <- rnorm(500, 47, 12)
#' iv1 <- rnorm(500, 0, 6)
#' iv2 <- rnorm(500, 100, 5)
#' iv3 <- rnorm(500, 12, 3)
#' iv4 <- rnorm(500, 12, 100)
#' dataset <- data.frame(dv, iv1, iv2, iv3, iv4)
#' fit1 <- lm(dv ~ iv1, data = dataset)
#' fit2 <- lm(dv ~ iv1 + iv2, data = dataset)
#' fit3 <- lm(dv ~ iv1 + iv2 + iv3, data = dataset)
#' fit4 <- lm(dv ~ iv1 + iv2 + iv3 + iv4, data = dataset)
#' models <- c(fit1, fit2, fit3, fit4)
#' Hier.reg(models)
# Hierarchical Regression
Hier.reg <- function(models) {
  nblocks <- length(which(names(models)=="call"))
  formula.locations <- which(names(models)=="call")
  Hier <- matrix(0, nblocks, 7)
  b <- matrix(0, nblocks, length(models[formula.locations[length(formula.locations)] - 9]$coefficients) - 1)
  fitfull <- lm(models[formula.locations[length(formula.locations)]]$call$formula)
  for (i in 1:nblocks) {
    fit <- lm(models[formula.locations[i]]$call$formula)
    fith <- anova(fit)
    if(i != 1){
      aovfit <- anova(lm(models[formula.locations[i - 1]]$call$formula), lm(models[formula.locations[i]]$call$formula))
      }
      Hier[i, 1]<- sum(fith[1:(nrow(fith) - 1), 2]) / sum(fith[1:nrow(fith), 2])
      Hier[i, 2]<- 1 - ((fith[nrow(fith), 2]) / sum(fith[1:nrow(fith), 2])) * (sum(fith[1:nrow(fith), 1]) / (fith[nrow(fith), 1]))
    if(i == 1){
      Hier[i, 3] <- Hier[i, 1]
      Hier[i, 4] <- fith[1, 4]
      Hier[i, 5] <- fith[1, 1]
      Hier[i, 6] <- fith[2, 1]
      Hier[i, 7] <- fith[1, 5]
    } else {
      Hier[i, 3] <- Hier[i, 1] - Hier[(i - 1), 1]
      Hier[i, 4] <- aovfit[nrow(aovfit), 5]
      Hier[i, 5] <- aovfit[nrow(aovfit), 3]
      Hier[i, 6] <- aovfit[nrow(aovfit), 1]
      Hier[i, 7] <- aovfit[nrow(aovfit), 6]
    }
      colnames(Hier) <- c("Rsq Total", "Adj Rsq", "Rsq Change", "F Change", "df1", "df2", "p")
      Hier <- round(Hier, digits = 4)
      b[i, 1:length(coef(fit)) - 1] <- coef(fit)[2:length(coef(fit))]
      b <- round(b, digits = 4)
  }
  row.names(b) <- paste("Step", 1:nblocks)
  colnames(b) <- names(fitfull$coeff[2:(nblocks + 1)])
  row.names(Hier) <- paste("Step", 1:nblocks)
  return(list("Coefficients by Step" = b, "Anova Table by Step" = Hier))
}
