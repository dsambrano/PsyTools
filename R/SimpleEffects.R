#' @rdname SimpleEffects
#' @title
#' Simple Effects Analyses
#' @aliases
#' Simple.Effects
#' @author
#' K. Preston and D. Sambrano
#' @description
#' This function takes a model of the lm class with two IVs and
#' produces A matrix with the simple effects F-ratios, degrees of freedom,
#' and p-values. Currently limited to only 2 factor designs.
#' @usage Simple.Effects(model = lm(dv ~ iv1 * iv2,
#'                                  data = dataset))
#' @param model An object of the lm class.
#' @return
#' A matrix displaying simple effects F-ratios, degrees of freedom, and p-values
#' @examples
#' dv <- rnorm(100, 50, 10)
#' iv1 <- factor(rep(1:2, each = 50))
#' iv2 <- factor(rep(1:5, 20))
#' data <- data.frame(dv, iv1, iv2)
#' model <- lm(dv ~ iv1 * iv2, data)
#' anova(model)
#' Simple.Effects(model)


# For a single cat variable
# data <- model$model
# vars <- names(data)
# fvars <- matrix(0, 1, ncol(data))
# for (j in 2:length(vars)){
#   x <- data[,vars[j]]
#   fvars[j] <- is.factor(x)
#   colnames(fvars) <- vars
# }
# fac.var <- data[,which(fvars == 1)]
#
# simp <- matrix(0,length(iv2levels),4)
# for(k in 1:(length(iv2levels))){
#   mydata <- subset(dataset, dataset[,iv2col] == iv2levels[k])
#   simpfit <- anova(lm(mydata[,dvcol] ~ mydata[,iv1col], mydata))
#   simp[k,3] <- simpfit[1,3]/fitfull[(nrow(fitfull)),3]
#   simp[k,4] <- 1-pf(simp[k,3],simpfit[1,1],fitfull[nrow(fitfull),1])
#   simp[k,1] <- simpfit[1,1]
#   simp[k,2] <- fitfull[nrow(fitfull),1]}
# rownames(simp) <- c(iv2levels)
# colnames(simp) <- c("df1","df2","F","p-value")
# round(simp,digits=3)


# Simple Effects
Simple.Effects <- function(model) {
  if (any(class(model) == "lm")) {fitfull <- anova(model)}
  else {warning("Model muse be lm class (e.g., lm(dv ~ iv1, iv2, data)")}
  dv <- names(model$model)[1] #column number of the DV
  iv1 <- names(model$model)[2] #column number of the first IV which(unlist(test) %in% "$"==TRUE); test <- strsplit(iv1, "")
  iv2 <- names(model$model)[3] #column number of the second IV
  dataset <- data.frame(dv = model$model[, 1], iv1 = model$model[, 2], iv2 = model$model[, 3])
  simp <- matrix(0, (length(levels(dataset$iv1)) + length(levels(dataset$iv2))), 4)
  for(k in 1:(length(levels(dataset$iv1)))){
    sub <- dataset$iv1 == levels(dataset$iv1)[k]
    mydata <- subset(dataset, sub)
    simpfit <- anova(lm(mydata$dv ~ mydata$iv2, mydata))
    simp[k, 1] <- simpfit[1, 3] / fitfull[(nrow(fitfull)), 3]
    simp[k, 2] <- simpfit[1, 1]
    simp[k, 3] <- fitfull[nrow(fitfull), 1]
    simp[k, 4] <- pf(simp[k, 1], simpfit[1, 1] ,fitfull[nrow(fitfull),1], lower.tail = FALSE)}
  for(k in (length(levels(dataset$iv1)) + 1):(length(levels(dataset$iv1)) + length(levels(dataset$iv2)))){
    mydata <- subset(dataset, dataset$iv2 == levels(dataset$iv2)[(k - length(levels(dataset$iv1)))])
    simpfit <- anova(lm(mydata$dv ~ mydata$iv1, mydata))
    simp[k, 1] <- simpfit[1, 3] / fitfull[(nrow(fitfull)),3]
    simp[k, 2] <- simpfit[1, 1]
    simp[k, 3] <- fitfull[nrow(fitfull), 1]
    simp[k, 4] <- pf(simp[k, 1],simpfit[1, 1], fitfull[nrow(fitfull), 1], lower.tail = FALSE)}
  rownames(simp) <- c(levels(dataset$iv1), levels(dataset$iv2))
  colnames(simp) <- c("F", "df1", "df2", "p-value")
  round(simp, digits = 3)
}
