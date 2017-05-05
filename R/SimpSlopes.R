#' @rdname SimpSlopes
#' @name Simple.Slopes
#' @title
#' Simple Slopes
#' @aliases
#' Simple.Slopes
#' @author
#' K. Preston and D. Sambrano
#' @description
#' Produces simple slope coefficients and plots for two predictor models.
#'     Great for visualizing interactions of continuous data.
#' @usage Simple.Slopes(model, z.score = FALSE, plot = TRUE,
#'               plot.colors = c("salmon", "plum4", "steelblue2"), line.width = 2,
#'               line.type = 1:3, sym = c(15, 17, 19), plot.title = "",
#'               legend = TRUE, legend.levels = c("high", "middle", "low"),
#'               legend.location = "topright"))
#' @param model An object of the lm class.
#' @param z.score A logical statement indicating whether you want complete centering
#'     (e.g., both mean and sd) or not, default is \code{FALSE}.
#' @param plot A logical statement indicating whether you want to produce the
#'     interaction plots, default is \code{TRUE}.
#' @param plot.colors A vector of colors of the plot, requires three.
#' @param line.width Width of the plot lines.
#' @param line.type a vector of the 3 lines produced, requires three values.
#' @param sym The symybol at points on the graph. 100 for no symbol.
#' @param plot.title The title of the plot
#' @param legend a logical vector indicating whether you want a legend for the
#'     plot, default is \code{TRUE}.
#' @param legend.levels a vector of the legend labels.
#' @param legend.location The location of the legend, default is topright.
#' @return
#' Returns a list of coeffiecients for plus or minus one standard deviation from the mean
#'     of each independent variable (IV) and displays an plot. Useful for displaying
#'     interactions of continuous data. The first IV in the model will be plot as the
#'     x-axis and the second IV will be the different lines.
#' @examples
#' dv <- rnorm(500, 47, 12)
#' iv1 <- rnorm(500, 0, 6)
#' iv2 <- rnorm(500, 100, 5)
#' dataset <- data.frame(dv, iv1, iv2)
#' model <- lm(dv ~ iv1 * iv2, data = dataset)
#' Simple.Slopes(model)
# Simple Slopes
Simple.Slopes <- function(model, z.score = FALSE, plot = TRUE,
                          plot.colors = c("salmon", "plum4", "steelblue2"), line.width = 2,
                          line.type = 1:3, sym = c(15, 17, 19), plot.title = "",
                          legend = TRUE, legend.levels = c("high", "middle", "low"),
                          legend.location = "topright") {


  if(z.score == FALSE) {
    data <- data.frame(dv = model$model[, 1],
                       scale(model$model[, 2:ncol(model$model)]))
  } else {
    data <- data.frame(dv = model$model[, 1],
                       center(model$model[, 2:ncol(model$model)]))
    }
  Y <- data[, 1]
  CX <- data[, 2]
  CZ <- data[, 3]
  fit <- lm(Y ~ CX*CZ)
  model_sum <- summary(fit)


  ##Simple slopes for X on Y at values of Z
  cfit <-as.matrix(coefficients(fit))
  vfit <- as.matrix(vcov(fit))
  vfit <- vfit[2:nrow(vfit), 2:ncol(vfit)]
  zhigh <- mean(CZ) + sd(CZ)
  zmean <- mean(CZ)
  zlow <- mean(CZ) - sd(CZ)
  xhigh <-mean(CX) + sd(CX)
  xmean <- mean(CX)
  xlow <- mean(CX) - sd(CX)

  zlowslope <- (cfit[2] + cfit[4] * zlow)
  zmeanslope <- (cfit[2] + cfit[4] * zmean)
  zhighslope <- (cfit[2] + cfit[4] * zhigh)
  zlowint <- (cfit[1] + cfit[3] * zlow)
  zmeanint <- (cfit[1] + cfit[3] * zmean)
  zhighint <- (cfit[1] + cfit[3] * zhigh)
  model_coef <-rbind(paste("high: Y=", round(zhighint, 3), "+", round(zhighslope, 3),
                           "*", names(data)[2]),
                     paste("mean: Y=", round(zmeanint, 3), "+", round(zmeanslope, 3),
                           "*", names(data)[2]),
                     paste("low: Y=" , round(zlowint, 3) , "+", round(zlowslope, 3) ,
                           "*", names(data)[2]))

  round(vfit, 3)
  (sehigh <-sqrt(vfit[1, 1] + 2 * zhigh * vfit[3, 1] + (zhigh^2) * vfit[3, 3]))
  (semean <-sqrt(vfit[1, 1] + 2 * zmean * vfit[3, 1] + (zmean^2) * vfit[3, 3]))
  (selow  <-sqrt(vfit[1, 1] + 2 * zlow  * vfit[3, 1] + (zlow^2)  * vfit[3, 3]))

  (thigh <- highslope / sehigh)
  round(2 * (1 - pt(thigh, (nrow(data) - 4))), 3)
  (tmean <- meanslope / semean)
  round(2 * (1 - pt(tmean,( nrow(data) - 4))), 3)
  (tlow<-lowslope / selow)
  round(2 * (1 - pt(tlow , (nrow(data) - 4))), 3)

  #R Code to Plot Simple Slopes with a Continuous Y Variable

  if (plot == TRUE) {
    YzlowXmin = (cfit[2] + cfit[4]*zlow)*xlow + (cfit[1] + cfit[3]*zlow)
    YzlowXmax = (cfit[2] + cfit[4]*zlow)*xhigh + (cfit[1] + cfit[3]*zlow)
    YzmeanXmin = (cfit[2] + cfit[4]*zmean)*xlow + (cfit[1] + cfit[3]*zmean)
    YzmeanXmax = (cfit[2] + cfit[4]*zmean)*xhigh + (cfit[1] + cfit[3]*zmean)
    YzhighXmin = (cfit[2] + cfit[4]*zhigh)*xlow + (cfit[1] + cfit[3]*zhigh)
    YzhighXmax = (cfit[2] + cfit[4]*zhigh)*xhigh + (cfit[1] + cfit[3]*zhigh)

    Ymin = min(YzlowXmin,YzlowXmax,YzmeanXmin,YzmeanXmax,YzhighXmin,YzhighXmax)
    Ymax = max(YzlowXmin,YzlowXmax,YzmeanXmin,YzmeanXmax,YzhighXmin,YzhighXmax)

    x <- c(min(xlow),max(xhigh))
    y <- c(Ymin,Ymax)
    matplot(x, y, type='n', xlab='x', ylab='y',
            xaxp=c(xlow,xhigh,2), main=plot.title)

    y1 <- c(YzlowXmin, YzlowXmax)
    y2 <-c(YzmeanXmin, YzmeanXmax)
    y3 <- c(YzhighXmin, YzhighXmax)

    colors <- plot.colors
    lines(x,y1, type='o', lty = line.type[1], pch = sym[1], lwd = line.width, col = colors[1])
    lines(x,y2, type='o', lty = line.type[2], pch = sym[2], lwd = line.width, col = colors[2])
    lines(x,y3, type='o', lty = line.type[3], pch = sym[3], lwd = line.width, col = colors[3])

    # Adds the legend
    if (legend == TRUE){
      location <- legend.location
      if (missing(legend.levels)) {
        legend.levels <- paste(names(data)[3], c("high", "middle", "low"), sep=" ")
      }
      legend(location, legend = legend.levels,
              col = colors, lty = line.type, lwd = c(1,1), bty = 'n',
              inset = 0.05, cex = 0.9)
    }
  }
  ### Tests of Simple Slopes via Recentering
  highZ <- CZ - (mean(CZ)+sd(CZ))
  meanZ <- CZ - mean(CZ)
  lowZ <- CZ - (mean(CZ)-sd(CZ))

  highfit <- lm(Y~CX*highZ)
  highcoe <- summary(highfit)$coefficients
  meanfit <- lm(Y~CX*meanZ)
  meancoe <- summary(meanfit)$coefficients
  lowfit <- lm(Y~CX*lowZ)
  lowcoe  <- summary(lowfit)$coefficients

  return(list("Centered or Z-scored Model" = summary(model), "High Coefficients" = highcoe,
              "Middle  Coefficients" = meancoe, "Low  Coefficients" = lowcoe))
}
