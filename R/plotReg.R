#' Plot regression
#' 
#' Visualise fitted regression and data with uncertainty (either a polygon or spaghetti plot)
#' 
#' @export
#' @return A plot
#' @param xdata The predictor. A numeric vector.
#' @param ydata The response variable. A numeric vector (can be a binary variable).
#' @param ... Further parameters to pass to plot. See \code{\link{par}}
#' @param plot.unc Logical. Plot uncertainty or just the average fitted response?
#' @param spag Logical. Make spaghetti plot? If False, and plot.unc=T, a shaded polygon is drawn instead
#' @param betamat A numeric matrix of nsims x nparams dimensions with the posterior values of the model parameters, e.g.
#' as retrieved from model$BUGSoutput$sims.matrix[,c("intercept", "slope")]
#' @param predmat A numeric matrix with ncol=nparams, containing the values of the covariates that should be used for the plot,
#' including the intercept. The order of the covariate columns should be the same as the parameter columns in betamat.
#' @param predcol A number telling which column in predmat represents the covariate to plot.
#' @param alpha.spag Alpha multiplier. A number between 0 and 1 (default=1) to adjust the transparency of the uncertainty lines in the spaghetti plot.
#' @param lwd.spag Line width (lwd) of every line in the spaghetti plot. Together with alpha.spag, define the degree of shadeness of the spaghetty plot
#' @param alpha.polyg Alpha parameter (values between 0 and 255) for setting the transparency of the shaded polygon.
#' @param colour Colour of the regression lines. If missing, it is taken from palette "Set1" in RColorBrewer.
#' @param add Logical. Add another regression line to an existing plot?
#' @author Paco, based on code by Lucas Leeman (see \link{http://andrewgelman.com/2012/08/graphs-showing-regression-uncertainty-the-code/})
#'
#'
#' 
#' @examples
#' 
#' ### LINEAR REGRESSION ###
#' 
#' # First generate some data
#' interc <- 0.6
#' slope <- 0.2
#' sigma <- 0.1
#' x <- seq(from=1, to=10, length.out=100)
#' y <- rnorm(100, interc + slope*x, sigma)
#' 
#' xdata <- x
#' ydata <- y
#' nsims <- 200
#' interc.post <- rnorm(nsims, interc, 0.05)
#' slope.post <- rnorm(nsims, slope, 0.01)
#' betamat <- cbind(interc.post, slope.post)
#' predmat <- cbind(rep(1,length(xdata)), xdata)
#' 
#' #plotReg(xdata, ydata, betamat, predmat)
#' #plotReg(xdata, ydata, betamat, predmat, spag=F)
#' #plotReg(xdata, ydata, betamat, predmat, plot.unc=F)
#' #plotReg(xdata, ydata, betamat, predmat, main="My regression", xlab="My x")    # including graphical parameters
#' #plotReg(xdata, ydata, betamat, predmat, colour="green")    # specifying colour
#' 
#' ## Add another regression lines to the plot
#' 

#' ### LOGISTIC REGRESSION ###
#' # Generate some data
#' x <- seq(1:1000)
#' yhat <- plogis(0.2 + 0.003*x)   # inverse logit
#' y <- rbinom(1000, 1, yhat)
#' 
#' # Fit model and get predicted probabilities
#' model <- glm(y~x, family="binomial")
#' ypred <- predict(model, type="response")






plotReg <- function(xdata, ydata, betamat, predmat, predcol, plot.unc=T, spag=T, colour, alpha.spag=1, lwd.spag=0.1, 
                    alpha.polyg=80, add=F, ...){
  
  if (add==F){
    ### Plot the data first
    plot(xdata,
         ydata,
         las=1, lwd=2, pch=20, cex=1, cex.axis=1.2, cex.lab=1.2, ...)
  }

  #predvals <- seq(min(xdata), max(xdata), length.out=100)
  predvals <- predmat[,predcol]
  
  ### Set colours for regression lines
  #require(RColorBrewer)
  if (missing(colour)){
    col.palette <- brewer.pal(9, "Set1")
    colour = col.palette[2]
    if (add==T){
      colour = col.palette[1]
    }
  }

  
  nsims = nrow(betamat)
  
  fittedy <- betamat %*% t(predmat)   # Modelled response values
  if (length(unique(na.omit(ydata)))==2){  # if the response is a binary variable (ie. logistic regression) 
    fittedy <- plogis(fittedy)     # take the inverse logit
  }
  
  
  # Plot uncertainty?
  
  if (plot.unc==T){    

    if (spag == T){     
      # SPAGHETTI PLOT
      #ysort <- t(apply(fittedy, 1, sort, decreasing=T))
      ysort <- fittedy[order(fittedy[,1], decreasing=T), ]
      lb <- nsims * 0.025
      ub <- nsims * 0.975
      
      for (i in lb:ub){
        lines(predvals, ysort[i,], col=rgb(t(col2rgb(colour)), alpha=alpha.spag*255*I(1/sqrt(nsims)), maxColorValue=255),
              lwd=lwd.spag)
        # alternatively, alpha = (255-abs(round(dim(y.pred)[1]/2)-i))/9
      }
      
    } else {    
      # Shaded Polygon plot
      # calculate quantiles 
      q2.5 <- apply(fittedy, 2, quantile, 0.025)
      q97.5 <- apply(fittedy, 2, quantile, 0.975)
      
      # draw quantiles and shaded polygons
      lines(predvals, q2.5, lty=2, col=colour)
      lines(predvals, q97.5, lty=2, col=colour)

      polygon(c(predvals,rev(predvals)), c(q2.5, rev(q97.5)), border=NA, 
              col=rgb(t(col2rgb(colour)), alpha=alpha.polyg, maxColorValue=255))
    }
    
  } 
  
  
  ### Finally, plot average fitted responses
  
  lines(predvals, colMeans(fittedy), lwd=3, col=colour)
  
}


## TODO:
# option to plot data
# option plot rug
# predvals must actually be a column of the predmat
# change cex to 1 in plot data
# and allow to change graphical parameters from the function call
# Add code to run directly from lm, glm or lmer objects


