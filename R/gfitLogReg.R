#' Goodness of fit of a Logistic Regression
#' 
#' Make a goodness of fit plot (observed vs predicted values) in a logistic regression context. Optionally, a table with 
#' numerical results (including a chi-squared test) can be produced.
#' 
#' @export
#' @importFrom Hmisc cut2
#' @author Paco, based on function \code{plotCalibration} from package \code{PredictABEL}.
#' @return A goodness of fit plot. Optionally (if \code{print.table=T}), also a table. See \code{plotCalibration} from package \code{PredictABEL}.
#' @seealso \code{plotCalibration} from package \code{PredictABEL}
#' @param obs numeric vector with observations (either 0 or 1)
#' @param pred numeric vector (same length as obs) with fitted model probabilities
#' @param groups Number of groups (bins) to divide the data into. If missing, the default is 10.
#' @param print.table Logical (default is FALSE).
#' @param ...  other arguments (e.g. xlim, xlab, main, ...) to be passed to \code{plot}. Some default values have been chosen.
#' @examples
#' # Generate some data
#' x <- seq(1:1000)
#' yhat <- plogis(0.2 + 0.003*x)   # inverse logit
#' y <- rbinom(1000, 1, yhat)
#' 
#' # Fit model and get predicted probabilities
#' model <- glm(y~x, family="binomial")
#' ypred <- predict(model, type="response")
#' 
#' # Check goodness of fit
#' gfitLogReg(y, ypred)
#' gfitLogReg(y, ypred, print.table=TRUE)
#' gfitLogReg(y, ypred, xlim=c(0.5,1))
#' gfitLogReg(y, ypred, groups=5)

gfitLogReg <- function (obs, pred, groups, print.table=FALSE,
                        main="Goodness of fit", xlab="Observations", ylab="Predictions", xlim=c(0,1),
                        pch = 16, ps = 2, cex.lab = 1.2, cex.axis = 1.1, las = 1, ...) 
{ 
  # modified from PredictABEL::plotCalibration
  
  if (missing(groups)) {
    groups <- 10
  }

  p = pred
  y = obs
  if (length(unique(y)) != 2) {
    stop(" The specified outcome is not a binary variable.\n")
  }
  else {
    matres <- matrix(NA, nrow = groups, ncol = 5)
    sor <- order(p)
    p <- p[sor]
    y <- y[sor]
    groep <- cut2(p, g = groups)
    total <- tapply(y, groep, length)
    predicted <- round(tapply(p, groep, sum), 2)
    observed <- tapply(y, groep, sum)
    meanpred <- round(tapply(p, groep, mean), 3)
    meanobs <- round(tapply(y, groep, mean), 3)
    matres <- cbind(total, meanpred, meanobs, predicted, 
                    observed)
    plot(matres[, 3], matres[, 2], main=main, xlab = xlab, 
         ylab = ylab, pch = pch, ps = ps, xlim = xlim, 
         ylim = xlim, cex.lab = cex.lab, cex.axis = cex.axis, 
         las = las, ...)
    lines(x = c(0, 1), y = c(0, 1))
    
    if (print.table){
      contr <- ((observed - predicted)^2)/(total * meanpred * 
                                             (1 - meanpred))
      chisqr <- sum(contr)
      df <- (groups - 2)
      pval <- 1 - pchisq(chisqr, df)
      out <- list(Table_HLtest = matres, Chi_square = round(chisqr, 3), df = df, p_value = round(pval, 4))
      print(out)
    }

  }
}