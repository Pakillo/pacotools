#' Logistic regression model checks
#' 
#' \code{checkLogReg} is a wrapper of other functions (\code{\link{auc}, \link{binnedplot}, \link{gfitLogReg}}) to 
#' perform several tests and plots of goodness of fit and residuals from a logistic regression model
#' 
#' @export
#' @importFrom SDMTools auc
#' @importFrom arm binnedplot
#' @author Paco
#' @param obs numeric vector with observations (either 0 or 1)
#' @param pred numeric vector (same length as obs) with fitted model probabilities
#' @param covs Optional. A named list of one or more numeric vectors that were used as covariates in the model
#' @param ... further arguments passed to \code{\link{gfitLogReg}}, such as 
#' \code{groups} number of bins or 
#' \code{print.table} Logical. Print table with numeric results or just make a plot? Default is FALSE.
#' @return several plots and quantities
#' @references Gelman & Hill (2007)
#' @seealso \code{\link{binnedplot}}, \code{\link{plotCalibration}}, \code{\link{auc}}
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
#' checkLogReg(y, ypred)
#' checkLogReg(y, ypred, covs=list(predictor=x))

checkLogReg <- function(obs, pred, covs=NULL, ...){
  
  if (any(is.na(obs))) stop("There are missing values in the observation vector")
  if (any(is.na(pred))) stop("There are missing values in the prediction vector")
  
  # calculate error rate (proportion of cases that actually did the opposite of the expected by the model)
  # based on Gelman & Hill (2007)
  error.rate <- mean((pred>0.5 & obs==0) | (pred<0.5 & obs==1))
  pred.null <- mean(obs)   # expected probability of the null model
  error.null <- mean((pred.null>0.5 & obs==0) | (pred.null<0.5 & obs==1))
  cat("\n The error rate (as defined in Gelman & Hill 2007, p. 99) is ", error.rate, "\n", 
    "The error rate of the null model is ", error.null, "\n \n")
  
  # AUC & ROC CURVES
  cat("THE AUC IS ", SDMTools::auc(obs, pred), "\n")
  
  # calculate residuals
  resid <- obs - pred
  
  # plot residuals vs fitted values
  # require(arm)
  cat("PLOTTING RESIDUALS VERSUS FITTED VALUES... \n")
  arm::binnedplot(pred, resid)
  
  if (!is.null(covs)){
    cat("PLOTTING RESIDUALS VERSUS PREDICTORS... \n")
    for (i in 1:length(covs)){
      arm::binnedplot(covs[[i]], resid, xlab=names(covs)[i], 
                 main="Predictor vs residuals")
    }    
  }
  
  # plot goodness of fit
  cat("PLOTTING GOODNESS OF FIT (OBSERVED VS PREDICTED)... \n")
  #plotCalibration(data=cbind(pred, obs), cOutcome=2, predRisk=pred, rangeaxis=axes.range, ...)
  gfitLogReg(obs, pred, ...)
  
}