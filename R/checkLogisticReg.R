#' Logistic regression model checks
#' 
#' \code{checkLogisticReg} performs several tests and plots of goodness of fit and residuals
#' of a logistic regression model
#' 
#' @export
#' @param obs numeric vector with observations (either 0 or 1)
#' @param pred numeric vector (same length as obs) with fitted model probabilities
#' @param covs Optional. A named list of one or more numeric vectors that were used as covariates in the model
#' @param axes.range range of the axes (xlim, ylim)
#' @param ... further arguments passed to \code{\link{plotCalibration}} function from package "PredictABEL"
#' @return several plots and quantities
#' @references Gelman & Hill (2007)
#' @seealso \code{\link{binnedplot}}, \code{\link{plotCalibration}}, \code{\link{performance}}

checkLogisticReg <- function(obs, pred, covs=NULL, axes.range=c(0,1), ...){
  
  if (any(is.na(obs))) stop("There are missing values in the observation vector")
  if (any(is.na(pred))) stop("There are missing values in the prediction vector")
  
  # calculate error rate (proportion of cases that actually did the opposite of the expected by the model)
  # based on Gelman & Hill (2007)
  error.rate <- mean((pred>0.5 & obs==0) | (pred<0.5 & obs==1))
  pred.null <- mean(obs)   # expected probability of the null model
  error.null <- mean((pred.null>0.5 & obs==0) | (pred.null<0.5 & obs==1))
  cat("\n The error rate is ", error.rate, "\n", 
    "The error rate of the null model is ", error.null, "\n \n")
  
  # AUC & ROC CURVES
  auc.pre <- prediction(pred, obs)
  roc <- performance(auc.pre, measure="tpr", x.measure="fpr")
  cat("PLOTTING THE ROC CURVE... \n")
  plot(roc)
  auc <- performance(auc.pre, "auc")
  cat("THE AUC IS ", print(auc@y.values[[1]]), "\n")
  
  
  
  # calculate residuals
  resid <- obs - pred
  
  # plot residuals vs fitted values
  # require(arm)
  cat("PLOTTING RESIDUALS VERSUS FITTED VALUES... \n")
  binnedplot(pred, resid)
  
  if (!is.null(covs)){
    cat("PLOTTING RESIDUALS VERSUS PREDICTORS... \n")
    for (i in 1:length(covs)){
      binnedplot(covs[[i]], resid, xlab=names(covs)[i], 
                 main="Predictor vs residuals")
    }    
  }
  
  # plot goodness of fit
  cat("PLOTTING GOODNESS OF FIT (OBSERVED VS PREDICTED) \n")
  plotCalibration(data=cbind(pred, obs), cOutcome=2, predRisk=pred, rangeaxis=axes.range, ...)
  
}