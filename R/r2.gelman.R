#'
#' Calculate R squared (R2) for a multilevel/hierarchical model following Gelman & Pardoe (2006)
#' 
#' This function calculates the R-squared of a multilevel/hierarchical model following Gelman & Pardoe (2006) (see \code{References}). 
#' 
#' 
#' @export
#' @return A numeric value: the r-squared
#' @param residuals A numeric matrix containing the residual values obtained through MCMC.
#' @param data.level Logical. If TRUE, calculates the R2 at the data level (level 1) and the denominator is the variance of the data
#' instead of variance of the parameters/linear predictors (see function definition for more details).
#' @param responsevar A numeric vector. The response variable used to fit the model. Only required for data-level calculations.
#' @param param.sims A numeric matrix containing the posterior samples for the random effects parameters.
#' @author Paco, based on code by Gelman & Hill (2007) (see \code{references}).
#' @references Gelman & Pardoe (2006) Technometrics.
#' Gelman & Hill (2007) book
#' @seealso \code{\link{r2.multilevel}} in this same package.
#' @seealso \code{\link{r.squaredGLMM}} in \code{MuMIn} package and \link{http://jslefche.wordpress.com/2013/03/13/r2-for-linear-mixed-effects-models/}
#' 



r2.gelman <- function(residuals, data.level, responsevar, param.sims){
  
  if (data.level == TRUE)
    r2 <- 1 - mean(apply(residuals, 1, var)) / var(responsevar)
  
  else
    r2 <- 1 - mean(apply(residuals, 1, var)) / mean(apply(param.sims, 1, var))
  
  return(r2)
  
}