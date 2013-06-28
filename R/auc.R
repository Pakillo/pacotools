#' AUC: Area Under the Curve of the Receiver Operating Curve
#' 
#' Estimates the AUC of the ROC using a Mann-Whitney U statistic. Excludes missing data, if any.
#' 
#' @export
#' @param obs a vector of observed values which must be 0 for absences and 1 for occurrences.
#' @param pred a vector of the same length as \code{obs} representing the predicted values. Values must be between 0 & 1.
#' @return Returns a single value representing the AUC value.
#' @author Jeremy VanDerWal
#' @seealso Package \code{SDMTools}
#' @examples
#' # create some data
#' obs = c(sample(c(0,1),20,replace=TRUE),NA)
#' pred = runif(length(obs),0,1)
#' 
#' # calculate AUC from the random data
#' auc(obs,pred)
#' 
#' # calculate an example 'perfect' AUC
#' obs = obs[order(obs)]
#' pred = pred[order(pred)]
#' auc(obs,pred)


auc <- function(obs,pred){
	#input checks
	if (length(obs)!=length(pred)) stop('this requires the same number of observed & predicted values')
	
	
	#deal with NAs
	if (length(which(is.na(c(obs,pred))))>0) {
		na = union(which(is.na(obs)),which(is.na(pred)))
		warning(length(na),' data points removed due to missing data')
		obs = obs[-na]; pred = pred[-na]
	}

	#define the n's and do checks
	n = length(obs); if (length(which(obs %in% c(0,1)))!=n) stop('observed values must be 0 or 1') #ensure observed are values 0 or 1
	n1 = length(which(obs==1)); n0 = length(which(obs==0))
	if (n1==0 || n1==n) return( NaN ) #if all observed 1's or 0's return NaN

	###calc AUC
	pred0 = pred[which(obs==0)]
	pred1 = pred[which(obs==1)]
	ranks = rank(pred,ties.method='average')#define ranks
	ranks0 = ranks[which(obs==0)]
	ranks1 = ranks[which(obs==1)]
	U = n0*n1 + (n0*(n0+1))/2 - sum(ranks0) #calc U stat
	AUC = U/(n0*n1) #estimate AUC
	if (AUC<.5) AUC = 1-AUC
	
	#return the auc value
	return(AUC)
}

