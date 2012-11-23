#' Save plot as a pdf file with reduced margins (suitable for publication)
#' 
#' Save plot as a pdf file with reduced margins (suitable for publication)
#' 
#' @author Rob Hyndman
#' @export
#' @param file filename
#' @param width width in cm
#' @param height height in cm
#' @return a pdf file in the working directory

savepdfSmallMarg <- function(file, width=16, height=10)
{
  fname <- paste(file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}
