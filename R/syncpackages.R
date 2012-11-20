#' Synchronise installed packages between different computers
#' 
#' This function takes a list of installed packages (e.g. from a backup folder)
#' and check that all those packages are installed and updated in this computer.
#'
#' 
#' @param path folder path containing R packages (e.g. "E:/backup.XPS8300/")
#' @return New packages will be installed and updated
#' @export
#' @author Adapted from function \code{instant_pkgs} by Kay Cichini


syncpackages <- function(path){
  
  pkgs <- list.files(paste(path,"R/win-library/2.15/", sep=""))   
  
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss, dependencies=T)
  }
  update.packages(checkBuilt=T, ask=F)
}
