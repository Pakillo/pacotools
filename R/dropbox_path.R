#' Get path to local Dropbox folder.
#'
#' This function returns the full path to the Dropbox folder in the local system. Currently it works for Windows only.
#' 
#' @importFrom RCurl base64
#' @export
#' @return A character vector with the path to the Dropbox folder.
#' @author Petr Simecek
#' @seealso \link{http://applyr.blogspot.com.es/2012/08/get-path-to-your-dropbox-folder.html}.
dropbox_path <- function() {
  if (Sys.info()["sysname"]!="Windows") stop("Currently, 'dropbox_path' works for Windows only. Sorry.")
  db.file <- paste(Sys.getenv('APPDATA'), '\\Dropbox\\host.db', sep='')
  base64coded <- readLines(db.file, warn=FALSE)[2]
  base64(base64coded, encode=FALSE)
} 
