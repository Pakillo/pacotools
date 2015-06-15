#' Get path to local Dropbox folder.
#'
#' This function returns the full path to the Dropbox folder in the local system.
#'
#' @export
#' @importFrom stringr str_split
#' @return A character vector with the path to the Dropbox folder.

dropbox_path <- function() {
  
  # old version:
  #   if (Sys.info()["sysname"]!="Windows") stop("Currently, 'dropbox_path' works for Windows only. Sorry.")
  #   db.file <- paste(Sys.getenv('APPDATA'), '\\Dropbox\\host.db', sep='')
  #   base64coded <- readLines(db.file, warn=FALSE)[2]
  #   base64(base64coded, encode=FALSE)
  
  docs_path <- unlist(stringr::str_split(path.expand("~/"), "/"))
  home_path <- paste(docs_path[1:3], collapse = "/")
  dropbox_path <- paste(home_path, "Dropbox", sep = "/")
  dropbox_path
  
}
