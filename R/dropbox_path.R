#' Get path to local Dropbox folder.
#'
#' This function returns the full path to the Dropbox folder in the local system.
#'
#' @export
#' @return A character vector with the path to the Dropbox folder.

dropbox_path <- function() {
  
  docs_path <- unlist(strsplit(path.expand("~/"), "/"))
  home_path <- paste(docs_path[1:3], collapse = "/")
  dropbox_path <- paste(home_path, "Dropbox", sep = "/")
  dropbox_path
  
}
