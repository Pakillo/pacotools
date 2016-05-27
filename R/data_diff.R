#' Diff two dataframes
#'
#' Compare two dataframes using \pkg{daff} package.
#' 
#' @param old.df Old dataframe
#' @param new.df New dataframe (to evaluate changes compared to old.df).
#' @param show.diff Logical. If TRUE (default), show data diff as HTML. 
#' See \code{\link[daff]{render_diff}} for more details.
#' @param file Character. Path to save diff results in a HTML file (optional).
#'
#' @return A \code{difference} object. See \code{\link[daff]{diff_data}}. Also,
#' optionally, an HTML file.
#' @export
#' @importFrom daff diff_data render_diff
#'
#' @examples
#' old <- iris
#' new <- old
#' new[1, 2] <- 3.8
#' new <- new[- 4, ]
#' data_diff(old, new)
#' 
data_diff <- function(old.df, new.df, show.diff = TRUE, file = tempfile(fileext = ".html")){
  
  dif <- daff::diff_data(old.df, new.df)
  
  if (show.diff) daff::render_diff(dif, file = file)
  
  invisible(dif)
}

