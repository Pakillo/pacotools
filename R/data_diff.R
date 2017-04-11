#' Diff two dataframes
#'
#' Compare two dataframes using \pkg{daff} package.
#' 
#' @param old.df Old dataframe
#' @param new.df New dataframe (to evaluate changes compared to old.df).
#' @param output character. \code{view} shows data diff as HTML (see \code{\link[daff]{render_diff}}). 
#' \code{data.frame} returns a data.frame (useful for knitting, see examples).
#' @param file Character. Path to save diff results in a HTML file (optional).
#' @return A \code{difference} object. See \code{\link[daff]{diff_data}}. Also,
#' optionally, an HTML file or data.frame.
#' @export
#' @importFrom daff diff_data render_diff write_diff
#'
#' @examples
#' old <- iris
#' new <- old
#' new[1, 2] <- 3.8
#' new <- new[- 4, ]
#' data_diff(old, new)
#' 
#' ## Diff two Excel files
#' # (or any two files that can be imported by \pkg{rio}) 
#' \dontrun{
#' library(rio)
#' old <- import("old.xls")
#' new <- import("new.xls")
#' data_diff(old, new)
#' 
#' # Including output in Rmarkdown document:
#' kable(data_diff(old, new, output = "data.frame"))
#' }
#' 
#' 
data_diff <- function(old.df, new.df, output = c('view', 'data.frame'), file = tempfile(fileext = ".html")){
  
  dif <- daff::diff_data(old.df, new.df)
  
  output <- match.arg(output)
  
  if (output == 'view') daff::render_diff(dif, file = file, view = TRUE)
  
  if (output == 'data.frame'){
    file = tempfile(fileext = ".csv")
    write_diff(dif, file)
    dif <- read.csv(file)
    names(dif)[1] <- "change"
  }
  
  invisible(dif)
}

