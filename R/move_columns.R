#' Move the columns of a dataframe relative to each other.
#'
#' Move the columns of a dataframe relative to each other.
#'
#' @param x a dataframe.
#' @param who A character vector of column names to move, or a logical vector of length names(x), or a vector of indices
#' @param before the column before which to put \code{who}: may be character, numeric or NA. If NA, the named columns are moved to the front (before the first column).
#' @param after the column after which to put \code{who}: may be character, integer, or NA. If NA, the named columns are moved to the end (after the last column).
#' @export
#' @note This function has been modified from archived package \code{metrumrg} where it was named \code{shuffle}.
#' @seealso \link{https://github.com/cran/metrumrg/blob/master/R/shuffle.R}.
#' @return A data frame with shuffled columns.
#' @examples \dontrun{
#' data(iris)
#' iris.new <- move_columns(iris, "Species", after = NA) # move species to last column
#' iris.new <- move_columns(iris, "Species", after = 2)
#' iris.new <- move_columns(iris, "Species", after = "Sepal.Length")
#' iris.new <- move_columns(iris, "Species", before = NA) # move species to first column
#' iris.new <- move_columns(iris, "Species", before = 3)
#' iris.new <- move_columns(iris, "Species", before = "Petal.Length")
#'}
move_columns <- function(x, who, before = NULL, after = NULL){
  
  if (!is.null(after) & !is.null(before)) stop("Please provide only 'before' or 'after', not both.")
  
  who <- names(x[, who, drop = FALSE])
  nms <- names(x)[!names(x) %in% who]
  
  
  if (!is.null(after)) {
    
    if (is.na(after))
      after <- length(nms)
    if (is.character(after))
      after <- match(after, nms, nomatch = 0)
    if (is.numeric(after) & after < 1) stop("'after' cannot be < 1")
    if (is.numeric(after) & after > length(nms)) {
      after <- length(nms)
      message("'after' > number of columns. Moving to last column.")
    }
    
    
  }
  
  
  # using before
  if (!is.null(before)) {
    
    if (is.na(before))
      after <- 0
    if (is.character(before))
      after <- match(before, nms, nomatch = 0) - 1
    if (is.numeric(before) & before < 1) stop("'before' cannot be < 1")
    if (is.numeric(before))
      after <- before - 1
    
  }
  
  
  nms <- append(nms, who, after = after)
  x[nms]
  
  
}