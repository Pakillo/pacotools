#' Move the columns of a dataframe relative to each other.
#'
#' Move the columns of a dataframe relative to each other.
#' 
#' @param x a dataframe.
#' @param who A character vector of column names to move, or a logical vector of length names(x), or a vector of indices
#' @param after the column after which to put 'who': may be character, integer, NA, or NULL.
#' @details If \code{after} is \code{NA}, the named columns are moved to the front (before the first column). If \code{after} has length zero, or is a number less than zero or an integer greater than the number of remaining columns, or is NULL, \code{who} is moved to the end. If \code{after} is an integer less than or equal to the number of remaining columns, \code{who} is placed after the corresponding column.
#' @export
#' @author Tim Bergsma.
#' @note This function has been taken from archived package \code{metrumrg} where it was named \code{shuffle}.
#' @seealso \link{https://github.com/cran/metrumrg/blob/master/R/shuffle.R}.
#' @return A data frame with shuffled columns.
#' @examples \dontrun{
#' data(iris)
#' iris.new <- move_columns(iris, "Species", after = NA) # move species to first column
#'}
move_columns <- function(x, who, after = NA){
  
  who <- names(x[, who, drop = FALSE])
  nms <- names(x)[!names(x) %in% who]
  if (is.null(after))
    after <- length(nms)
  if (is.na(after))
    after <- 0
  if (length(after) == 0)
    after <- length(nms)
  if (is.character(after))
    after <- match(after, nms, nomatch = 0)
  if (after < 0)
    after <- length(nms)
  if (after > length(nms))
    after <- length(nms)
  nms <- append(nms, who, after = after)
  x[nms]
}
