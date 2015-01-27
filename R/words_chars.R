#' Word and character counts
#'
#' Counts number of words and characters (with or without spaces) in a piece of text.
#' 
#' @rdname words_chars
#' @importFrom qdap wc character_count
#' @param text Text to count words or characters in. If missing, will read from the clipboard.
#' @param spaces Logical. Include spaces? Default is FALSE.
#' @export
#' @author F. Rodriguez-Sanchez.
#' @examples \dontrun{
#' # Copy text to clipboard and then run
#' words()
#' chars()
#' chars(spaces = TRUE)
#' 
#' # Or provide text:
#' words("This is just a trial")
#'}

words <- function(text){
  require(qdap)
  if (missing(text)) {
    text <- readLines("clipboard", warn = FALSE)  # read from clipboard
  }
  sum(wc(text), na.rm = TRUE)
}


#' @rdname words_chars
#' @export
chars <- function(text, spaces = FALSE){
  require(qdap)
  if (missing(text)) {
    text <- readLines("clipboard", warn = FALSE)  # read from clipboard
  }
  sum(character_count(text, count.space = spaces), na.rm = TRUE)
}
