#' Including Rmd source in Rpubs
#' 
#' Including Rmd source in Rpubs
#' 
#' @export
#' @author Ramnath Vaidyanathan
#' @seealso http://rpubs.com/ramnathv/including_rmd_source

rpubs_rmd <- function(file){
  myrmd = base64enc::dataURI(file = file, mime = 'text/rmd')
  dl_link = "<a href='%s' target='_blank' download='mynb.Rmd'>
  <span class='glyphicon glyphicon-cloud-download' style='font-size:1.2em;'></span> 
  Download
  </a>"
  cat(sprintf(dl_link, myrmd))
}

