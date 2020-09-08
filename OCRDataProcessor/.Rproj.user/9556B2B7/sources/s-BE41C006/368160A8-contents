#' Get all but first element for split columns
#'
#' @param x split column
#'
#' @return processed split column
#' @export
get_all_but_first <- function(x) {
  if (length(x) == 1) {
    NA
  } else {
    out <- x[2:length(x)]
    out <- out[out != '']
    paste(out, collapse = ' ') %>% 
      str_squish() %>% 
      removePunctuation()
  }
}