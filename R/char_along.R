#' Simple wrapper to aid in iterating `iterstring`s
#'
#' `char_along(string)` simply means `seq_len(nchar(string))`
#'
#' @param string a string to create an integer sequence from
#' @export

char_along <- function(string) {
  return(seq_len(nchar(string)))
}
