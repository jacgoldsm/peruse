#' Create a Python-style range vector
#'
#' @param a minimum, integer
#' @param b supremum, integer
#' @examples
#' range(1,5)
#' range(9,10)
#' @export

range <- function(a,b) {
  a <- as.integer(a)
  b <- as.integer(b)
  if (a < b) return(a:(b-1)) else return(numeric())
}
