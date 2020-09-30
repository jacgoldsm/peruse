#' Use an approximation of Python's set builder
#' Notation in R, based around the magrittr pipe
#' @param a desc
#' @param b desc
#' @example
#' range(1,4)
#' @export

range <- function(a,b) {
  a <- as.integer(a)
  b <- as.integer(b)
  if (a < b) return(a:(b-1)) else return(numeric())
}
