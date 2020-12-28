#' Get the current value of an Iterator without changing its state
#'
#' @description An `Iterator` `yield`s a variable every time `yield_next()` is called.
#' Get the current value of that variable without changing the state of the Iterator.
#'
#' @param iter An `Iterator` object
#'
#' @return The current value of `iter`
#' @export

current <- function(iter) {
  iter$initial[[as.character(iter$yield)]]
}
