#' Increment an Iterator and Return the Next Value
#'
#' Finds the value of the next iteration of an Iterator object
#' and increments the Iterator to the next value.
#' @param iter An Iterator object object
#' @return An object of whatever type 'result' evaluates to from the Iterator
#' @export


yield_next <- function(iter) {
  stopifnot(is_Iterator(iter))
  iter_name <- deparse(substitute(iter))
  yield_name <- as.character(iter$yield)

  for (j in seq_along(iter$result)) {
    eval(iter$result[[j]], envir = iter$initial)
  }

  for (key in names(iter$initial)) {
    iter$initial[[key]] <- eval(rlang::parse_expr(key), envir = iter$initial)
  }
  assign(iter_name, iter, pos = .yieldenv)
  return(iter$initial[[yield_name]])
}
