#' Equivalent to Python's 'next'
#'
#' Finds the value of the next iteration of an Iterator object
#' If argument is list, attempts to coerce to Iterator.
#' @param iter An Iterator object object
#' @return An object of whatever type 'result' evaluates to from the Iterator
#' @export



yield_next <- function(iter) {
  e1 <- environment()
  if (!is_Iterator(iter)) iter <- as_Iterator(iter)
  iter_name <- deparse(substitute(iter))
  yield_name <- as.character(iter$yield)
  list2env(iter$initial, envir = e1)

  for (j in 1:length(iter$result)) {
    eval(iter$result[[j]], envir = e1)
  }

  for (key in names(iter$initial)) {
    iter$initial[key] <- eval(rlang::parse_expr(key), envir = e1)
  }
  #pushes the local copy of 'gen' into the parent environment
  assign(iter_name, iter, pos = parent.frame(n = 1))
  return(iter$initial[[yield_name]])
}
