#'@name yield
#'@rdname yields
#'
#'@title Increment an Iterator and Return the Next Value(s)
#'
#'@description Finds the value of the next iteration(s) of an Iterator object
#' and increments the Iterator to the next value(s).
#' @param iter An Iterator object
#' @param more How many values to yield
#' @return An object of whatever type `result` evaluates to from the Iterator, or
#' a vector of that type in the case of `yield_more(iter, more > 1L)`.
NULL


#'@rdname yields
#'@export
yield_next <- function(iter) {
  stopifnot(is_Iterator(iter))
  env <- rlang::env(parent.frame(), !!! iter$initial)
  yield_name <- as.character(iter$yield)

  for (j in seq_along(iter$result)) {
    eval(iter$result[[j]], envir = env)
  }

  iter$initial <- as.list(env, all.names = TRUE)

  return(iter$initial[[yield_name]])
}

#'@rdname yields
#'@export
yield_more <- function(iter, more = 1L) {
  vec <- vector(length = more)
  for (i in seq_len(more)) {
    vec[[i]] <- yield_next(iter)
  }
  vec
}
