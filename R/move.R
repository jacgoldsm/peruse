#'@name move
#'@rdname moves
#'
#'@title Increment an Iterator Without Returning the Value(s)
#'
#' @description Increments the Iterator without returning anything.
#' `move_more()` repeats `move_next()` a specified number of times. `move_while()`
#' repeats `move_next()` until a condition is met. Refer to the number of the current
#' iteration with `.iter`.
#'
#' @param iter An Iterator object object
#' @param more How many times to iterate
#' @param cond A quoted logical expression involving some variable(s) in `iter$initial`, so that `move_next()`
#' continues being called while the expression returns TRUE
#'
#' @examples
#'primes <- 2:10000 %>%
#'             that_for_all(range(2, .x)) %>%
#'             we_have(~.x %% .y != 0, "Iterator")
#'current(primes)
#'move_more(primes, 100)
#'current(primes)
#'
NULL


#'@rdname moves
#'@export
move_next <- function(iter) {
  yield_next(iter)
  invisible(NULL)
}

#'@rdname moves
#'@export
move_more <- function(iter, more = 1L) {
  yield_more(iter, more = more)
  invisible(NULL)
}

#'@rdname moves
#'@export
move_while <- function(iter, cond) {
  yield_while(iter, cond)
  invisible(NULL)
}


