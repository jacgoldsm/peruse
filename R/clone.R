#' clone
#'
#' Clone an Iterator, making an exact copy that can then be modified separately.
#' This is a simple wrapper around [rlang::env_clone()]. Includes the option to
#' clone the Iterator into a different environment.
#'
#' @param iter an `Iterator` object
#' @param parent the environment to put the `Iterator`
#' @return a copy of the `Iterator` passed as a parameter
#' @examples
#' it <- Iterator({m <- m + 1}, c(m = 0), m)
#' other <- clone(it)
#' yield_next(it)
#' current(other) == current(it) # false
#' @export

clone <- function(iter, parent = rlang::env_parent(iter)) {
  env <- rlang::env_clone(iter, parent)
  class(env) <- "Iterator"
  env
}
