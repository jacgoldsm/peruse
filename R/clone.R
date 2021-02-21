#' clone
#'
#' Clone an Iterator, making an exact copy that can then be modified separately.
#' This is a simple wrapper around [rlang::env_clone()]. Optionally,
#' override old initial parameters.
#'
#' @param iter an `Iterator` object
#' @param ... optionally override the `$initial` parameters in `iter`
#' @return a copy of the `Iterator` passed as a parameter
#' @examples
#' it <- Iterator({m <- m + n}, list(m = 0, n = 1), m)
#' other <- clone(it)
#' yield_next(it)
#' current(other) == current(it) # false
#'
#' it2 <- clone(other, n = 5)
#' yield_next(it2)
#' it2$initial$n  # 5
#' @export

clone <- function(iter, ...) {
  env <- rlang::env_clone(iter)
  env$initial <- as.list(rlang::dots_list(!!! iter$initial, ...,
                                         .homonyms = "last",
                                         .named = TRUE,
                                         .check_assign = TRUE))
  class(env) <- "Iterator"
  env
}
