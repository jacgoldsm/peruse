#'@name move
#'@rdname moves
#'
#'@title Increment an Iterator without returning the Value(s)
#'
#'@description Increments the Iterator without returning anything.
#' @param iter An Iterator object object
#' @param more How many times to iterate
NULL


#'@rdname moves
#'@export
move_next <- function(iter) {
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
  invisible(NULL)
}

#'@rdname moves
#'@export
move_more <- function(iter, more = 1L) {
  vec <- vector(length = more)
  for (i in seq_len(more)) {
    vec[[i]] <- yield_next(iter)
  }
  invisible(NULL)
}
