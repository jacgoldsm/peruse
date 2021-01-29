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
  #env <- rlang::env(parent.frame(), !!! iter$initial)
  env <- list2env(iter$initial, parent = parent.frame())

  yield_name <- as.character(iter$yield)

  for (j in seq_along(iter$result)) {
    eval(iter$result[[j]], envir = env)
  }

  iter$initial <- as.list(env, all.names = TRUE)
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
