#'@name yield
#'@rdname yields
#'
#'@title Increment an Iterator and Return the Next Value(s)
#'
#'@description Finds the value of the next iteration(s) of an Iterator object
#' and increments the Iterator to the next value(s). `yield_more()` repeats
#' `yield_next()` a specified number of times.
#' Refer to the number of the current iteration in `yield_more()` with `.iter`.
#'
#' @param iter An Iterator object
#' @param more How many values to yield
#' @return An object of whatever type `result` evaluates to from the Iterator, or
#' a vector of that type in the case of `yield_more(iter, more > 1L)`.
#'
#'@examples
#'primes <- 2:10000 %>%
#'          that_for_all(range(2, .x)) %>%
#'          we_have(~.x %% .y != 0, "Iterator")
#'
#'sequence <- yield_more(primes, 100)
#'
#'# use `.iter` to reference the current iteration
#'rwd <- Iterator({
#'         set.seed(seeds[.iter])
#'         n <- n + sample(c(-1L, 1L), size = 1L, prob = c(0.25, 0.75))
#'        },
#'        initial = list(n = 0, seeds = 1:100),
#'        yield = n)
#'
#'yield_more(rwd, 100)
NULL


#'@rdname yields
#'@export
yield_next <- function(iter) {
  stopifnot(is_Iterator(iter))
  env <- list2env(iter$initial, parent = rlang::caller_env())

  eval(iter$result, envir = env)

  iter$initial <- as.list(env, all.names = TRUE)

  return(iter$initial[[iter$yield]])
}

#'@rdname yields
#'@export
yield_more <- function(iter, more = 1L) {
  stopifnot(is_Iterator(iter))
  vec <- vector(length = more)
  env <- list2env(iter$initial, parent = rlang::caller_env())
  env$.iter <- 1L

    for (i in seq_len(more)) {
      vec[[i]] <- yield_next_from_helper(iter, env)
      env$.iter <- env$.iter + 1L
    }
  iter$initial <- as.list(env, all.names = TRUE)
  iter$initial <- within(iter$initial, rm(.iter))
  return(vec)
}

yield_next_from_helper <- function(iter, env) {

  eval(iter$result, envir = env)

  return(env[[iter$yield]])
}
