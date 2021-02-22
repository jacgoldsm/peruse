#'@name yield
#'@rdname yields
#'
#'@rawNamespace import(rlang, except = set_names)
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
  iter$initial$.iter <- 1L
  env <- list2env(iter$initial, parent = caller_env())

  eval_bare(iter$result, env = env)

  iter$initial <- as.list(env, all.names = TRUE)
  iter$initial <- within(iter$initial, rm(.iter))

  return(iter$initial[[iter$yield]])
}

#'@rdname yields
#'@export
yield_more <- function(iter, more = 1L) {
  stopifnot(is_Iterator(iter))
  vec <- vector(length = more)
  env <- list2env(iter$initial, parent = caller_env())
  env$.iter <- 1L
  expr <- iter$result
  char <- iter$yield

    for (i in seq_len(more)) {
      eval_bare(expr, env = env)
      vec[[i]] <- env[[char]]
      env$.iter <- i
    }
  iter$initial <- as.list(env, all.names = TRUE)
  iter$initial <- within(iter$initial, rm(.iter))
  return(vec)
}

