#' yield_while
#'
#'
#' @description Keep yielding the next element of an `Iterator` while a condition is met.
#' A condition is a logical expression involving variables in `iter$initial` or variables
#' that are defined in the enclosure. Refer to the number of the current iteration with `.iter`.
#'
#' @param iter An `Iterator` object
#' @param cond A logical expression involving some variable(s) in `iter$initial`
#' or in the enclosure, so that `yield_next()` continues being called
#' while the expression returns TRUE
#'
#' @examples
#' collatz <- Iterator({
#'             if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1
#'            },
#'            initial = list(n = 50),
#'            yield = n)
#'yield_while(collatz, n != 1L)
#'
#'p_success <- 0.5
#'threshold <- 100
#'seeds <- 1000:1e6
#'iter <- Iterator({
#'         set.seed(seeds[.iter])
#'         n <- n + sample(c(1,-1), 1, prob = c(p_success, 1 - p_success))
#'        },
#'        list(n = 0),
#'        n)
#'sequence <- yield_while(iter, n <= threshold)
#'
#' @export

yield_while <- function(iter, cond) {
  stopifnot(is_Iterator(iter))
  ret <- rep(NA, 1e3)
  cond <- enexpr(cond)
  iter$initial$.iter <- 1L
  env <- list2env(iter$initial, parent = caller_env())
  expr <- iter$result
  char <- iter$yield

 if (!is.null(iter$initial$.finished)) {
  while (eval_bare(cond, env)) {
    # Efficiently resizes `ret` in the style of `std::vector()` from the STL
      if (env$.iter > length(ret)) ret <- c(ret, rep(NA, length(ret)))
      eval_bare(expr, env = env)
      if (env$.finished) break
      ret[[env$.iter]] <- env[[char]]
      env$.iter <- env$.iter + 1L
  }
 } else {
   while (eval_bare(cond, env)) {
     if (env$.iter > length(ret)) ret <- c(ret, rep(NA, length(ret)))
     eval_bare(expr, env = env)
     ret[[env$.iter]] <- env[[char]]
     env$.iter <- env$.iter + 1L
   }
 }
  iter$initial <- as.list(env, all.names = TRUE)
  iter$initial <- within(iter$initial, rm(.iter))
  ret[1:env$.iter - 1]
}
