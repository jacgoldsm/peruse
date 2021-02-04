#' yield_while
#'
#' @description Keep yielding the next element of an `Iterator` while a condition is met.
#' A condition is a logical expression involving variables in `iter$initial` or variables
#' that are defined globally. Refer to the number of the current iteration with `.iter`.
#'
#' @param iter An `Iterator` object
#' @param cond A quoted logical expression involving some variable(s) in `iter$initial`
#' or globally defined, so that `yield_next()` continues being called
#' while the expression returns TRUE
#'
#' @examples
#' expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1"
#' collatz <- Iterator(result = expr,
#'                     initial = list(n = 50),
#'                     yield = n)
#'yield_while(collatz, "n != 1L")
#'
#'p_success <- 0.5
#'threshold <- 100
#'seeds <- 1000:1e6
#'expr <- "
#'          set.seed(seeds[.iter])
#'          n <- n + sample(c(1,-1), 1, prob = c(p_success, 1 - p_success))
#'        "
#'iter <- Iterator(expr, list(n = 0), n)
#'sequence <- yield_while(iter, "n <= threshold")
#'
#' @export

yield_while <- function(iter, cond) {

  ret <- vector()
  cond <- rlang::parse_expr(cond)
  iter$initial$.iter <- 1L
  while (eval(cond, envir = iter$initial, enclos = rlang::caller_env())) {
      ret <- c(ret, yield_next_from_helper(iter))
      iter$initial$.iter <- iter$initial$.iter + 1L
  }
  iter$initial <- within(iter$initial, rm(.iter))
  ret
}
