#' Making an Irregular Sequence Iterator
#'
#' Create an Iterator object, where the user defines a sequence and a
#' set of initial values, and then calls `yield_next()` to generate the
#' next element of the sequence. `Iterator`s are R environments, which means
#' they are modified in place, even when passed as arguments to functions.
#' To make a copy of an Iterator that can be modified separately, see [clone()].
#'
#' @param result R expression to run each time 'yield_next' is called
#' @param initial named list or vector; declare and initialize every variable that appears in 'result'
#' @param yield variable to yield when 'yield_next()' is called
#'
#' @return An environment object of S3 type Iterator
#'
#' @note The expression to be evaluated can include constant values not defined in
#' `$initial` as long as they are defined globally. These values will not vary from iteration to
#' iteration. Notably, `$initial` is a list, not an environment. As such, the environment
#' in which the `Iterator` is created does not matter at all for how the expression is
#' evaluated—all that matters is the environment where `yield_*` is called from.
#'
#' @examples
#' #Create the Collatz sequence starting with 50 and print out the first 30 elements
#' expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1"
#' collatz <- Iterator(result = expr,
#'                     initial = c(n = 50),
#'                     yield = n)
#'
#' seq <- yield_more(collatz, 30)
#'
#' # using objects defined outside `$initial`:
#' # Note that `n` in `$initial` overrides the global `n`
#' m <- 100
#' n <- 10
#' expr <- "out <- n + m"
#' it <- Iterator(result = expr,
#'                initial = c(n = -10),
#'                yield = out)
#'
#' yield_next(it)
#'
#' # environments are modified in place, so be aware:
#' it <- Iterator('m <- m + 1', c(m = 0), m)
#' other <- it
#' yield_next(it)
#' current(other)
#'
#'@seealso [yield_next()], [yield_while()], [current()]
#'
#' @export

Iterator <- function(result,
                      initial,
                      yield) {
  yield <- rlang::enexpr(yield)
  if (length(result) != 1L || !is.character(result)) {
    rlang::abort("`result` must be a single character string")
  }
  result <- rlang::parse_exprs(result)

  if (!rlang::is_dictionaryish(initial)) {
    rlang::abort("Every element must have a valid and unique name in `initial`. ")
  }

  envir <- rlang::env(initial = as.list(initial),
                      result = result,
                      yield = yield)
  structure(envir,
            class = "Iterator")
}
