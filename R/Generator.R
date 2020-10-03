#' Making a Python-style Generator
#'
#' Create a Generator object, where the user defines a sequence and a
#' set of initial values, and then calls 'yield_next' to generate the
#' next element of the sequence
#' @param result R expression to run each time 'yield_next' is called
#' @param current declare and initialize every variable that appears in 'result'
#' @param yield variable to yield when 'yield_next' is called
#' @return An object of S3 type generator
#' @examples
#' #Create the Collatz sequence starting with 50 and print out the first
#' #30 elements
#' expr <- "if (n0 %% 2 == 0) n <- n0 / 2
#' if (n0 %% 2 != 0) n <- n0*3 + 1
#' n0 <- n"
#' collatz <- Generator(result = expr,
#'                      current = c(n0 = 50,
#'                                  n = 0),
#'                      yield = n)
#'
#' a <- numeric(length = 30)
#' for (i in 1:30) {
#'   a[i] <- yield_next(collatz)
#' }
#'
#' @export

Generator <- function(result,
                      current,
                      yield) {
  yield <- rlang::enexpr(yield)
  result <- rlang::parse_exprs(result)
  list <- list(current = as.list(current),
               result = result,
               yield = yield)
  structure(list,
            class = "Generator")
}
