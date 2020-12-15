#' Making an Irregular Sequence Iterator
#'
#' Create an Iterator object, where the user defines a sequence and a
#' set of initial values, and then calls 'yield_next' to generate the
#' next element of the sequence.
#' @param result R expression to run each time 'yield_next' is called
#' @param initial declare and initialize every variable that appears in 'result'
#' @param yield variable to yield when 'yield_next' is called
#'
#' @return An object of S3 type Iterator
#'
#' @examples
#' #Create the Collatz sequence starting with 50 and print out the first 30 elements
#' expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1"
#' collatz <- Iterator(result = expr,
#'                     initial = c(n = 50),
#'                     yield = n)
#' a <- numeric(length = 30)
#' for (i in 1:50) {
#'   a[i] <- yield_next(collatz)
#' }
#'
#' @export

Iterator <- function(result,
                      initial,
                      yield) {
  yield <- rlang::enexpr(yield)
  result <- rlang::parse_exprs(result)
  list <- list(initial = list2env(as.list(initial)),
               result = result,
               yield = yield)
  structure(list,
            class = "Iterator")
}
