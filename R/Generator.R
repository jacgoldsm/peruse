#' Making a Python-style Generator
#'
#' Uses R's S3 OO paradigm and lazy evaluation
#' @param result R expression to run each time 'yield_next' is called
#' @param current declare and initialize every variable that appears in 'result'
#' @param yield variable to yield when 'yield_next' is called
#' @return An object of S3 type generator
#' @examples
#' #Fibonacci sequence: Each time yield_next is called, the next
#' #number from the sequence will be generated and returned
#' expr <- "if (i == 0) {
#'             n0 <- 0;
#'            n1 <- 0;
#'            n <- n0 + n1; };
#'         if (i == 1) {
#'            n0 <- 0; n1 <- 1;
#'            n <- n0 + n1;
#'         };
#'         if (i > 1) {
#'
#'            n <- n0 + n1;
#'            a <- n1;
#'             b <- n0 + n1;
#'             n0 <- a;
#'             n1 <- b;
#'           };
#'           i <- i + 1;"
#'
#' gen <- Generator(result = expr,
#'                 current = c(i = 0,
#'                             a = 0,
#'                             b = 0,
#'                             n0 = 0,
#'                             n1 = 0,
#'                             n = 0),
#'                 yield = n)
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
