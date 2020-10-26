#' Summary and print method for objects of S3 class Iterator
#' @param iter an object of S3 class Iterator
#'
#' @examples
#' expr <- 'if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1'
#' collatz <- Iterator(result = expr,
#'                      initial = c(n = 50),
#'                      yield = n)
#' print(collatz)
#' summary(collatz) #equivalent


print.Iterator <- function(iter) {
  cat(" Expression: ",
      as.character(iter$result[[1]]),
      "\n",
      "Initial Value(s): ",
      unlist(names(iter$initial)),
      " = ",
      unlist(iter$initial),
      "\n",
      "Variable to Yield:",
      as.character(iter$yield))
}

summary.Iterator <- function(iter) {
  print(iter)
}

