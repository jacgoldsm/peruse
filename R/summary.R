
#'@export
print.Iterator <- function(x, ...) {
  cat(" Expression: ",
      as.character(x$result[[1]]),
      "\n",
      "Variable to Yield:",
      as.character(x$yield),
      "\n")

  cat("\n")
  cat("Current Variable Values:")
  print(mget(ls(x$initial), envir = x$initial))


  invisible(NULL)
}

#'@export
summary.Iterator <- function(object, ...) {
  print(object)
  invisible(NULL)
}

