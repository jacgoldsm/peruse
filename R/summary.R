
#'@export
print.Iterator <- function(x, ...) {
  cat(" Expression: ",
      as.character(unlist(x$result)),
      "\n",
      "Variable to Yield:",
      as.character(x$yield),
      "\n")

  cat("\n")
  cat("Current Variable Values:")
  print(mget(ls(list2env(x$initial)), envir = list2env(x$initial)))


  invisible(NULL)
}

#'@export
summary.Iterator <- function(object, ...) {
  print(object)
  invisible(NULL)
}

