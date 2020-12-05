

print.Iterator <- function(x, ...) {
  cat(" Expression: ",
      as.character(x$result[[1]]),
      "\n",
      "Initial Value(s): ",
      unlist(names(x$initial)),
      " = ",
      unlist(x$initial),
      "\n",
      "Variable to Yield:",
      as.character(x$yield))
}

summary.Iterator <- function(object, ...) {
  print(object)
}

