#' Equivalent to Python's 'next'
#'
#' Finds the value of the next iteration of 'gen'
#' If argument is list, attempts to coerce to Generator
#' @param gen a generator object
#' @return an object of whatever type 'result' evaluates to from the Generator
#' @export



yield_next <- function(gen) {
  e1 <- environment()
  if (!is_Generator(gen)) gen <- as_Generator(gen)
  gen_name <- deparse(substitute(gen))
  yield_name <- as.character(gen$yield)
  list2env(gen$current, envir = e1)

  for (j in 1:length(gen$result)) {
    eval(gen$result[[j]], envir = e1)
  }

  for (key in names(gen$current)) {
    gen$current[key] <- eval(rlang::parse_expr(key), envir = e1)
  }

  return(gen$current[[yield_name]])
  #pushes the local copy of 'gen' into the parent environment
  assign(gen_name, gen, pos = parent.frame(n = 1))
}
