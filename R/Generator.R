################################################
#Making a Python-style Generator
#Uses R's S3 OO paradigm and lazy evaluation
################################################




####################################################
#Constructor
#Requires initial value for n and iter. expression
#Inherits from base type 'list'
####################################################

Generator <- function(result,
                      current,
                      yield = n) {
  yield <- rlang::enexpr(yield)
  result <- rlang::parse_exprs(result)
  list <- list(current = as.list(current),
               result = result,
               yield = yield,
               syms = syms)
  structure(list,
            class = "Generator")
}



###########################################################
#Equivalent to Python's 'next'
#Finds the value of the next iteration of 'gen'
#If argument is list, attempts to coerce to Generator
###########################################################
next_one <- function(gen) {
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

  print(gen$current[[yield_name]])
  #pushes the local copy of 'gen' into the parent environment
  assign(gen_name, gen, pos = parent.frame(n = 1))
}


#########################################
#Test if an object is a Generator
#########################################

is_Generator <- function(list) {
  class(list) == "Generator"
}



#########################################
#Coerce an object to type Generator
#Only works if obj is list of two
#Such that l[[1]] is an expression,
#And l[[2]] is numeric
#########################################

as_Generator <- function(list) {
  stopifnot(length(list) == 3)
  stopifnot(is.expression(list[[1]]))
  stopifnot(is.numeric(list[[2]]))
  stopifnot(is.expression(list[[3]]) | length(list[[3]]) != 1)
  list <- list(result = list[[1]],
               current = list[[2]],
               yield = as.name(list[[3]][[1]]))
  structure(list,
            class = "Generator")
}
###################################################
# Example: Fibonacci Sequence
#
# expr <- "if (i == 0) {
#             n0 <- 0;
#             n1 <- 0;
#             n <- n0 + n1; };
#          if (i == 1) {
#             n0 <- 0; n1 <- 1;
#             n <- n0 + n1;
#          };
#          if (i > 1) {
#             n <- n0 + n1;
#             a <- n1;
#             b <- n0 + n1;
#             n0 <- a;
#             n1 <- b;
#           };
#           i <- i + 1;"
#
# gen <- Generator(result = expr,
#                 current = c(i = 0,
#                             a = 0,
#                             b = 0,
#                             n0 = 0,
#                             n1 = 0,
#                             n = 0),
#                 yield = n)
####################################################



