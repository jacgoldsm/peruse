#' @name sets
#' @rdname funs
#'
#' @import magrittr
#' @title R Set Comprehension
#'
#' @description Set comprehension with the Magrittr Pipe.
#' Always use the syntax:
#' `.x %>% that_for_all(.y) %>% we_have_*(f(.x, .y))`.
#'
#' @note if .y is an numeric vector, you probably want a value obtained from
#' `peruse::range(start, end)` rather than `start:end` or `seq(start,end)`, as when
#' start is greater than end you want an empty vector rather than counting backwards.
#' Note that `peruse::range` views end as a supremum, not a maximum, thus `range(a,b)`
#' is equivalent to the set `[`a,b) when a < b or `{}` when b >= a.
#' @param .x A set, represented as either an atomic vector or a list
#' @param .y A set to compare to `.x`
#' @param formula A boolean-valued function, lambda, or formula
#' @param result Should the expression return a `vector` or an `Iterator`?

#' @param that_for A list passed to `we_have`--can be ignored with proper syntax

#' @details This function is designed to be used with \code{%>%} from \code{magrittr}.
#' Essentially, this function evaluates \code{f %>% g()} as
#' \code{eval(g(f), envir = new.env(parent = parent.frame())}. Any pipe or pipe-like
#' function that works similarly should work with \code{that_for_*}. In particular,
#' the R Core Team is expected to implement a base R pipe in the near future with
#' the syntax \code{|>}. While these functions will likely work properly with the new
#' \code{|>}, as of right now the only pipe that is guaranteed to work is
#' \code{%>%}.
#'
#' @examples
#' library(magrittr)
#' 2:100 %>% that_for_all(range(2, .x)) %>% we_have(function(.x, .y) .x %% .y != 0) #is the same as
#' 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0)
#' #c.f.
#' primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0, "Iterator")
#' yield_next(primes)

#' {c("I", "Don't", "wan't", "chicken") %>%
#'              that_for_all("\'") %>%
#'              we_have(~grepl(.y, .x))}

#' #Twin primes 1 through 100
#' \dontrun{
#' primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0)
#' primes %>% that_for_any(primes) %>% we_have(~abs(.x - .y) == 2)
#' #Prime numbers 1 through 100 that are two away from a square number
#' }
#' (2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0)) %>%
#'     that_for_any(range(2, .x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y)
#'
#' @return For \code{that_for_all} and \code{that_for_any}, an object of S3 class that_for_all or that_for_any.
#' For we_have, a vector of the same type as `.x` if `return == 'vector'` and an Iterator object if `return == 'Iterator'`.
NULL

#' @rdname funs
#' @export
that_for_all <- function(.x, .y) {
  .y <- rlang::enexpr(.y)
  structure(list(.x = .x,
                 .y = .y,
                 quant = 'all'))
}

#' @rdname funs
#' @export
that_for_any <- function(.x, .y) {
  .y <- rlang::enexpr(.y)
  structure(list(.x = .x,
                 .y = .y,
                 quant = 'any'))
}



#' @rdname funs
#' @export
we_have <- function(that_for, formula, result = "vector") {

  if (result == "vector") {

    quant <- match.fun(that_for$quant) # either `all()` or `any()`
    ret <- rep(NA, length(that_for$.x)) # defaults to logical, will be coerced
                                        # if it gets any other values
    for (i in seq_along(that_for$.x)) {
      bool_vec <- purrr::map2_lgl(that_for$.x[i],
                                  rlang::eval_bare(that_for$.y, rlang::env(.x = that_for$.x[i])),
                                  formula)
      if (quant(bool_vec)) ret[i] <- that_for$.x[i]
      }

    return(ret[which(!is.na(ret))])

  }

  if (result == "Iterator") {


  if (that_for$quant == "all") {
    expr <- "
    repeat {
    bool_vec <- purrr::map2_lgl(x_name[i],
                                rlang::eval_bare(y_name, rlang::env(.x = x_name[i])),
                                formula_name)

    if (all(bool_vec)) {
          .nth <- x_name[i]
          i <- i + 1L
          break
    } else {
          i <- i + 1L
    }
    }

    "
  } else if (that_for$quant == "any") {
    expr <- "
    repeat {
    bool_vec <- purrr::map2_lgl(x_name[i],
                                rlang::eval_bare(y_name, rlang::env(.x = x_name[i])),
                                formula_name)

    if (any(bool_vec)) {
          .nth <- x_name[i]
          i <- i + 1L
          break
    } else {
          i <- i + 1L
    }
    }
    "
  } else rlang::abort("Invalid quantifier")

    initial <- rlang::env(i = 1,
                          .nth = 0,
                          x_name = that_for$.x,
                          y_name = that_for$.y,
                          formula_name = formula)

    return(
      Iterator(result = expr, initial = initial, yield = .nth)
    )
  }
}
