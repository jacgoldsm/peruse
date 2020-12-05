#' @name sets
#' @rdname funs
#'
#' @import magrittr
#' @title R Set Comprehension
#'
#' @description Set comprehension with the Magrittr Pipe.
#' Always use the syntax:
#' `.x %>% that_for_all(f(.x)) %>% we_have(f(.x, .y))`.
#'
#' @note if .y is an numeric vector, you probably want a value obtained from
#' `itertools::range(start, end)` rather than start:end or seq(start,end), as when
#' start is greater than end you want an empty vector rather than counting backwards.
#' Note that `itertools::range` views end as a supremum, not a maximum, thus range(a,b)
#' is equivalent to the set `[`a,b) when a < b or `{}` when b >= a.
#' @param .x A set, represented as either an atomic vector or a list
#' @param .y A set to compare to `.x`
#' @param formula A boolean-valued function, lambda, or formula
#' @param result Should the expression return a `vector` or an `Iterator`?

#' @param that_for A list passed to `we_have`--can be ignored with proper syntax

#'
#' @examples
#' library(magrittr)
#' 2:100 %>% that_for_all(range(2, .x)) %>% we_have(function(.x, .y) .x %% .y != 0) #is the same as
#' 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0)
#' #c.f.
#' primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0, "Iterator")
#' yield_next(primes)

#' \dontrun{c("I", "Don't", "wan't", "chicken") %>%
#'              that_for_all("\'") %>%
#'              we_have(~stringr::str_detect(.x, .y))}

#' #Twin primes 1 through 100
#' primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0)
#' primes %>% that_for_any(primes) %>% we_have(~abs(.x - .y) == 2)
#' #Prime numbers 1 through 100 that are two away from a square number

#' (2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0)) %>%
#'     that_for_any(range(2, .x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y)
#'
#' @return For that_for_all and that_for_any, an object of S3 class that_for_all or that_for_any.
#' For we_have, a vector of the same type as .x if `return == 'vector'` and an Iterator object if `return == 'Iterator'`.
#' @export

that_for_all <- function(.x, .y) {
  .y <- rlang::enexpr(.y)
  structure(list(.x = .x,
                 .y = .y,
                 quant = 'all',
                 type = class(.x)))
}

#' @rdname funs
#' @export
that_for_any <- function(.x, .y) {
  .y <- rlang::enexpr(.y)
  structure(list(.x = .x,
                 .y = .y,
                 quant = 'any',
                 type = class(.x)))
}



#' @rdname funs
#' @export
we_have <- function(that_for, formula, result = "vector") {

  if (result == "vector") {
    base <- rep(NA, length(that_for$.x))

    ret <- as.vector(mode = that_for$type,
                     x = base)

    if (that_for$quant == 'all') {
      for (i in seq_along(that_for$.x)) {
        bool_vec <- purrr::map2_lgl(that_for$.x[i],
                                    rlang::eval_bare(that_for$.y, rlang::env(.x = that_for$.x[i])),
                                    formula)
        if (all(bool_vec)) ret[i] <- that_for$.x[i]
      }
    }

    if (that_for$quant == 'any') {
      for (i in seq_along(that_for$.x)) {
        bool_vec <- purrr::map2_lgl(that_for$.x[i],
                                    rlang::eval_bare(that_for$.y, rlang::env(.x = that_for$.x[i])),
                                    formula)
        if (any(bool_vec)) ret[i] <- that_for$.x[i]
      }
    }
    return(ret[which(!is.na(ret))])

  }

  if (result == "Iterator") {
    assign(".x", that_for$.x, pos = sys.frame(which = -2))
    assign(".y", that_for$.y, pos = sys.frame(which = -2))
    assign(".formula", formula, pos = sys.frame(which = -2))
  if (that_for$quant == "all") {
    expr <- "
    repeat {
    ex <- new.env()
    assign('.x', .x[i], pos = ex)
    bool_vec <- purrr::map2_lgl(.x[i], eval(.y, envir = ex), .formula)

    if (all(bool_vec)) {
          nth <- .x[i]
          i <- i + 1
          break
    } else {
          i <- i + 1
    }
    }

    "
  } else {
    expr <- "
    repeat {
    ex <- new.env()
    assign('.x', .x[i], pos = ex)
    bool_vec <- purrr::map2_lgl(.x[i], eval(.y, envir = ex), .formula)

    if (any(bool_vec)) {
          nth <- .x[i]
          i <- i + 1
          break
    } else {
          i <- i + 1
    }
    }

    "
  }
    return(
      Iterator(result = expr, initial = c(i = 1, nth = 0), yield = nth)
    )
  }
}
