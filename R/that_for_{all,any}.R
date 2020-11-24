#' @name sets
#' @rdname funs
#'
#' @import magrittr
#' @title R Set Comprehension
#'
#' @description Set comprehension with the Magrittr Pipe.
#' Always use the syntax:
#' set1 %>% that_for_all(set2) %>% we_have(formula).
#'
#' @note if set2 is an numeric vector, you probably want a value obtained from
#' itertools::range(start, end) rather than start:end or seq(start,end), as when
#' start is greater than end you want an empty vector rather than counting backwards.
#' Note that itertools::range views end as a supremum, not a maximum, thus range(a,b)
#' is equivalent to the set `[`a,b) when a < b or `{}` when b >= a.
#' @param set1 A superset
#' @param set2 A subset of set1
#' @param formula A logical formula to test elements from set1 against those from set2
#' @param eval Should evaluation be eager (return a vector) or lazy (return an Iterator)?
#' @examples
#' library(magrittr)
#' 2:100 %>% that_for_all(range(2,x)) %>% we_have(~.x %% .y != 0) #is equivalent to
#' #reticulate::py_eval('{i for i in range(2,101) if all(i % y for y in range(2,i-1))}')
#' #c.f.
#' primes <- 2:100 %>% that_for_all(range(2,x)) %>% we_have(~.x %% .y != 0, "lazy")
#' yield_next(primes)
#' c("I", "Don't", "wan't", "chicken") %>% that_for_all('\'') %>% we_have(~stringr::str_detect(.x, .y))
#' #Twin primes 1 through 100
#' primes <- 2:100 %>% that_for_all(range(2,x)) %>% we_have(~.x %% .y != 0)
#' primes %>% that_for_any(primes) %>% we_have(~abs(.x - .y) == 2)
#' #Prime numbers 1 through 100 that are two away from a square number
#' (2:100 %>% that_for_all(range(2,x)) %>% we_have(~.x %% .y != 0)) %>%
#'     that_for_any(range(2,x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y)
#'
#' @return For that_for_all and that_for_any, an object of S3 class that_for_all or that_for_any.
#' For we_have, a vector of the same type as set1 if eval == 'eager' and an Iterator object if eval == 'lazy'.
#' @export

that_for_all <- function(set1, set2) {
  set2 <- rlang::enexpr(set2)
  structure(list(set1 = set1,
                 set2 = set2,
                 quant = 'all',
                 type = class(set1)))
}

#' @rdname funs
#' @export
that_for_any <- function(set1, set2) {
  set2 <- rlang::enexpr(set2)
  structure(list(set1 = set1,
                 set2 = set2,
                 quant = 'any',
                 type = class(set1)))
}



#' @rdname funs
#' @export
we_have <- function(that_for, formula, eval = "eager") {

  if (eval == "eager") {
    base <- rep(NA, length(that_for$set1))

    ret <- as.vector(mode = that_for$type,
                     x = base)

    if (that_for$quant == 'all') {
      for (i in seq_along(that_for$set1)) {
        ex <- new.env()
        assign("x", that_for$set1[i], pos = ex)
        bool_vec <- purrr::map2_lgl(that_for$set1[i], eval(that_for$set2, envir = ex), formula)
        if (all(bool_vec)) ret[i] <- that_for$set1[i]
      }
    }

    if (that_for$quant == 'any') {
      for (i in seq_along(that_for$set1)) {
        ex <- new.env()
        assign("x", that_for$set1[i], pos = ex)
        bool_vec <- purrr::map2_lgl(that_for$set1[i], eval(that_for$set2, envir = ex), formula)
        if (any(bool_vec)) ret[i] <- that_for$set1[i]
      }
    }
    return(ret[which(!is.na(ret))])

  }

  if (eval == "lazy") {
    assign("set1", that_for$set1, pos = sys.frame(which = -2))
    assign("set2", that_for$set2, pos = sys.frame(which = -2))
    assign("formula", formula, pos = sys.frame(which = -2))
  if (that_for$quant == "all") {
    expr <- "
    repeat {
    ex <- new.env()
    assign('x', set1[i], pos = ex)
    bool_vec <- purrr::map2_lgl(set1[i], eval(set2, envir = ex), formula)

    if (all(bool_vec)) {
          nth <- set1[i]
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
    assign('x', set1[i], pos = ex)
    bool_vec <- purrr::map2_lgl(set1[i], eval(set2, envir = ex), formula)

    if (any(bool_vec)) {
          nth <- set1[i]
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
