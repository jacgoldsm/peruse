#' @name sets
#' @rdname funs
#' @import magrittr
#' @title R Set Comprehension
#'
#' @description Set comprehension with the magrittr Pipe.
#' Always use the basic syntax:
#'
#' `.x %>% that_for_all(.y) %>% we_have_*(f(.x, .y))`,
#' but see the examples for more detail.
#'
#' @details `formula` can be anything that is recognized as a function by [rlang::as_function()].
#' See the examples for how to specify the end of a sequence when used with an `Iterator`.
#'
#' Handling missing values in these expressions is possible and sometimes desirable but
#' potentially painful because `NA` values can't be compared with normal operators.
#' See the README for a detailed example.
#'
#' Note that `.x %>% that_for_all(.y)` is vacuously true if `.y` is empty, while
#' `.x %>% that_for_any(.y)` is vacuously false if `.y` is empty.
#'
#' @seealso The implementation of these functions involves code adapted from [purrr::every()]
#' and [purrr::some()], by Lionel Henry, Hadley Wickham, and RStudio, available under the
#' MIT license.
#'
#' @note if `.y` is an numeric vector, you probably want a value obtained from
#' `range(start, end)` rather than `start:end` or `seq.int(start,end)`,
#' as when start is greater than end you want an empty vector rather than counting backwards.
#' Note that [peruse::range()] views end as a supremum, not a maximum, thus `range(a,b)`
#' is equivalent to the set `[`a,b) when a < b or the empty set when b >= a.
#'
#' Also note that there is some indirection in the way that `.x` and `.y` are referenced
#' in the formula. In the function `we_have()`, the actual name of the two sets is `.x`
#' and `.y`. That is what makes the function interface work,
#' e.g. `function(.x, .y) .x - .y`. On the other hand, `purrr`-style lambda expressions,
#' e.g. `~.x - .y`, use positional arguments, where `.x` is the first argument and `.y`
#' is the second argument, no matter their names. Because those are actually their names,
#' this difference should never matter.
#'
#' @param .x A set, represented as either an atomic vector or a list
#' @param .y A set to compare to `.x`
#' @param formula A function, lambda, or formula. Must be understood by
#' [rlang::as_function()]
#' @param result Should the expression return a `vector` or an `Iterator`?
#' @param that_for A list passed to [we_have()]—can be ignored with proper syntax
#'
#' @examples
#' 2:100 %>% that_for_all(range(2, .x)) %>% we_have(function(.x, .y) .x %% .y != 0) #is the same as
#' 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y) # 0 = F, (not 0) = T
#' #c.f.
#' primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y, "Iterator")
#' yield_next(primes)
#' primes2 <- clone(primes)
#'
#' # Refer to the vector .x with `.x_vector` and the current index of that vector with `.i`
#' # For example, to yield to the end of the sequence:
#' yield_while(primes, .x_vector[.i] <= length(.x_vector))
#' # `.finished` is an alias for `.x_vector[.i] > length(.x_vector)`
#' # Equivalent to previous expression:
#' yield_while(primes2, !.finished)
#' {c("I", "Don't", "wan't", "chicken") %>%
#'              that_for_all("\'") %>%
#'              we_have(~grepl(.y, .x))}
#' #Twin primes 1 through 100
#' primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y)
#' primes %>% that_for_any(primes) %>% we_have(~abs(.x - .y) == 2)
#' #Prime numbers 1 through 100 that are two away from a square number
#' (2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y)) %>%
#'     that_for_any(range(2, .x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y)
#'
#' @return For \code{that_for_all()} and \code{that_for_any()}, an object of S3 class that_for_all or that_for_any.
#' For `we_have()`, a vector of the same type as `.x` if `return == 'vector'` and an Iterator object if `return == 'Iterator'`.
NULL

#' @rdname funs
#' @export
that_for_all <- function(.x, .y) {
  .y <- enquo(.y)
  list(.x = .x,
       .y = .y,
       quant = 'all')
}

#' @rdname funs
#' @export
that_for_any <- function(.x, .y) {
  .y <- enquo(.y)
  list(.x = .x,
       .y = .y,
       quant = 'any')
}



#' @rdname funs
#' @export
we_have <- function(that_for, formula, result = "vector") {

  if (result == "vector") {
    ret <- rep(NA, length(that_for$.x))

    if (that_for$quant == "all") {
      for (i in seq_along(that_for$.x)) {
        current_y_vector <- eval_tidy(that_for$.y, list(.x = that_for$.x[i]))
        ret[i] <- every2(that_for$.x[i], current_y_vector, formula)
      }

    } else {
      for (i in seq_along(that_for$.x)) {
        current_y_vector <- eval_tidy(that_for$.y, list(.x = that_for$.x[i]))
        ret[i] <- some2(that_for$.x[i], current_y_vector, formula)
      }
    }

    return(that_for$.x[which(ret)])
  }

  if (result == "Iterator") {

  .finished <- FALSE

  .fun <- NA
  if (that_for$quant == "all") {
    .fun <- every2
  } else if (that_for$quant == "any") {
    .fun <- some2
  } else rlang::abort("Invalid Quantifier")

  expr <- rlang::expr({
    if (.i > length(.x_vector)) rlang::abort("Error: attempt to call `yield_next()` without any more elements of sequence")
    ret <- FALSE
    .fun <- !! .fun
    while(!ret) {
      current_y_vector <- rlang::eval_tidy(y_name, list(.x = .x_vector[.i]))
      ret <- .fun(.x_vector[.i], current_y_vector, formula_name)

      if (ret) {
        .nth <- .x_vector[.i]
      }
      .i <- .i + 1L

      if (.i > length(.x_vector)) {
        message("(Note: result has reached end of sequence)")
        .finished <- TRUE
        .nth <- NA
        break
      }
    }
  })
    initial <- list(.i = 1L,
                    .nth = NA,
                    .x_vector = that_for$.x,
                    y_name = that_for$.y,
                    formula_name = formula,
                    .finished = .finished)

    # Note: since `expr` is already quoted, forcing just creates the expression,
    # unevaluated
    return(
      Iterator(result = !! expr, initial = initial, yield = .nth)
    )
  }
  }


every2 <- function(.x, .y, .p, ...) {
  .p <- rlang::as_function(.p)
  if (length(.y) == 0L) return(TRUE) # condition holds vacuously
  for (i in seq_along(.x)) {
    for(j in seq_along(.y)) {
      val <- .p(.x[[i]], .y[[j]], ...)

      if (!val) {
        return(FALSE)
      }
    }
  }

  TRUE
}

some2 <- function(.x, .y, .p, ...) {
  .p <- rlang::as_function(.p)
  if (length(.y) == 0L) return(FALSE) # returns false vacuously
  val <- FALSE
  for (i in seq_along(.x)) {
    for (j in seq_along(.y)) {
      val <- val || .p(.x[[i]], .y[[j]], ...)

      if (val) {
        return(TRUE)
      }
    }
  }

  val
}

