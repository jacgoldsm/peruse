#' Python-style range function
#'
#' Wrapper around base::seq that replaces the maximal end value with the supremum
#' and returns an empty vector if b <= a, in the style of Python's range().
#' Note that itertools::range views end as a supremum, not a maximum, thus range(a,b)
#' is equivalent to the set `[`a,b) when a < b or `{}` when b >= a.
#'
#' @param a minimum, integer
#' @param b supremum, integer
<<<<<<< HEAD
#' @param ... other params passed to `base::seq()`
=======
#' @param ... other params passed to base::seq()
>>>>>>> 4740ccddd35aa43b82ae3f65d3fa181262711579
#' @examples
#' range(1,5)
#' range(9,10)
#' @export

range <- function(a,b, ...) {
  if (a < b) return(seq(a, b-1,...)) else return(numeric())
}
