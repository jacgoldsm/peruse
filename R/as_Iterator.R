#' Coerce a list to Iterator object
#' @param list A list to coerce to Iterator
#' @export

as_Iterator <- function(list) {
  stopifnot(length(list) == 3)
  stopifnot(is.expression(list[[1]]))
  stopifnot(is.numeric(list[[2]]))
  stopifnot(is.expression(list[[3]]) | length(list[[3]]) != 1)
  list <- list(result = list[[1]],
               initial = list[[2]],
               yield = as.name(list[[3]][[1]]))
  structure(list,
            class = "Iterator")
}
