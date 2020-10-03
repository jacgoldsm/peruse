#' Coerce a list to generator
#' @param list A list to coerce to Generator
#' @export

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
