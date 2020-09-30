#' Coerce a list to generator
#' @param list a list to coerce
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
