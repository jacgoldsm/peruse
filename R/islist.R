#' @name islist
#' @rdname islists
#' @title Functions to create and manipulate `islist`s, lists of `iterstring`s
#'
#' @description Since an `iterstring` is an S3 object, there can be no atomic vectors of `iterstrings`.
#' Therefore containers of `iterstring`s are `list`s, and their S3 class is `islist`.
#' An `islist` is just a list of `iterstring`s, so it is inadvisable to try to call
#' `as_islist` on any object except a character vector, a list of `iterstring`s,
#' or a list of character vectors each of length one.
#'
#'@param islist an `islist` to turn into a character vector
#'@param object an object to convert to `islist`
#'
NULL

char_to_islist <- function(charvec) {
  l <- vector("list", length(charvec))
  for (i in seq_along(charvec)) {
    l[[i]] <- iterstring(charvec[[i]])
  }
  structure(l, class = "islist")
}

#' @rdname islists
#' @export
as.character.islist <- function(islist) {
  return(unlist(islist))
}

#' @rdname islists
#' @export
as_islist <- function(object) {
  if (is.character(object)) {
    object <- char_to_islist(object)
  } else {
    tryCatch(object <- lapply(object, iterstring),
             error = function(e) stop("Cannot coerce contained object to `iterstring`. "))
    obj <- as.list(object)
  }
  structure(obj, class = 'islist')
}

#' @export
print.islist <- function(list) {
  cat(unlist(list))
  invisible(NULL)
}
