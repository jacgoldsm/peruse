#' @name Extract
#' @rdname subset
#' @title Get and set elements of an `iterstring`
#'
#' @description Get and set elements of a string, similar to an R vector or a Python string.
#' Note that indices start at 1 like R vectors, not Python strings.
#' Includes `slice`-ing vectors with positive or negative indices, like in Python.
#' Follows R convention of `[` single-bracket subsetting multiple elements and preserving
#' attributes, and `[[` double bracket subsetting a single element (in this case a single character)
#' and stripping attributes.
#'
#' Note how negative indices work—the last character of a string `str` is `str[-1]`,
#' the second to last is `str[-2]`, etc. `str[[0]]` will throw an error, as will `str[[-0]]`.
#' `str[0]` and `str[-0]` will return an empty string.
#'
#' @param string an `iterstring` to subset
#' @param index an index to extract with `[[`
#' @param range a range of indices to extract with `[`
#' @param value a suibable replacement value, must be a single character with `[[`
NULL

#' @rdname subset
#' @export
`[.iterstring` <- function(string, range) {
  start <- range[1]
  end <- range[length(range)]
  if (start < 0) start <- nchar(string) + (start + 1L)
  if (end < 0) end <- nchar(string) + (end + 1L)
  sub <- substr(string, start, end)
  sub

}

#' @rdname subset
#' @export
`[[.iterstring` <- function(string, index) {
  if (length(index) != 1L) stop("`[[` requires a scalar index. ")
  if (index < 0) index <- nchar(string) + (index + 1L)
  if (index > nchar(string)) stop("Index out of bounds")
  sub <- substr(string, index, index)
  class(sub) <- NULL
  names(sub) <- NULL
  sub
}

#' @rdname subset
#' @export
`[<-.iterstring` <- function(string, range, value) {
  if (length(value) != 1L) stop("Replacement must be of length `1`. ")
  start <- range[1]
  end <- range[length(range)]
  if (start < 0) start <- nchar(string) + (start + 1L)
  if (end < 0) end <- nchar(string) + (end + 1L)
  substr(string, start, end) <- value
  iterstring(string)
}

#' @rdname subset
#' @export
`[[<-.iterstring` <- function(string, index, value) {
  if (length(value) != 1L) stop("Replacement must be of length `1`. ")
  if (length(index) != 1L) stop("`[[<-` requires a scalar index. ")
  if (index > nchar(string)) stop("Index out of bounds")
  if (index < 0) index <- nchar(string) + (index + 1L)
  substr(string, index, index) <- value
  iterstring(string)
}
