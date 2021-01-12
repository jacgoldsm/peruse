#' @name combine
#' @rdname combines
#' @title Combine `iterstring`s easily
#'
#' @description `c.iterstring()` does exactly what you would expect it to, concatenating two
#' `iterstring`s and stripping their attributes (except `class`). `insert()` inserts
#' an `iterstring` *after* `pos` (remember, R-style indices starting with 1).
#'
#' Negative indices work the same as with [`[.iterstring`]—inserting a string at `pos` of
#' `-1` means putting it at the *end* of the string, i.e. after `str[-1]`, the last character
#' of `str`.
#'
#' @param string the starting `iterstring` (or `character` vec of length 1, coerced)
#' @param newstring an `iterstring` (or `character` vec. of length 1, coerced) to insert
#' @param pos the position *after which* `newstring` will be inserted
#' @param string1 the first `iterstring` in `c.iterstring()`
#' @param string2 the second `iterstring` in `c.iterstring()`
NULL

#' @rdname combines
#' @export
insert <- function(string, newstring, pos) {
  if (!inherits(string, "iterstring")) string <- iterstring(string)
  if (nchar(string) == 0L) return(iterstring(newstring))
  if (abs(pos) > nchar(string)) stop("Position out of range")
  if (pos < 0) pos <- nchar(string) + (pos + 1L)
  if (pos == 0L) return(iterstring(paste0(newstring, string, sep = '')))
  before <- string[seq_len(pos)]
  after <- ifelse(pos != nchar(string),
                  string[(pos + 1) : nchar(string)],
                  "")
  str <- paste0(before, newstring, after, sep = '')
  iterstring(str)
}

#' @rdname combines
#' @export
c.iterstring <- function(string1, string2) {
  str <- paste0(string1, string2, sep = '')
  iterstring(str)
}

