#' Making an iterable string
#'
#' In R, string objects are just elements of character vectors,
#' so they are not themselves iterable. An `iterstring` object changes that.
#' Mostly, `iterstrings` behave like Python strings, with some `R`-like exceptions
#'
#' @param string a string to make iterable
#' @return an iterable object of S3 class `iterstring`
#' @export
iterstring <- function(string) {
  if (length(string) != 1L) stop("`iterstring`s must be character vectors of length `1`. ")
  structure(string, class = "iterstring", names = names(string))
}

#' @export
print.iterstring <- function(string) {
  cat(string)
  invisible(NULL)
}
