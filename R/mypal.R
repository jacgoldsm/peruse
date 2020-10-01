is_palindrome <- function(x) {
  if (is.numeric(x)) x <- as.character(x)
  x <- stringr::str_remove_all(x, "[^[:alpha:]]")
  x <- stringr::str_to_lower(x)

  l <- nchar(x)
  trues <- logical(length = l)
  for (i in 1:l) {
    if (substring(x, i, i) == substring(x, (l+1) - i, (l+1) - i)) trues[i] <- T
  }
  return(all(trues))
}
