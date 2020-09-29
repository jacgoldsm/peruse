
`%[%` <- function(string, number) {
  new <- substring(string, seq(1, nchar(string)), seq(1, nchar(string)))
  return(new[number])
}

`%+=%` <- function(var,num) {
  x <- as.character(enexpr(var))
  assign(x, var + num, pos = .GlobalEnv)
}

`%-=%` <- function(var,num) {
  x <- as.character(enexpr(var))
  assign(x, var - num, pos = .GlobalEnv)
}