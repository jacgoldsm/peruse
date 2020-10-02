#' @name sets
#' @rdname funs
#'
#' @title R Set Comprehension
#'
#' @description  Replicates Python's set comprehension using tidy syntax.
#' Always use the syntax: set1 %>% that_for_all(set2) %>% we_have(formula).
#' @param set1 A superset
#' @param set2 A subset of set1
#' @param formula A logical formula to test elements from set1 against those from set2
#' @examples
#' 1:10 %>% that_for_all(1:5) %>% we_have(~.x %% .y != 0)
#' c("I", "Don't", "wan't", "chicken") %>% that_for_all('\'') %>% we_have(~stringr::str_detect(.x, .y))
#'
#' @return For that_for_all and that_for_any, an object of S3 class that_for_all or that_for_any.
#' For we_have, a vector of the same type as set1
#' @export

that_for_all <- function(set1, set2) {
  set2 <- rlang::enexpr(set2)
  structure(list(set1,set2, 'all'), class = "that_for_all")
}

#' @rdname funs
#' @export
that_for_any <- function(set1, set2) {
  set2 <- rlang::enexpr(set2)
  structure(list(set1,set2, 'any'), class = "that_for_any")
}

#' @rdname funs
#' @export
we_have <- function(that_for, formula) {
  ret <- list()
  if (that_for[[3]] == 'all') {
    for (x in 1:length(that_for[[1]])) {
      bool_vec <- purrr::map2_lgl(that_for[[1]][x], eval(that_for[[2]]), formula)
      bool_vec <- ifelse(is.na(bool_vec), FALSE, bool_vec)
      if (all(bool_vec)) {
        ret <- append(ret,that_for[[1]][x])
      }
    }
  }

  if (that_for[[3]] == 'any') {
    for (x in 1:length(that_for[[1]])) {
      bool_vec <- purrr::map2_lgl(that_for[[1]][x], eval(that_for[[2]]), formula)
      bool_vec <- ifelse(is.na(bool_vec), FALSE, bool_vec)
      if (any(bool_vec)) {
        ret <- append(ret,that_for[[1]][x])
      }
    }

  }
  return(unlist(ret))
}
