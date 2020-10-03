#' Test if an object is a Generator
#' @param list Object to test
#' @export

is_Generator <- function(list) {
  class(list) == "Generator"
}
