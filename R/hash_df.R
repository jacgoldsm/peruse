#'@name hash_df
#'
#'
#'@title  `data.frame`s with hash table internals
#'
#'@description Create a `data.frame` built on the `environment`
#'`S3` class and embedded in an `R6` object. The primary advantage
#'of this structure is the reference semantics, which means that
#'elements (in this case `data.frame` columns) are not copied upon
#'modification. Although modern R avoids making deep copies of
#'`data.frame`s when they are modified, the copy on modify semantics
#'applied to the container (the list of pointers to the columns of the `data.frame`),
#'can still significantly slow down loops that modify large numbers of columns
#'on wide `data.frame`s.
#'
#'`hash_df`s also offer access to their columns in amortized constant
#'time (`O(1)`) due to their internal structure as a hash table.
#'
#'`hash_df`s are bare-bones, with methods for adding (`bind`ing)
#' and deleting (`unbind`ing) variables as well as printing a
#' preview of the data. In addition, {dplyr} functions `select`
#' and `mutate` have implementations for `hash_df`'s (that do not
#' depend on {dplyr}). Other {dplyr} functions like `summarise()`
#' and `*_join` cannot easily take advantage of the `hash_df` structure,
#' therefore they are not implemented here.
#' Data wrangling and analysis functions not implemented in `hash_df`
#' should be done after converting back to a `data.frame`
#' object with `hash_df$return_df()`.
#'
#'@examples
#'df <- hash_df$new(iris)
#'for (name in df$vars) {
#'  if (is.numeric(df$data[[name]])) df$data[[name]] <- 2 * df$data[[name]]
#'}
#'
#'hash_mtcars <- hash_df$new(mtcars)
#'hash_mtcars$data <- lapply(hash_mtcars$data, function(x) log(x + 1))
#'hash_mtcars$print()
#'
#'wide_df <- as.data.frame(matrix(1:5000, nrow = 2))
#'hash_wide_df <- hash_df$new(wide_df)
#'
#'for (name in hash_wide_df$vars) {
#'   hash_wide_df$data[[name]] <- -1 *  hash_wide_df$data[[name]]
#'}
#'\dontrun{
#'timer <- bench::mark(
#'  check = F,
#'  for(name in names(wide_df)) wide_df[[name]] <- -1 * wide_df[[name]],
#'  for (name in hash_wide_df$vars) hash_wide_df$data[[name]] <- -1 *  hash_wide_df$data[[name]]
#')}
#'#> hash_df: Median = 22.2.3ms
#'#> data.frame: Median = 615.3ms
#'
#'@return A `hash_df` object, built on `R6` and `env` `S3` classes
#'
#'@export


hash_df <- R6::R6Class("hash_df",
  public = list(
    #' @field data an `environment` containing the the data from a `data.frame`.
    data = NA,
    #' @description add `data.frame` columns
    #' @param ... named columns to add
    #' @examples
    #' df <- hash_df$new(iris)
    #' df$bind(col_ones = 1, col_his = "hi")
    #' df$print()
  bind = function(...) {
    x <- rlang::dots_list(..., .named = T)
    rlang::env_bind(self$data, !!!x)
    invisible(self)
  },
  #' @description remove `data.frame` columns
  #' @param ... names of the columns to remove
  #' @examples
  #' df <- hash_df$new(iris)
  #' df$unbind(Petal.Length)
  #' df$print()
  unbind = function(...) {
    x <- unlist(as.character(rlang::enexprs(...)))
    rlang::env_unbind(self$data, x)
    invisible(self)
  },
  #' @description return the `data.frame` from a `hash_df`
  #' @return A `data.frame` object
  return_df = function() {
    as.data.frame(as.list(self$data))
  },
  #' @description print a preview of the `data.frame`
  print = function() {
    print(self$return_df()[1:20,])
  },
  #' @description create a new `hash_df` from a `data.frame`
  #' @param df a `data.frame`
  initialize = function(df) {
    if (is.matrix(df)) df <- as.data.frame(df)
    stopifnot(is.data.frame(df))
    self$data = list2env(df)
    private$.nrow = nrow(df)
    private$.ncol = ncol(df)
    private$.vars = names(df)
  },
  #' @description open the data in the `View` pane
  View = function() {
    View(self$return_df())
  },
  #'@description a bare-bones `dplyr::mutate` that takes advantage of the hashed data structure
  #'@param ... named R expressions to mutate the columns by
  #'@examples
  #'df <- hash_df$new(iris)
  #'df$mutate(Sepal.Size = Sepal.Length * Sepal.Width)
  #'df$print()
  #'@seealso `dplyr::mutate`
  mutate = function(...) {
    exprs <- rlang::enexprs(...)
    cols <- names(exprs)

    for (key in cols){
      self$data[[key]] <- eval(exprs[[key]], envir = self$data)
    }
  },
  #'@description a bare-bones `dplyr::select` that takes advantage of the hashed data structure
  #'@param ... variables to select, separated by a comma
  #'@examples
  #'df <- hash_df$new(iris)
  #'df$select(Sepal.Length, Species)
  #'df$print()
  #'@seealso `dplyr::select`
  select = function(...) {
      vars <- unlist(as.character(rlang::enexprs(...)))
      tryCatch(
        expr = {self$data <- list2env(rlang::env_get_list(self$data, vars))},
        error = function(e) stop("Error: attempt to select columns that don't exist")
      )
    }
 ),
  active = list(
    #' @field nrow the number of rows of the `data.frame`
    nrow = function(value) {
      if (missing(value)) nrow(self$return_df()) else stop("cannot replace `nrow` manually")
    },
    #' @field ncol the number of columns of the `data.frame`
    ncol = function(value) {
      if (missing(value)) ncol(self$return_df()) else stop("cannot replace `ncol` manually")
    },
    #' @field vars the variables (column names) of the `data.frame`
    vars = function(value) {
      if (missing(value))  names(self$data) else stop("cannot replace `vars` manually")
    }
  ),
 private = list(
   .nrow = NA,
   .ncol = NA,
   .vars = NA
 ))


