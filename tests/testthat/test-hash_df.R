test_that("prime_numbers", {
  expect_equal((2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0))[7],
               17L)
})

test_that("prime_within_two", {
          expect_equal(((2:100 %>% that_for_all(range(2,.x)) %>% we_have(~.x %% .y != 0)) %>%
                               that_for_any(range(2, .x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y))[2],
          11L)
})

test_that("collatz", { .yieldenv <- new.env(parent = emptyenv())
          expect_equal({expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
           collatz <- Iterator(result = expr,
                             initial = c(n = 50),
                              yield = n);
            yield_next(collatz)},
          25L)
})

test_that("mutate", {
          expect_equal({df <- hash_df$new(iris);
          df$mutate(Sepal.Size = Sepal.Length * Sepal.Width);
          (df$return_df())[1,1]},
          17.85)
})

test_that("lapply", {
          expect_equal({hash_mtcars <- hash_df$new(mtcars);
          hash_mtcars$data <- lapply(hash_mtcars$data, function(x) log(x + 1));
          (hash_mtcars$return_df())[1,1]},
          1.609438,
          tolerance = 8e-7)
})

test_that("mutate", {
  expect_equal({df <- hash_df$new(iris)
                df$mutate(Sepal.Size = Sepal.Length * Sepal.Width)
                df$data$Sepal.Size[1]},
                17.85)
})

test_that("mutate_if", {
    expect_equal({
      df <- hash_df$new(iris)
      df$mutate_if(is.numeric, log)
      df$data$Sepal.Length[1]
    },
    1.629241,
    tolerance = 5e-7)
})

test_that("mutate_at", {
  expect_equal({
    df <- hash_df$new(iris)
    df$mutate_at("Sepal*", log)
    df$data$Sepal.Length[1]
  },
  1.629241,
  tolerance = 5e-7)
})

test_that("mutate_all", {
  expect_equal({
    df <- hash_df$new(mtcars)
    df$mutate_all(~.x - mean(.x))
    df$data$am[1]},
    0.59375)
})

test_that("select", {
  expect_equal({
    df <- hash_df$new(iris)
    df$select(Sepal.Length, Sepal.Width)
    df$vars[1]},
    "Sepal.Width")
})

test_that("select_if", {
  expect_equal({
    df <- hash_df$new(iris)
    df$select_if(is.numeric)
    length(df$vars)},
    4L)
})

test_that("select_at", {
  expect_equal({
    df <- hash_df$new(iris)
    df$select_at("Sepal*")
    length(df$vars)},
    2L)
})

test_that("prime_iter", {
 expect_equal({
   .e1 <- new.env(parent = emptyenv())
   primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0, "Iterator")
   yield_next(primes)},
   2L)
 })

test_that("grepl_set", {
  expect_equal({
    c("I", "Don't", "wan't", "chicken") %>%
                               that_for_all("\'") %>%
                               we_have(~grepl(.y, .x))},
    c("Don't", "wan't"))
})
